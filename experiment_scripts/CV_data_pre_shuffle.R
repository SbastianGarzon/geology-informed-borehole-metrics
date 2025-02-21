#! /usr/bin/env Rscript

# Loading required libraries
library(caret)
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(tictoc)
library(zoo)
library(patchwork)
library(data.table)
library(ggh4x)
library(RColorBrewer)

# Timer for script
tic("Data Preparation")

# Get name from experiment from command argument
args <- commandArgs(trailingOnly=TRUE)
experiment_name <- toString(args[[1]])

# Loading Strata
install.packages("../stRata", repos = NULL, type = "source", dependencies = TRUE, ask = FALSE)
library(stRata)

## Define paths to data
# Path to basis data
basis_path = file.path("..","Data","basisdata","basisdata_gef_bovennaaronder_public_domain_RVG.csv")

# Path to DGM data
dgm_path = file.path("..","Data","DGMplus_extractie","DGMplus_extractie_DGM_v02r2_en_H3OdeKempen_NL_public_domain_RVG.csv")

# Path to Roer Valley Graben shapefile
rvg_path = file.path("..","Data","RVG_shapefile","RVG_OK.shp")

##### Load files ###

# Load Area of study (Roer Valley Graben)
rvg_shapefile <- st_read(rvg_path) %>% st_transform(crs= st_crs(28992))

# Load Basisdata (Lithological descriptions)
basisdata <- stRata::load_dataset(file_path = basis_path, crs_target =28992, area_of_interest=rvg_shapefile)

# Load dgm_extractie (Lithostratigraphic labels)
dgm_extractie <-  stRata::load_dataset(file_path = dgm_path, crs_target =28992,cords_names = c("X","Y"), area_of_interest=rvg_shapefile)

# Define stratigraphic order for plots

stratigraphic_order <- c(
  "AAOP","AAES","NUNA",
  "NA","NASC","NAZA","NAWA1","NAWA2","NAWO","NAWOBE",
  "EC","EC1","EC2",
  "NI","NIGR","NIHO","NIBA",
  "BX","BXKO","BXSI","BXDE","BXWI","BXLM","BXSC","BXBS",
  "BE","BEWY","BERO",
  "HT",
  "KR","KR1","KR2","KRWY",
  "KW",
  "WB",
  "EE",
  "DRGI",
  "DN",
  "UR",
  "UR1","UR2",
  "PE",
  "AP",
  "ST","SY","PZWA","MS","OO","KI","IE","BRVI","VE","RU","TO","DO","LA","HO","MT","VA","AK","DIEP","GE","NN",NA)

## DGM and Basisdata have different height references. Here we adjust values to NAP (Normaal Amsterdams Peil)

dgm_extractie <- stRata::dgm_to_nap(dgm_extractie)

# Merging datasets

basis_dgm_roer_combined <- stRata::merge_basis_dgm(basisdata = basisdata,dgm_extractie = dgm_extractie, stratigraphic_order = stratigraphic_order)

# Remove DIEP - VA - VE - RU -LA & HO units

basis_dgm_roer_combined <- basis_dgm_roer_combined %>% filter(!strat_inter%in%c("DIEP","VA","VE","RU","LA","HO"))

# Extract Unique borehole locations

unique_boreholes <- basis_dgm_roer_combined %>% group_by(nr) %>% slice(1) %>% st_as_sf(coords = c("x_28992","y_28992"),crs=st_crs(28992)) %>% select(nr)

#Simplify Classes 

# Load the CSV
strat_class_map_df <- fread("../Data/strat_class_map.csv")

basis_dgm_roer_combined$zmk_litho <- basis_dgm_roer_combined$zmk

# Convert back to a named vector
strat_class_map <- setNames(as.character(strat_class_map_df$value), strat_class_map_df$key)

# Preprocessing for ZML_ML feature

zmk_class_map <- c('ZUF'= 84,'ZUFO'= 84,'ZZF'= 127.5,
                   'ZZFO'= 127.5,'ZMF'= 180,'ZMFO'= 180,
                   'ZMG'= 255,'ZMGO'= 255,'ZZG'= 360,'ZZGO'= 360,
                   'ZUG'= 1210,'ZUGO'= 1210,'ZFC'= 136.5,'ZGC'= 1105,
                   'WZF'= 94,'WF'= 187.5,'WM'= 375,'WG'= 750,'WZG'= 1500,
                   'ZMO'= NA,'ZMC'= 225,"None"=NA)

basis_dgm_roer_combined <- stRata::simplify_classes(basis_dgm_roer_combined,"zmk",zmk_class_map)

basis_dgm_roer_combined$zmk <- as.numeric(basis_dgm_roer_combined$zmk)
basis_dgm_roer_combined$zm <- as.numeric(basis_dgm_roer_combined$zm)

basis_dgm_roer_combined$zmk_ml <- ifelse(is.na(basis_dgm_roer_combined$zm),basis_dgm_roer_combined$zmk,basis_dgm_roer_combined$zm)

basis_dgm_roer_combined$zmk_ml[basis_dgm_roer_combined$zmk_ml<63 | basis_dgm_roer_combined$zmk_ml>1999] <- NA 

basis_dgm_roer_combined <- stRata::simplify_classes(basis_dgm_roer_combined,"strat_inter",strat_class_map)

# Simplify Litho

lithology <- read.csv2("../Data/categorieen_hoofdlitho.csv")
lithology_class_map <- setNames(lithology$categorie,lithology$code)

basis_dgm_roer_combined$lith_plot <- basis_dgm_roer_combined$lith
basis_dgm_roer_combined <- stRata::simplify_classes(basis_dgm_roer_combined,"lith",lithology_class_map)

# Discrqetize the data every 0.5 metres
basis_dgm_roer_disc_0dot5 <- stRata::discretize_intervals(basis_dgm_roer_combined,interval = 0.5)

basis_dgm_roer_disc_0dot5[, org := as.numeric(org)]
basis_dgm_roer_disc_0dot5[, sh := as.numeric(sh)]

# Initial split (Labeled/Unlabeled)

df_lab <- basis_dgm_roer_disc_0dot5[!is.na(strat_inter)]

## For this experiment, we are not predicting in new boreholes, so there are no
# unlabeled boreholes
df_unlabeled <- basis_dgm_roer_disc_0dot5[is.na(strat_inter)]

# Shuffle dataset (Original dataset is ordered by NR (Borehole code), 
# so we shuffle by NR to get a random order)

# Define seed for reproducibility
set.seed(2013)

df_labeled <- df_lab %>%
  group_by(nr) %>%
  group_split() %>%
  sample() %>%
  bind_rows()

# For this experiment We are removing boreholes that include at least one interval
# with "NN" in between other valid stratigraphic units
# For Boreholes where NN values are present on top or bottom, the NN values are
# removed, but the other stratigraphic units are kept.

sequence_list <- lapply(unique(df_labeled$nr), function(borehole) {
  # Extract the sequence for each borehole
  borehole_data <- df_labeled %>%
    filter(nr == borehole) %>%
    pull(strat_inter)  # Assuming 'target_variable' is the sequence column
  seq <- unlist(borehole_data)
  x <- as.character(seq)
  rle_values <- rle(x)$values
  
  return(rle_values)
})

names(sequence_list) <- unique(df_labeled$nr)

check_nn_position <- function(sequence) {
  nn_positions <- which(sequence == "NN")  # Find positions of "NN"
  if (length(nn_positions) == 0) {
    # No "NN" in the sequence, valid
    return(TRUE)
  }

  valid <- all(nn_positions %in% c(1, length(sequence)))  # "NN" is at the top or bottom

  # "NN" is somewhere in the middle, not valid
  return(valid)
}

invalid_boreholes <- names(sequence_list)[!sapply(sequence_list, check_nn_position)]

invalid_count <- length(invalid_boreholes)
total_boreholes <- length(unique(df_labeled$nr))

cat("Removing ", invalid_count, " invalid boreholes out of ", total_boreholes, "\n", 
    "Invalid boreholes: ", paste(invalid_boreholes, collapse = ", "),
    " \n This boreholes have Not Interpreted intervals between valid units",
    "\n")

# Remove invalid boreholes

df_labeled <- df_labeled %>% filter(!nr %in% invalid_boreholes)

# Here I remove the "NN" values from boreholes only if they are at the top 
# or bottom of the borehole. This is done to ensure that the stratigraphic
# order is maintained and not affected by "NN" values. Similarly, the model
# is not going to learn from "NN" values, as this is not informative.

df_labeled <- df_labeled %>% filter(strat_inter !="NN")

# Stratified group sampling

k_fold <- stRata::stratified_group_sampling_k_fold(df_labeled,5)

# Extract known transitions
known_transitions <- extract_known_transitions(df_labeled)

df_labeled$strat_inter <- droplevels(df_labeled$strat_inter)

# Unlist the k_fold and fix names to reflect folds properly
fold_vector <- unlist(k_fold, use.names = TRUE)
names(fold_vector) <- rep(names(k_fold), lengths(k_fold))  # Ensure names are just the fold names

df_labeled$fold <- names(fold_vector)[match(df_labeled$nr, fold_vector)]

# Get colors for stratigraphic units
strat_colors_path <- file.path("..","Data","kleurcodes_strat.csv")
color_vector <- get_strat_colors(strat_colors_path,stratigraphic_order)

# Plot class distribution
plot_class_prop <- plot_class_proportion(df_labeled,color_vector,stratigraphic_order)

# Plot class distribution per fold
plot_class_by_folds <- plot_class_prop_per_fold(df_labeled, stratigraphic_order,color_vector)

# Create folder for experiment
dir.create(experiment_name)

# Save data and export plots
write.csv2(df_labeled,file.path(experiment_name,"df_labeled.csv"), row.names = FALSE)
ggsave(file.path(experiment_name,"plot_class_distribution.pdf"),plot_class_prop, width = 12, height = 8)
ggsave(file.path(experiment_name,"plot_class_proportion.pdf"),plot_class_by_folds, width=12,height = 8)
toc()


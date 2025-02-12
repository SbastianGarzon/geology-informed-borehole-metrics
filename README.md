# Assessment of automated stratigraphic interpretations of boreholes with geology-informed metrics

Sebastián Garzón <a itemprop="sameAs" content="https://orcid.org/0000-0002-8335-9312" href="https://orcid.org/0000-0002-8335-9312" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>, Willem Dabekaussen, Freek Busschers <a itemprop="sameAs" content="https://orcid.org/0000-0003-1495-1766" href="https://orcid.org/0000-0003-1495-1766" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>, Eva De Boever <a itemprop="sameAs" content="https://orcid.org/0000-0002-7514-4936" href="https://orcid.org/0000-0002-7514-4936" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>, Siamak Mehrkanoon <a itemprop="sameAs" content="https://orcid.org/0000-0002-0516-0391" href="https://orcid.org/0000-0002-0516-0391" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a> and Derek Karssenberg <a itemprop="sameAs" content="https://orcid.org/0000-0002-6475-363X" href="https://orcid.org/0000-0002-6475-363X" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>

This reposity contains the scripts required to reproduce the publication "Assessment of automated stratigraphic interpretations of boreholes with geology-informed metrics". 

# Manual

The repository contains two folders. The first one `stRata` is an R package with all functions required to process the data. The second folder, `experiment_scripts` contains the specific scripts to reproduce the experiments described in the publication.
Additionally to reproduce the results, you need to download the `Data` folder from Zenodo: [10.5281/zenodo.14859952](https://doi.org/10.5281/zenodo.14859952).

## Data preprocessing

Once you have all folders (`stRata`,`experiment_scripts`, and `Data`) you can start with the experiment (in this case "Experiment_2025"). The first script will preprocess the data and create a new `Experiment_2025` folder:

```
cd experiment_scripts
Rscript CV_data_pre_shuffle.R Experiment_2025
```

## Reproducing one experiment

Each experiment consists of an independent script (e.g. `NN_3_All.R`). For instance, to reproduce the cross-validation using the Neural Network (NN) arquitecture using all features (Set 3) you need to execute the following code:

```
Rscript NN_3_All.R Experiment_2025
```


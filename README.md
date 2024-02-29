# CS-PHOC

<!-- badges: start -->

<!-- [![DOI](https://zenodo.org/badge/514008683.svg)](https://zenodo.org/badge/latestdoi/514008683) -->
[![Dataset DOI](https://img.shields.io/badge/DOI-10.15468/d79fbe-blue)](https://doi.org/10.15468/d79fbe)

<!-- badges: end -->


This repository is an R project that contains the data, code, figures, and documentation relevant to the Cape Shirreff PHOcid Census (CS-PHOC) dataset and data paper. Repo contents and structure are described below.

Specific versions of the [manuscript](Woodman_etal_CS-PHOC.docx) can be found in [releases](https://github.com/us-amlr/cs-phoc/releases). 

This project uses [renv](https://github.com/rstudio/renv/) to manage the project environment. Users can clone this repo and run `renv::restore()` as described in the [renv docs](https://rstudio.github.io/renv/).

## Data

The manuscript describing this data is in review. We ask that you do not publish with this data until the manuscript has been published. If you are interested in this project or this data before this time, please contact [sam.woodman@noaa.gov](mailto:sam.woodman@noaa.gov).

If using the data, please cite as TODO. Interested parties can access the CS-PHOC dataset in two ways:

- (Recommended) Access Darwin Core compliant event and occurrence tables through SCAR Antarctic Biodiversity Portal (www.biodiversity.aq): in progress

- Download the header and count CSV files included in this repo and referenced in the manuscript: [cs-phoc-headers.csv](data/manuscript/cs-phoc-headers.csv) and [cs-phoc-counts.csv](data/manuscript/cs-phoc-hcounts.csv)

Examples of loading and using CS-PHOC data can be found in the [examples file](R/examples.R), as well as the code used to generate the figures for the manuscript (see below)

## Manuscript

The CS-PHOC data paper has been submitted, and is currently in review. Manuscript access and citation details will be updated here as they become available.

Code to generate figures from the manuscript can be found [here](R/manuscript_figures.R). 

## Repo structure

```
├── R                     
    ├── import_*.R      : code for importing files from 'data/pre' into the database
    ├── examples.R      : example code for using the CS-PHOC dataset
    ├── manuscript_*.R  : code for generating CS-PHOC dataset and manuscript figures
├── data                  
    ├── dwc             : Darwin Core compliant tables
    ├── manuscript      : CSV files; presented in the manuscript
    ├── pre             : historical data files; now imported into the database
├── figures             : figures and table; included in the manuscript
    ├── other           : exploratory plots
├── renv                : renv files for dependencies
├── Woodman_etal_CS-PHOC.docx : manuscript (data paper), without EndNote field codes
├── Woodman_etal_CS-PHOC_endnote.docx : manuscript (data paper), with EndNote formatting
```

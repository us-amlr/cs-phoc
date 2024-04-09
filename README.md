# CS-PHOC

<!-- badges: start -->

<!-- [![DOI](https://zenodo.org/badge/514008683.svg)](https://zenodo.org/badge/latestdoi/514008683) -->
[![Dataset DOI](https://img.shields.io/badge/DOI-10.48361/gklk1u-blue)](https://doi.org/10.48361/gklk1u)


<!-- badges: end -->


This repository is an R project that contains the data, code, figures, and documentation relevant to the Cape Shirreff PHOcid Census (CS-PHOC) dataset and data paper. Repo contents and structure are described below.

This project uses [renv](https://github.com/rstudio/renv/) to manage the project environment. Users can clone this repo and run `renv::restore()` as described in the [renv docs](https://rstudio.github.io/renv/).

## Data

The manuscript describing this data is in review. We ask that you do not publish using this data until the manuscript has been published. If you are interested in this project or this data before this time, please contact [sam.woodman@noaa.gov](mailto:sam.woodman@noaa.gov).

If using the data, please cite the [dataset](https://doi.org/10.48361/gklk1u), and/or the manuscript (in review, citation todo) as appropriate. Interested parties can access the CS-PHOC dataset in two ways:

- Access Darwin Core Archive event and occurrence tables published through the [SCAR Antarctic Biodiversity Portal](https://www.biodiversity.aq/) at https://doi.org/10.15468/d79fbe

- Download the events and counts CSV files included in this repo and described in the manuscript: [cs-phoc-events.csv](data/manuscript/cs-phoc-events.csv) and [cs-phoc-counts.csv](data/manuscript/cs-phoc-counts.csv)

Examples of loading and using CS-PHOC data can be found in the [R/examples. R](R/examples.R), as well as the [code](R/manuscript_figures.R) used to generate the figures for the manuscript.

## Manuscript

The CS-PHOC data paper has been submitted to Scientific Data, and is currently in review. Manuscript access and citation details will be updated here as they become available.
 
Code to generate figures from the manuscript can be found in [R/manuscript_figures.R](R/manuscript_figures.R).

Specific versions of the [manuscript](Woodman_etal_CS-PHOC.docx) can be found in [releases](https://github.com/us-amlr/cs-phoc/releases). 

## CS-PHOC repo structure

```
├── R                   : R code
    ├── data_to_dwca.R  : generating DwCA tables for publication at biodiversity.aq
    ├── import_*.R      : importing files from 'data/pre' into the AMLR Pinnipeds database
    ├── examples.R      : example code for using the CS-PHOC dataset
    ├── manuscript_*.R  : generating CS-PHOC dataset, and manuscript figures
    ├── smw_qc.R        : early and ad-hoc CS-PHOC census data qc and explorations
├── data                  
    ├── dwca            : Darwin Core Archive tables; presented in the manuscript
    ├── manuscript      : standardized CSV files; presented in the manuscript
    ├── pre             : historical data files; now imported into the database
├── figures             : figures and table; included in the manuscript
    ├── other           : exploratory plots
├── renv                : renv files for dependencies
├── Woodman_etal_CS-PHOC.docx         : manuscript (data paper), submitted to SciData
├── Woodman_etal_CS-PHOC_endnote.docx : manuscript (data paper), with EndNote formatting
```

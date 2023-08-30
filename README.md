# CS-PHOC

This repository is an R project that contains the data, code, figures, and documentation relevant to the Cape Shirreff PHOcid Census (CS-PHOC) dataset and data paper. Repo strucutre is described below.

Specific versions of the [manuscript](Woodman_etal_CS-PHOC.docx), such as those shared with co-authors or sent out for review, can be found in [releases](https://github.com/us-amlr/cs-phoc/releases). 

This project uses [renv](https://github.com/rstudio/renv/) to manage the project environment. Users can clone this repo and run `renv::restore()` as described in the renv docs.

## Data

Interested parties can access the CS-PHOC dataset in two ways:

- Access Darwin Core compliant event and occurrence tables through SCAR Antarctic Biodiversity Portal (recommended; www.biodiversity.aq): in progress

- Download the header and count CSV files included in this repo: [cs-phoc-headers.csv](data/manuscript/cs-phoc-headers.csv) and [cs-phoc-counts.csv](data/manuscript/cs-phoc-hcounts.csv)

As described in the manuscript, raw data stored in the ***REMOVED*** database can be requested through the authors.

If using the data, please cite as TODO

## Manuscript

The CS-PHOC data paper is currently in prep. Manuscript access and citation details will be added here.

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
├── Woodman_etal_CS-PHOC.docx : manuscript (data paper)
```

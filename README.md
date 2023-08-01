# CS-PHOC

This repository contains the data, code, and documentation for generating the Cape Shirreff PHOcid Census (CS-PHOC) dataset, as well as all code, figures, and other files for the CS-PHOC data paper. Specific versions of the manuscript, e.g. those shared with co-authors or sent out for review, can be found in [releases](https://github.com/us-amlr/cs-phoc/releases). 


Note that this repo/project uses [renv](https://github.com/rstudio/renv/) to document the project environment. Anyone using this project and wishing to use the same project environment can simply clone this repo and run `renv::restore()`, as described in the renv readme. This is not required, but could be helpful if you are running into errors when attempting to reproduce any part of the project.

Note: README-data.md is outdated and needs to be updated.

## Directories

* data-in: contains historical INACH and AMLR data stored in Excel files. These data were imported into the ***REMOVED*** database via import_inach.R and import_amlr.R.

* manuscript: all files associated with the CS-PHOC data paper: manuscript, data, figures, and tables.

* output: exploratory plots and other output associated with this project

* R: the R code for this project. The code in this folder includes importing INACH and US AMLR data to the ***REMOVED*** database (import_inach.R and import_amlr/R), preparing the final CS-PHOC dataset (manuscript_data.R), preparing figures for the CS-PHOC data paper (manuscript_map.R and manuscript_plots.R), and example code referenced int he manuscript (examples.R).

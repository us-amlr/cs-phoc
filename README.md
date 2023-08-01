# CS-PHOC

This repository contains the code and documentation for generating the Cape Shirreff PHOcid Census (CS-PHOC) dataset, as well as all code and figures for the CS-PHOC data paper. 

Note that this repo/project uses [renv](https://github.com/rstudio/renv/) to document the project environment. Anyone using this project and wishing to use the same project environment can simply clone this repo and run `renv::restore()`, as described in the renv readme. This is not required, but could be helpful if you are running into errors when attempting to reproduce any part of the project.

Note: README-data.md is outdated and needs to be updated.

## Directory Descriptions

* data: contains historical INACH and AMLR data stored in Excel files. These data were imported into the ***REMOVED*** database via import_inach.R and import_amlr.R.

* manuscript: a word document of the CS-PHOC data paper

* output: manuscript figures, exploratory plots, and other output associated with this project. Figures for the CS-PHOC data paper are in output/manuscript

* R: the R code for this project. The code in this folder inculdes importing INACH and US AMLR data to the ***REMOVED*** database (import_inach.R and import_amlr/R), preparing the final CS-PHOC dataset (manuscript_data.R), preparing figures for the CS-PHOC data paper (manuscript_map.R and manuscript_plots.R), and example code (examples.R)

# CS-PHOC

This repository is an R project that contains the data, code, and documentation for generating the Cape Shirreff PHOcid Census (CS-PHOC) dataset. It also contains all code, figures, and other files relevant to the CS-PHOC data paper: [Woodman_etal_CS-PHOC.docx](Woodman_etal_CS-PHOC.docx). 

Specific versions of the [manuscript](Woodman_etal_CS-PHOC.docx), such as those shared with co-authors or sent out for review, can be found in [releases](https://github.com/us-amlr/cs-phoc/releases). 

Note that this R project uses [renv](https://github.com/rstudio/renv/) to document the project environment. Anyone using this project and wishing to use the same project environment can simply clone this repo and run `renv::restore()`, as described in the [renv documentation](https://rstudio.github.io/renv/). This is not required, but could be helpful if you are running into errors when attempting to reproduce any part of the project.

Note: README-data.md is outdated and needs to be updated.

## Directories

* data: Data relevant to the CS-PHOC project
  * antabif: Data files to be published via https://ipt.biodiversity.aq/
  * manuscript: CSV files referenced in the CS-PHOC data paper publication. Created from data in the ***REMOVED*** database.
  * pre: historical INACH and AMLR data stored in Excel files. These data were imported into the ***REMOVED*** database via import_inach.R and import_amlr.R. 
  * Raw data stored in the ***REMOVED*** database can be requested through the authors.

* figures: Figures (and table) for the CS-PHOC data paper. Also contains exploratory plots (in 'other')

* R: The R code for this project
  * import_*.R files: Code for importing INACH and US AMLR data to the ***REMOVED*** database
  * manuscript_*.R files: Code for the CS-PHOC data paper. This includes preparing the final CS-PHOC dataset (manuscript_data.R), preparing figures for the CS-PHOC data paper (manuscript_figures.R), and example code referenced in the manuscript (manuscript_examples.R).

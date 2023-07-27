# CS-PHOC

This repository holds the code and documentation for generating the Cape Shirreff PHOcid Census (CS-PHOC) dataset, as well as code and figures for the CS-PHOC data paper. 

See the [readme-data](readme-data.md) markdown file for descriptions of the data.

## R Folder (scripts)

Todo: document and outline all

* phocid_census_amlr_import.R: Script used to import historical US AMLR phocid census data into the Pinnipeds database from Excel files. Requires access to the ***REMOVED*** database.

* phocid_census_inach.R: Script to read in, process, and combine raw INACH phocid census data. Writes concatenated INACH data to 'inach_data' folder.

* phocid_census_combine.R: Read INACH data from files and US AMLR data from database. Write US AMLR data to CSV files in the amlr_data folder. Combine INACH and US AMLR data into single data frame(s), and write to CSV files in 'cs_combined_data' folder.

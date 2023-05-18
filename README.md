# phocid-census-cs

An exploration of US AMLR and INACH phocid census data for Cape Shirreff. 

Current efforts are to combine these data sets, document differences between both seasons and data sets, and to prepare the data for a data paper, a la [Ropert-Coudert et al 2020](https://doi.org/10.1038/s41597-020-0406-x).

See the [readme-data](readme-data.md) markdown file for descriptions of the data.

## Scripts

* phocid_census_amlr_import.R: Script used to import historical US AMLR phocid census data into the Pinnipeds database from Excel files. Requires access to the ***REMOVED*** database.

* phocid_census_amlr_export.R: Extract phocid census data from the US AMLR Pinnipeds database and write it to 'amlr_data' folder. Requires access to the ***REMOVED*** database.

* phocid_census_inach.R: Script to read in, process, and combine raw INACH phocid census data. Writes concatenated INACH data to 'inach_data' folder.

* phocid_census_combine.R: Combine INACH and US AMLR data into single data frame(s), and write to CSV files in 'cs_combined_data' folder.

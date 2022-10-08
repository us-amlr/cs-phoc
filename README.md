# phocid-census-cs

An exploration of US AMLR and INACH phocid census data for Cape Shirreff. 

Current efforts are to combine these data sets, document differences between both seasons and data sets, and to prepare the data for a data paper, a la [Ropert-Coudert et al 2020](https://doi.org/10.1038/s41597-020-0406-x).

See todo for assumptions made for each US AMLR season.

## Scripts

* phocid_census_import_usamlr.R: Script used to import historical US AMLR phcoid census data into the Pinnipeds database from Excel files.

* phocid_census_export_usamlr.R: Extract phocid census data from the US AMLR Pinnipeds database and write it to Google Sheets and/or CSV files. Requires access to the ***REMOVED*** database.

* phocid_census_inach.R: Script to read in, process, and combine raw INACH phocid census data. Writes concatenated INACH data to 'inach_data/phocids_inach_cs.csv' and 'inach_data/phocids_inach_cs_header.csv'.

* phocid_census_combine.R: Combine INACH and US AMLR data into single data frame(s), and write to CSV files in 'cs_combined_out'.

---
output:
  html_document: default
  word_document: default
  pdf_document: default
---
# Cape Shirreff Combined Phocid Census Data

This document details the creation of the combined Cape Shirreff phocid census dataset ([phocids_cs_combined.csv](cs_combined_data/phocids_cs_combined.csv)) using data from the [INACH](https://www.inach.cl/inach/) and [US AMLR](https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center) programs. The INACH data span 1997/98 - 2006/07, and the US AMLR data span 2009/10 - present. Phocid census surveys for Cape Shirreff were not recorded during the 2007/08 and 2008/09 seasons.

The INACH data were extracted from historical INACH records by Renato Borras, and concatenated together using the [phocid_census_inach.R](phocid_census_inach.R) script. The US AMLR data were extracted from the AMLR program's Pinnipeds database using the [phocid_census_amlr_export.R](phocid_census_amlr_export.R) script. The combined data files were generated from the INACH and US AMLR datasets using the [phocid_census_combine.R](phocid_census_combine.R) script. 

## TODO

Confirm/ok assumptions/decisions that Sam made while combining the data:

* Aggregated all records up to 'full beach' level (i.e., location_group in AMLR data). This loses some granularity, but standardizes location names in the data
	* For instance: E/C/W/S records were aggregated to primary beach
* Removed census_id and observer columns from the final combined output CSV

General

* Should we add (or more likely move?) research_program to the header table, since there was never an instance where a header record spans data records from both programs? Sam votes yes.
* Should I change 'present' (e.g., 2009/10 to present) to '2021/22 season'?

INACH

* 2005/06: only a single entry for all of Cape Shirreff. Notes say "There was no per beach countings in the documents. Only total of the whole cape not including San Telmo". What beaches can we assume that these surveys include?
* Map Paso Ancho records to Media Luna? This is what happens in US AMLR data (via location_group).
* Remove records from 1 Feb, 8 Feb, and 16 Feb 2007 based on "I would not trust this week" notes?
* Confirm assumption that San Telmo was never surveyed in INACH data

US AMLR

* Notes about the glacier: what is the south end of Media Luna, by season?
	* Only 3 records have notes about surveying to the glacier
	* Sam remembers we normally surveyed this in 2016/17
* Any other beaches that might have year-specific ranges?
* Should 'Paulina-Aranda' actually be 'Paulina-delLobero'? Seems unlikely Remanso-Aranda were surveyed every time.

## Data Structure

The phocid census data is split into two tables: a 'census header' table and a 'census data' table'. Having the header table allows us to explicitly group census records from the same phocid census that occurred on multiple days, e.g., if the census effort was split across a Thursday and a Friday. Each record in the data table has a header record ID, which allows each data record to be joined with its corresponding header record.

[phocids_cs_combined_header.csv](cs_combined_data/phocids_cs_combined_header.csv) contains only the phocid census header data, without the census count records. The combined data is provided in two forms: wide (one record per date/location/species; [phocids_cs_combined_wide.csv](cs_combined_data/phocids_cs_combined_wide.csv)) and long (one record per count; [phocids_cs_combined_long.csv](cs_combined_data/phocids_cs_combined_long.csv)). Note that all of the NA counts have been removed in the long data.

### Data Columns

Column descriptions for combined Cape Shirreff phocid census header info and data. All values are recorded by the observer, unless otherwise indicated.

##### Header table

* **season_name**: season in which this census occurred; derived from census_date
* **census_date_start**, **census_date_end**: start and end dates of the phocid census survey
* **census_days**: a calculated value (census_date_end - census_date_start + 1). The number of days over which the census was performed
* **header_notes**: notes for the phocid census header record
* **header_id**: ID value for the header record. Note for INACH records these are concatenations of the year and week number, while for the US AMLR data they are integers incrementing from 1.

##### Data table, general:

* **season_name**: season in which this record was recorded; derived from census_date
* **census_date**: date on which the record was recorded
* **time_start**, **time_end**: corresponding survey start and end times. Depending on the season, these times may be for the specific beach, for an observers entire survey (1-3 hour window), or the entire survey day across observers.
* **location**: the location (beach) of the census record. Note that this may not be the raw location data recorded by the observer, e.g., a record of 'Cachorros East' would have been aggregated up to the main beach of 'Cachorros'. 
* **species** the phocid species
* **census_notes**: observer notes
* **research_program**: either 'INACH' or 'USAMLR'; the research program that was the source of the observation
* **orig_record**: for US AMLR records only; a boolean indicating if this was a recorded observation (TRUE) or an explicit zero record that was generated in [phocid_census_combine.R](phocid_census_combine.R) (FALSE). See TODO for more information
* **header_id**: the ID of the corresponding header record

##### Data table, wide:

* **ad_female_count**: count of adult females
* **ad_male_count**: count of adult males
* **ad_unk_count**: count of adults of unknown sex
* **juv_female_count**: count of juvenile females
* **juv_male_count**: count of juvenile males
* **juv_unk_count**: count of juveniles of unknown sex
* **pup_live_count**: count of live pups
* **pup_dead_count**: count dead pups
* **unk_female_count**: count of females of unknown age class. This count is usually used when an inexperienced observer isn't able to decide between a large juvenile or small adult
* **unk_male_count**: count of males of unknown age class. This count is usually used when an inexperienced observer isn't able to decide between a large juvenile or small adult
* **unk_unk_count**: count of seals where sex and age class is unknown. This count is usually used for elephant seals where the observer can't see if it is an adult female or juvenile male
* **total_count**: a calculated value; the sum of all of the count columns (ad_female_count to unk_unk_count)
* **total_count_nodead**: a calculated value; the sum of all of the count columns (ad_female_count to unk_unk_count) except for pup_dead_count

##### Data table, long:

* **age_class**: pinniped age class; values are one of Adult, Juvenile, Pup-dead, Pup-live, or Unknown
* **sex**: pinniped sex; values are one of F (female), M (male) or U (unknown)
* **count**: count recorded by observer

#### Data Column Use

INACH observers recorded data for the following counts: **ad_female_count**, **ad_male_count**, **ad_unk_count**, **juv_female_count**, **juv_male_count**, **juv_unk_count**, **unk_unk_count**, and **pup_live_count**. Which counts were recorded depends on the species and season. Counts are NA (or blank in the CSV file) if they were not recorded.

In the US AMLR data, the following columns were used in all seasons (2009/10 to present): **ad_female_count**, **ad_male_count**, **ad_unk_count**, **juv_female_count**, **juv_male_count**, **juv_unk_count**, and **pup_live_count**

Other counts were only recorded in certain seasons:
* **pup_dead_count**: used from 2012/13 to present
* **unk_female_count** and **unk_male_count**: used from 2017/18 to present
* **unk_unk_count**: used from 2014/15 to present

## INACH Data Decisions and Notes

These INACH data include records with explicit zeros for when a beach was surveyed and (one or more) phocid species were not observed. 

INACH data decisions:

* Records with location 'Nibaldo Bahamondes' were mapped to 'Peninsula Cerro Gajardo' to be consistent with US AMLR data.
* Records with location 'Punta Olivia' were merged with 'Alcazar' records.
* Records with location 'Rocas Yeco' were merged with 'Schiappacasse' records.
* Records with location 'Paso Ancho' were merged with 'Media Luna' records, based on the assumption that this is most likely were those seals originally hauled out.

INACH data notes:

* Punta San Telmo was not surveyed during any INACH surveys.
* The INACH data uses a single location, 'Copihue', rather than the 'Copi' and 'Hue' locations used in the US AMLR data. During the INACH surveys, the Copi and Hue beaches were considered a single location known as Copihue. 
* For 2005/06 there is only a single entry for all of Cape Shirreff. This record contains a note: "There was no per beach countings in the documents. Only total of the whole cape not including San Telmo".

## US AMLR Data Decisions and Notes

### Beaches

The raw US AMLR phocid census data are not explicit, meaning that field biologists did not enter '0' records if there were no phocids of a particular species on a beach. However, we can assume that if there is no record for a particular species on a 'regular survey beach', then that beach was surveyed and zero pinnipeds were observed. The following beaches are all assumed to be 'regular survey beaches', i.e., they were surveyed during every US AMLR phocid census survey: Media Luna, Punta Yuseff, Larga, Marko, Daniel, Modulo, Hue, Copi, Maderas, Cachorros, Chungungo, Ballena Sur, Ballena Norte, Bahamonde, Nibaldo, Roquerio, Alcazar, Pinochet de la Barra, Papua, Antarctico, Loberia, Yamana, Golondrina, del Lobero, Paulina, Schiappacasse, El Plastico, Leopard, del Canal. 

NOTE: The west coast beaches south of Yamana, Golondrina to del Canal, were not recorded individually in the database until the 2018/19 season. Prior to this, these beaches were grouped into two 'blocks': Golondrina-del Lobero (Golondrina, del Lobero) and Paulina-Aranda (Paulina, Schiappacasse, El Plastico, Leopard, del Canal). 

### Adding Explicit Zero Records

In the [phocid_census_combine.R](phocid_census_combine.R) script, the [complete](https://tidyr.tidyverse.org/reference/complete.html) function was used to add records with count values of zero as needed to the US AMLR data for 'regular survey beaches' before generating the combined dataset. The AMLR data were split into four parts for this effort:

1) Explicit zero records were generated as needed for locations 'Golondrina-del Lobero' and 'Paulina-Aranda' for all census header records in the AMLR database from the 2009/10 season through the 2017/18 season. 

2) Explicit zero records were generated as needed for granular west coast locations (Golondrina, del Lobero, Paulina, Schiappacasse, El Plastico, Leopard, del Canal) for all census header records in the AMLR database from the 2018/19 season through the present. It was assumed that these locations were always surveyed on the same day because of their proximity.

3) Explicit zero records were generated for all other 'regular survey beaches' (Media Luna, Punta Yuseff, Larga, Marko, Daniel, Modulo, Hue, Copi, Maderas, Cachorros, Chungungo, Ballena Sur, Ballena Norte, Bahamonde, Nibaldo, Roquerio, Alcazar, Pinochet de la Barra, Papua, Antarctico, Loberia, Yamana) for all census header records in the AMLR database (2009/10 to present). 

4) Explicit zero records were generated for Punta San Telmo for all census header records in the AMLR database (2009/10 to present) where the header flag 'surveyed_san_telmo' was recorded as TRUE. 

Thus, the AMLR portion of the [combined data](cs_combined_data/phocids_cs_combined.csv) was the concatenation of the output of these four parts, as well as the remaining records from 'non-regular survey beaches'.

The following sections detail assumptions were made when generating these explicit zero records.

#### Dates: 

* For all four parts, if a census record existed for a given header/location pair, that date was used for all other records at that location (e.g., if there was only a record for one species for a given location).

* For parts 1 and 2, it was assumed that these locations (or location aggregations) were always surveyed on the same day because of their proximity whenever census date information was available for any beach. Thus, if there were any data records in these parts for a given header record, the other data records were assigned the same date.

* For parts 3 and 4 if there were no data records for a particular location for a given header, and parts 1 and 2 when there were no data records at all for a given header record, then the explicit zero record was assigned the census_date_end value (the last day of that census) from the corresponding header record. This decision was consistent with the decision made when [importing historical US AMLR records](phocid_census_amlr_import.R) from Excel files.

#### Times: 

The following assumptions on assigning times for explicit zero records apply to all four parts:

* For each explicit zero record, if there was a data record with a recorded start/end time for the same date/location, then the explicit zero record was assigned that start/end time. If there were multiple data records for the same date/location (e.g., records from multiple observers), then the data record with the earliest start time was used.

* For each explicit zero record, if there was no data record with a recorded start/end time for the same date/location, then the minimum start time and maximum end time was determined when considering all data records for that day. The explicit zero record was assigned these start/end times.

#### Counts:

Explicit zero records were assigned zero or NA values in a manner that was consistent with the count column used described in [Data Column Use](#data-column-use). For instance, **ad_female_count** values were always zero, while **unk_unk_count** values were NA or zero if the record date was before or after (inclusive) the 2014/15 season.

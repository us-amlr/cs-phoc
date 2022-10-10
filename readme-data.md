# Cape Shirreff Phocid Census Data

## TODO

General
* Make combined data long

INACH
* 2005/06: only a single entry for all of Cape Shirreff. Notes say "There was no per beach countings in the documents. Only total of the whole cape not including San Telmo". What beaches does this include?
* Map Paso Ancho records to Media Luna? This is what happens in US AMLR location_group.
* Remove records from 1 Feb, 8 Feb, and 16 Feb 2007 based on "I would not trust this week" notes?
* Confirm assumption that San Telmo was never surveyed in INACH data

US AMLR
* Make US AMLR data explicit in terms of 0s
	* Need to confirm it's ok to aggregate up to location groups
	* Need to decide how to fill in time values for 'explicit 0' records
* Notes about the glacier: what is the south end of Media Luna, by season?
	* 3 records have notes about surveying to the glacier
	* I remember we normally surveyed this in 2016/17
* Any other beaches that might have year-specific ranges?
* Should 'Paulina-Aranda' actually be 'Paulina-delLobero'? Seems unlikely Remanso-Aranda were surveyed every time.

## Combined

Notes about combined Cape Shirreff phocid census data ([phocids_cs_combined.csv](cs_combined_data/phocids_cs_combined.csv)) from the [INACH](https://www.inach.cl/inach/) and [US AMLR](https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center) programs. The INACH data span 1997/98 - 2006/07, and the US AMLR data span 2009/10 - present. Phocid census surveys for Cape Shirreff were not recorded during the 2007/08 and 2008/09 seasons.

### Data Structure

The phocid census data is split into two tables: a 'census header' table and a 'census data' table'. Each record in the data table has a header record ID, which allows each data record to be joined with its corresponding header record. Having the header table allows for two important things: 

1) explicitly group census records from the same phocid census that occurred on multiple days, e.g., if the census effort was split across a Thursday and a Friday. 
2) explicitly indicate if Punta San Telmo was surveyed to differentiate between 'there are no San Telmo records because no one surveyed it' and 'there are no San Telmo records because there were no phocids'.

[phocids_cs_combined_header.csv](cs_combined_data/phocids_cs_combined_header.csv) contains only the phocid census header data, without the census count records.

### Data Columns

Column descriptions for Cape Shirreff phocid census data. 

* **season_name**: season in which this census occurred; derived from census_date
* **census_date_start**, **census_date_end**: from census_phocid_header; start and end dates of phocid census survey
* **surveyed_san_telmo**: from census_phocid_header; true/false indicating if Punta San Telmo was surveyed
* **observer**: observer that made the observation, if known
* **census_date**
* **time_start**, **time_end**: corresponding survey start and end times. Depending on the season, these times may be for the specific beach, from a weather record (1-3 hour window), or the entire survey day.
* **location**: the location (beach) recorded by the observer
* **location_group**: In past years, observers had the option of recording different entries for, e.g., Cachorros East and Cachorros. The 'location group' system aggregates these records up to the main beach. For instance, if the location for a census record was 'Cachorros East', the location_group would be 'Cachorros'.
* **species**
* **total_count**: A derived value; the sum of all of the _count columns (ad_female_count to unk_unk_count).
* **total_count_nodead**: A derived value; the sum of all of the _count columns (ad_female_count to unk_unk_count) except for pup_dead_count.
* **ad_female_count**: count recorded by observer, adult female
* **ad_male_count**: count recorded by observer, adult male
* **ad_unk_count**: count recorded by observer, adult of unknown sex
* **juv_female_count**: count recorded by observer, juvenile female
* **juv_male_count**: count recorded by observer, juvenile male
* **juv_unk_count**: count recorded by observer, juvenile of unknown sex
* **pup_live_count**: count recorded by observer, live pup
* **pup_dead_count**: count recorded by observer, dead pup
* **unk_female_count**: count recorded by observer, female of unknown age class
* **unk_male_count**: count recorded by observer, male of unknown age class
* **unk_unk_count**: count recorded by observer, seal where sex and age class is unknown (usually used for elephant seals where observer can't see if it is an adult female or juvenile male)
* **header_notes**: observer notes from census_phocid_header table
* **census_notes**: observer notes from census table
* **census_phocid_header_id**: census_phocid_header ID value for particular record
* **census_id**: census ID value for particular record

TODO: determine which of these columns to keep for data to be published

## INACH Phocid Census Data

Notes specific to INACH Cape Shirreff Phocid Census data ([phocids_cs_inach.csv](inach_data/phocids_cs_inach.csv)).

### Description

These data were exctracted from historical INACH records by Renato Borras. These INACH data include records with explicit 0s for when a beach was surveyed and no phocids (of a certain species) were observed. 

### Beaches

Note that in the INACH data, 

* Records with 'Nibaldo Bahamondes' were mapped to 'Peninsula Cerro Gajardo' to be consistent with US AMLR data.
* Records with location 'Punta Olivia' were merged with 'Alcazar' records.
* Records with location 'Rocas Yeco' were merged with 'Schiappacasse' records.

* 2005/06: only a single entry for all of Cape Shirreff. Notes say "There was no per beach countings in the documents. Only total of the whole cape not including San Telmo".
* Make note about Copihue: combo of Copi and Hue beahces

## US AMLR Phocid Census Data

Notes specific to US AMLR Cape Shirreff Phocid Census data ([phocids_cs_amlr.csv](amlr_data/phocids_cs_amlr.csv)).

### Beaches

The raw US AMLR phocid census data are not explicit, meaning that field biologists do not enter '0' records if there are no phocids of a particular species on a beach. However, you can assume that if there is no record for a particular species on a '[regular survey beach](#regular-survey-beaches)', then that beach was surveyed and zero pinnipeds were observed.

Phocid census records for all other beaches (other than Punta San Telmo - see [Data Structure](#data-structure) should be considered 'opportunistic', and thus the lack of a census record should NOT be interpreted as no phocids.

Note that (TODO) the explicit zero records have been added in the 'combined' data.

### 'Regular Survey Beaches'

The following beaches are all assumed to have been surveyed during every US AMLR phocid census survey:

Media Luna, Punta Yuseff, Larga, Marko, Daniel, Modulo, Hue, Copi, Maderas, Cachorros, Chungungo, Ballena Sur, Ballena Norte, Bahamonde, Nibaldo, Roquerio, Alcazar, Pinochet de la Barra, Papua, Antarctico, Loberia, Yamana, Golondrina, del Lobero, Paulina, Schiappacasse, El Plastico, Leopard, del Canal.

NOTE: The west coast south of Yamana (Golondrina to del Canal) was not recorded individually until the 2018/19 season. Prior to this, these beaches were grouped into two 'blocks': Golondrina-del Lobero (Golondrina, del Lobero) and Paulina-Aranda (Paulina, Schiappacasse, El Plastico, Leopard, del Canal). See TODO below.

### Data Column Use By Season

This section details which counts were recorded (i.e., which columns were used) in which seasons of US AMLR data.

**ad_female_count**, **ad_male_count**, **ad_unk_count**, **juv_female_count**, **juv_male_count**, **juv_unk_count**, **pup_live_count**: used in all seasons (2009/10 to present)

**pup_dead_count**: used from 2012/13 to present

**unk_female_count** and **unk_male_count**: used from 2017/18 to present

**unk_unk_count**: used from 2014/15 to present

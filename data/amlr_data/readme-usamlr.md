# US AMLR Phocid Census Data - Raw

The 'Weekly Phocid Census YYYY-YY' files are the original files from the indicated field seasons. They were imported into the US AMLR Pinnipeds database using the [phocid_census_amlr_import](R/phocid_census_amlr_import.R) script.

# US AMLR Phocid Census Data - Processed

Notes about US AMLR Cape Shirreff Phocid Census data ([phocid_census_us_amlr_header.csv](data/amlr_data/phocid_census_us_amlr_header.csv) and [phocid_census_us_amlr.csv](data/amlr_data/phocid_census_us_amlr.csv)). These data span 2009/10 - present. These files were created in the [phocids_census_combine.R](R/phocids_census_combine.R) script.

## Database Structure

The phocid census data in the US AMLR Pinnipeds database is split into two tables: census_phocid_header and census. Each phocid census record in the census table has a census_phocid_header record ID, which allows each record to be joined with its corresponding census_phocid_header record. Having the header table allows us to two important things: 

1) explicitly group census records from the same phocid census that occurred on different days, e.g., if the census effort was split across a Thursday and a Friday. 
2) explicitly indicate if Punta San Telmo was surveyed to differentiate between 'there are no San Telmo records because no one surveyed it' and 'there are no San Telmo records because there were no phocids'.

[phocid_census_us_amlr_header.csv](data/amlr_data/phocid_census_us_amlr_header.csv) contains only the US AMLR phocid census header data, without the census count records.

## Data import

The AMLR phocid census data had not been imported from Excel files for the 2009/10, 2010/11, and 2011/12 seasons. The [phocids_census_amlr_import.R](R/phocids_census_amlr_import.R) script reads in these data from the raw Excel files in the '15-Weekly Phocid census' folder, cleans and processes them, and imports them into the database

## Beaches

The raw US AMLR phocid census data are not explicit, meaning that field biologists do not enter '0' records if there are no phocids of a particular species on a beach. However, you can assume that if there is no record for a particular species on a '[regular survey beach](#regular-survey-beaches)', then that beach was surveyed and zero pinnipeds were observed.

Phocid census records for all other beaches (other than Punta San Telmo - see [Database Structure](#database-structure) should be considered 'opportunistic', and thus the lack of a census record should NOT be interpreted as no phocids.

### 'Regular Survey Beaches'

The following beaches are all assumed to have been surveyed during every US AMLR phocid census survey:

Media Luna, Punta Yuseff, Larga, Marko, Daniel, Modulo, Hue, Copi, Maderas, Cachorros, Chungungo, Ballena Sur, Ballena Norte, Bahamonde, Nibaldo, Roquerio, Alcazar, Pinochet de la Barra, Papua, Antarctico, Loberia, Yamana, Golondrina, del Lobero, Paulina, Schiappacasse, El Plastico, Leopard, del Canal.

NOTE: The west coast south of Yamana (Golondrina to del Canal) was not recorded individually until the 2018/19 season. Prior to this, these beaches were grouped into two 'blocks': Golondrina-del Lobero (Golondrina, del Lobero) and Paulina-Aranda (Paulina, Schiappacasse, El Plastico, Leopard, del Canal).
TODO: should this be 'Paulina-delLobero'? Seems unlikely Aranda was surveyed every time

## Data Columns

Column descriptions for ([phocid_census_us_amlr.csv](data/amlr_data/phocid_census_us_amlr.csv). See [Column Use By Season](#column-use-by-season) for a list of which count columns were used in which years.

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

### Column Use By Season

This section details which counts were recorded (i.e., which columns were used) in which seasons of US AMLR data.

**ad_female_count**, **ad_male_count**, **ad_unk_count**, **juv_female_count**, **juv_male_count**, **juv_unk_count**, **pup_live_count**: used in all seasons (2009/10 to present)

**pup_dead_count**: used from 2012/13 to present

**unk_female_count** and **unk_male_count**: used from 2017/18 to present

**unk_unk_count**: used from 2014/15 to present

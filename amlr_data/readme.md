# Phocid Census Data 

## Description

The phocid census data in the US AMLR Pinnipeds database is split into two tables: census_phocid_header and census. Each phocid census record in the census table has a census_phocid_header record ID, which allows each record to be joined with its corresponding census_phocid_header record. Having the header table allows us to two important things: 

1) explicitly group census records from the same phocid census that occurred on different days, e.g., if the census effort was split across a Thursday and a Friday. 
2) explicitly indicate if Punta San Telmo was surveyed so we can differentiate between 'there are no San Telmo records because no one surveyed it' and 'there are no San Telmo record because there were no phocids'.

The other important note about the US AMLR phocid census data is that the data is not explicit, meaning that field biologists do not enter '0' records if there are no phocids of a particular species on a beach. However, you can assume that if there is no record for a particular species on a 'regular survey beach', then that beach was surveyed and zero pinnipeds were observed. 'Regular survey beaches' consist of: Media Luna through Marko to Ballena Norte, Bahamonde to Antartico, and Loberia to del Canal (see complete list below). Phocid census records for all other beaches (other than Punta San Telmo - see above) should be considered 'opportunistic', and thus the lack of a census record should not be interpreted as no phocids.

The US AMLR phocid census data are in this folder: the header data (phocid_census_header_us_amlr.csv) and the phocid census data joined to the header data (phocid_census_us_amlr.csv). 

## Data Columns

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

'Regular survey beaches' in the data: Alcazar; Antartico, Bahamonde; Ballena Norte; Ballena Sur; Cachorros; Chungungo; Copi; Daniel; del Canal, Playa; del Lobero, Playa; El Plastico, Playa; Golondrina-del Lobero; Golondrina, Playa; Hue; Larga; Leopard Beach; Loberia; Maderas; Marko; Media Luna; Modulo; Nibaldo; Papua; Paulina-Aranda; Paulina, Playa; Pinochet de la Barra; Roquerio; Schiappacasse, Playa; Yamana, Playa; Yuseff, Punta

## Other

CSV files in this directory have been downloaded from <https://docs.google.com/spreadsheets/d/1bzjN_uUcJxc7o-CnPEa4U69QJWVlDpCB92oteaGM_Ec>


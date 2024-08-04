# CS-PHOC CSV files

The CSV files used to generate the Darwin Core Archive (DwCA) data in these two CSV files can be joined using the ‘event_id’ key present in both files. All key values are character strings that represent a unique CS-PHOC survey window (i.e., an ‘event’).

Users should note that because of the Darwin Core (DwC) standard, the ID keys used in the CSV files (event_id and count_id) are slightly different from the ID keys in the DwC Archive files (eventID and occurrenceID). See the [repo readme](https://github.com/us-amlr/cs-phoc) for a detailed discussion of these differences, as well as how to merge across these formats if desired.

## Events: cs-phoc-events.csv

The high-level information for each census.

-   event_id: The unique identifier for event records, can be used to join count and event records

-   season_name: A character string of the field season name (e.g., "2016/17")

-   census_start_date: The date of the beginning of the census

-   census_end_date: The date the census was completed

-   census_days: The number of days of the census, i.e. census_end_date - census_start_date

-   surveyed_pst: A boolean flag indicating whether or not the Punta San Telmo (PST) location was surveyed during that census

-   research_program: A character string indicating the research program that conducted the census (‘INACH’ or ‘USAMLR’)

## Counts: cs-phoc-counts.csv

The census counts. All count data in this file are explicit, meaning that each record has a numeric value if and only if a count of that species/count type was conducted, and a blank (i.e., NA) if no data were recorded.

-   count_id: character; A unique identifier for each count record

-   event_id: character; The unique identifier for event records, can be used to join count and event records

-   location: character; The Cape Shirreff location

-   species: character; The scientific name of the phocid species

-   species_common: character; The common name of the phocid species

-   total_count: integer; The sum of all of the other '\_count' columns, i.e., the total count for the corresponding census/location/species

-   ad_female_count: integer; Aggregate count of adult females (for the corresponding census/location/species)

-   ad_male_count: integer; Aggregate count of adult males

-   ad_unk_count: integer; Aggregate count of adults of unknown sex

-   juv_female_count: integer; Aggregate count of juvenile females

-   juv_male_count: integer; Aggregate count of juvenile males

-   juv_unk_count: integer; Aggregate count of juveniles of unknown sex

-   pup_count: integer; Aggregate count of pups (young of the year, less than one year old) of any sex

- unk_female_count: integer; Aggregate count of unknown age class females

- unk_male_count: integer; Aggregate count of unknown age class males

- unk_unk_count: integer; Aggregate count of phocids of unknown age class and unknown sex

# Convert CS-PHOC dataset to Darwin Core-compliant tables

library(dplyr)
library(readr)
library(tidyr)
library(worrms)
library(here)


# Read data---------------------------------------------------------------------
x.header <- read.csv(here("data", "manuscript", "cs-phoc-headers.csv"))
x.count <- read.csv(here("data", "manuscript", "cs-phoc-counts.csv"))


# Prep WORRMS names-------------------------------------------------------------
# match taxa using the list of unique scientific names
matched_taxa_tibbles <- wm_records_names(unique(x.count$species))

# bind the list of tibbles returned into 1 tibble, and prep to join
matched_taxa <- bind_rows(matched_taxa_tibbles) %>% 
  rename(scientificName = "scientificname") %>%
  select(scientificName, lsid, rank, kingdom)


## Create Event table-----------------------------------------------------------
event <- x.header %>%
  mutate(
    # eventDate is mandatory
    eventDate = if_else(census_date_start == census_date_end, census_date_start, 
                        paste(census_date_start, census_date_end, sep = "/")),  
    # whatever that cannot be mapped to Darwin Core terms goes to dynamicProperties
    dynamicProperties = sprintf(
      '{"research_program": "%s", "surveyed_san_telmo": %s}', 
      research_program, ifelse(surveyed_san_telmo, "true", "false")
    ),
    # add recommended Darwin Core terms: https://dwc.tdwg.org/terms/#event
    decimalLongitude = "-60.77",  # mandatory
    decimalLatitude = "-62.47",  # mandatory
    # coordinatePrecision = "",  # coordinateUncertaintyInMeters is more important, you can skip this if you don't have it
    coordinateUncertaintyInMeters = "",  # strongly recommended
    locality = "Cape Shirreff, Livingston Island",  
    higherGeography = "South Shetland Islands",
    higherGeographyID = "https://data.aad.gov.au/aadc/gaz/scar/display_name.cfm?gaz_id=131551", 
    countryCode = "AQ",
    sampleSizeValue = census_days,
    sampleSizeUnit = ifelse(census_days <= 1, "day", "days"),
    geodeticDatum = "EPSG:4326",
    samplingProtocol = "TODO: paper DOI upon publication"
  ) %>%
  rename(eventID = header_id) %>%
  # fields that cannot be mapped to Darwin Core
  select(-c(season_name, census_days, census_date_start, census_date_end, 
            surveyed_san_telmo, research_program))

# write to file
write_tsv(event, here("data", "dwc", "event.txt"), na = "")


## Create Occurrence table------------------------------------------------------
occ <- x.count %>% 
  rename(scientificName = species) %>% 
  left_join(matched_taxa, by = "scientificName") %>%
  # rename columns to Darwin Core terms
  rename(
    eventID = header_id,
    occurrenceID = count_id, 
    vernacularName = species_common,
    scientificNameID = lsid,
    taxonRank = rank
  ) %>%
  # add recommended Darwin Core terms: https://dwc.tdwg.org/terms/#occurrence
  mutate(
    basisOfRecord = "HumanObservation",
    identificationReferences = "https://doi.org/10.1016/C2012-0-06919-0"
  )

# create the long table which collapses sex and lifeStage to generate the Occurrence table for Darwin Core Archive: https://rs.gbif.org/core/dwc_occurrence_2022-02-02.xml 
occ.long <- occ %>%
  select(-total_count) %>% 
  rename(pup_unk_count = pup_count) %>% 
  pivot_longer(cols = ends_with("_count"), 
               names_to = c("lifeStage", "sex", "extra_count"), names_sep = "_", 
               values_to = "individualCount") %>%
  select(-extra_count) %>% 
  filter(!is.na(individualCount)) %>% 
  mutate(lifeStage = case_when(
    lifeStage == "ad" ~ "adult",
    lifeStage == "juv" ~ "juvenile",
    lifeStage == "pup" ~ "pup",
    lifeStage == "unk" ~ "unknown",
  ),
  sex = case_when(
    grepl("female", sex) ~ "female",
    grepl("male", sex) ~ "male",
    grepl("unk", sex) ~ "unknown"
  )) %>%
  mutate(occurrenceStatus = if_else(individualCount == 0, "absent", "present")
         # occurrenceStatus = case_when(
         #   individualCount == 0 ~ "absent",
         #   # is.na(individualCount) ~ NA_character_, # to confirm with Ming
         #   .default =  ~ "present"),
  ) %>%
  left_join(select(event, eventID, dateIdentified = eventDate), 
            by = "eventID")

# # Sanity checks
# occ.long %>% 
#   select(-c(eventID, occurrenceID, individualCount, dateIdentified)) %>% 
#   lapply(table, useNA = "ifany")
# table(occ.long$individualCount, occ.long$occurrenceStatus, useNA = "ifany")


# write to file
write_tsv(occ.long, here("data", "dwc", "occurrence.txt"), na = "")

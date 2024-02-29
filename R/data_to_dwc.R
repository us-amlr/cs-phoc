# Convert CS-PHOC dataset to Darwin Core compliant records

library(dplyr)
library(readr)
library(tidyr)
library(worrms)
library(here)


# Read CSV data-----------------------------------------------------------------
x.events <- read.csv(here("data", "manuscript", "cs-phoc-events.csv"))
x.counts <- read.csv(here("data", "manuscript", "cs-phoc-counts.csv"))


# Prep WORRMS names-------------------------------------------------------------
# match taxa using the list of unique scientific names
matched_taxa_tibbles <- wm_records_names(unique(x.counts$species))

# bind the list of tibbles returned into 1 tibble, and prep to join
matched_taxa <- bind_rows(matched_taxa_tibbles) %>% 
  rename(scientificName = "scientificname") %>%
  select(scientificName, lsid, rank, kingdom)


## Create Event table-----------------------------------------------------------
event <- x.events %>%
  mutate(
    # eventDate is mandatory
    eventDate = if_else(census_date_start == census_date_end, census_date_start, 
                        paste(census_date_start, census_date_end, sep = "/")),  
    # whatever that cannot be mapped to Darwin Core terms goes to dynamicProperties
    dynamicProperties = sprintf(
      '{"research_program": "%s", "surveyed_pst": %s}', 
      research_program, if_else(surveyed_pst, "true", "false")
    ),
    # add recommended Darwin Core terms: https://dwc.tdwg.org/terms/#event
    decimalLongitude = "-60.77",
    decimalLatitude = "-62.47",
    coordinateUncertaintyInMeters = "2650",
    locality = "Cape Shirreff, Livingston Island",  
    higherGeography = "Antarctica | South Shetland Islands | Cape Shirreff, Livingston Island",
    higherGeographyID = "https://data.aad.gov.au/aadc/gaz/scar/display_name.cfm?gaz_id=131551", 
    continent = "Antarctica",
    countryCode = "AQ",
    sampleSizeValue = census_days,
    sampleSizeUnit = ifelse(census_days <= 1, "day", "days"),
    geodeticDatum = "EPSG:4326",
    samplingProtocol = "CS-PHOC project"
  ) %>%
  rename(eventID = event_id) %>%
  # fields that cannot be mapped to Darwin Core
  select(-c(season_name, census_days, census_date_start, census_date_end, 
            surveyed_pst, research_program))

stopifnot(
  !any(is.na(event)), 
  sum(duplicated(event$eventID)) == 0
)

# write to file
write_tsv(event, here("data", "dwca", "event.txt"), na = "")


## Create Occurrence table------------------------------------------------------
occ <- x.counts %>% 
  rename(scientificName = species) %>% 
  left_join(matched_taxa, by = "scientificName") %>%
  # rename columns to Darwin Core terms
  rename(
    eventID = event_id,
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

# create the long table
occ.long <- occ %>%
  select(-total_count) %>% 
  rename(pup_unk_count = pup_count) %>% 
  pivot_longer(cols = ends_with("_count"), 
               names_to = c("lifeStage", "sex", "extra_count"), names_sep = "_", 
               values_to = "individualCount") %>%
  select(-extra_count) %>% 
  filter(!is.na(individualCount)) %>% 
  mutate(occurrenceStatus = if_else(individualCount == 0, "absent", "present"), 
         lifeStage_id = case_when(
           lifeStage == "ad" ~ 1,
           lifeStage == "juv" ~ 2,
           lifeStage == "pup" ~ 3,
           lifeStage == "unk" ~ 4,
         ),
         lifeStage = case_when(
           lifeStage == "ad" ~ "adult",
           lifeStage == "juv" ~ "juvenile",
           lifeStage == "pup" ~ "pup",
           lifeStage == "unk" ~ "unknown",
         ),
         sex_id = case_when(
           grepl("female", sex) ~ 1,
           grepl("male", sex) ~ 2,
           grepl("unk", sex) ~ 3
         ),
         sex = case_when(
           grepl("female", sex) ~ "female",
           grepl("male", sex) ~ "male",
           grepl("unk", sex) ~ "unknown"
         ), 
         occurrenceID = paste(occurrenceID, lifeStage_id, sex_id, sep = "-")) %>%
  left_join(select(event, eventID, dateIdentified = eventDate), 
            by = "eventID") %>% 
  select(-c(lifeStage_id, sex_id)) %>% 
  relocate(occurrenceID, .before = eventID)

# # Visual sanity checks
# occ.long %>%
#   select(-c(eventID, occurrenceID, individualCount, dateIdentified)) %>%
#   lapply(table, useNA = "ifany")
# table(occ.long$individualCount, occ.long$occurrenceStatus, useNA = "ifany")

stopifnot(
  !any(is.na(occ.long)), 
  sum(duplicated(occ.long$occurrenceID)) == 0
)

# write to file
write_tsv(occ.long, here("data", "dwca", "occurrence.txt"), na = "")

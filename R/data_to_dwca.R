# Convert CS-PHOC event/count data to Darwin Core Archive records

library(dplyr)
library(readr)
library(tidyr)
library(worrms)
library(here)


# Read CSV data-----------------------------------------------------------------
x.events <- read.csv(here("data", "manuscript", "cs-phoc-events.csv"))
x.counts <- read.csv(here("data", "manuscript", "cs-phoc-counts.csv"))


# Prep WoRMS names--------------------------------------------------------------
# match taxa using the list of unique scientific names
matched_taxa_tibbles <- wm_records_names(unique(x.counts$species))

# bind the list of tibbles returned into 1 tibble, and prep to join
matched_taxa <- bind_rows(matched_taxa_tibbles) %>% 
  rename(scientificName = "scientificname") %>%
  select(scientificName, lsid, rank, kingdom)


## Create Event table-----------------------------------------------------------
# Split Core and PST locations into their own events with their own IDs
event1 <- x.events %>%
  rename(eventID = event_id) %>% 
  mutate(eventID = paste0(eventID, "-1"), 
         decimalLongitude = "-60.77",
         decimalLatitude = "-62.47",
         locality = "Cape Shirreff, Livingston Island",
         locationRemarks = "CS-PHOC core census locations, on Cape Shirreff")

event2 <- x.events %>%
  rename(eventID = event_id) %>% 
  filter(surveyed_pst) %>% 
  mutate(eventID = paste0(eventID, "-2"), 
         decimalLongitude = "-60.808",
         decimalLatitude = "-62.4835",
         locality = "Punta San Telmo, Cape Shirreff, Livingston Island",
         locationRemarks = "CS-PHOC location Punta San Telmo, on Cape Shirreff")

# Bind Core and PST rows together, and add all other DwC info
event <- bind_rows(event1, event2) %>%
  mutate(eventDate = census_date_start, 
         eventRemarks  = if_else(
           census_days == 1, "", 
           paste("The census event spanned multiple days.", 
                 "The reported date is the start of the census event window")),  
         sampleSizeValue = census_days,
         sampleSizeUnit = ifelse(census_days <= 1, "day", "days"),
         # whatever cannot be mapped to DwC terms goes to dynamicProperties
         dynamicProperties = sprintf(
           '{"research_program": "%s"}', research_program
         ),
         # add recommended Darwin Core terms: https://dwc.tdwg.org/terms/#event
         coordinateUncertaintyInMeters = "2650",
         higherGeography = paste("Antarctica | South Shetland Islands |", 
                                 "Cape Shirreff, Livingston Island"),
         higherGeographyID = paste0("https://data.aad.gov.au/aadc/gaz/scar/", 
                                    "display_name.cfm?gaz_id=131551"),
         continent = "Antarctica",
         countryCode = "AQ",
         geodeticDatum = "EPSG:4326",
         samplingProtocol = paste(
           "The count of a given species, age class, and sex", 
           "made at the given locality by a trained observer ", 
           "using binoculars during the specified sampling event.")
  ) %>% 
  arrange(eventDate, eventID) %>%
  # fields that cannot be mapped to Darwin Core
  select(-c(season_name, census_days, census_date_start, census_date_end, 
            research_program))

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
  # rename finished columns to Darwin Core terms
  rename(eventID = event_id,
         vernacularName = species_common,
         scientificNameID = lsid,
         taxonRank = rank
  ) %>%
  # add recommended Darwin Core terms: https://dwc.tdwg.org/terms/#occurrence
  mutate(basisOfRecord = "HumanObservation",
         identificationReferences = "https://doi.org/10.1016/C2012-0-06919-0", 
         eventID = case_when(
           location == "Core census locations" ~ paste0(eventID, "-1"), 
           location == "Punta San Telmo" ~ paste0(eventID, "-2"), 
           .default = NA_character_
         ))


# create the long occurrence table
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
           grepl("unk", sex) ~ "indeterminate"
         ), 
         occurrenceID = paste(count_id, lifeStage_id, sex_id, sep = "-")) %>%
  left_join(select(event, eventID, dateIdentified = eventDate), 
            by = "eventID") %>% 
  select(-c(count_id, lifeStage_id, sex_id)) %>% 
  relocate(occurrenceID, .before = eventID)


# ## Visual sanity checks
# occ.long %>%
#   select(-c(eventID, occurrenceID, individualCount, dateIdentified)) %>%
#   lapply(table, useNA = "ifany")

## Sanity checks
stopifnot(
  !any(is.na(occ.long)), 
  sum(duplicated(occ.long$occurrenceID)) == 0, 
  all(occ$eventID %in% event$eventID), 
  all(event$eventID %in% occ$eventID), 
  # Confirm in occurrence that there are 4 records for each eventID
  all.equal(
    unique(occ %>% group_by(eventID) %>% summarise(n_id = n()) %>% pull(n_id)), 
    4), 
  # Confirm that occurrenceID prefix (from manuscript_data.R) matches eventID
  all.equal(
    occ.long$eventID, 
    purrr::map_chr(
      stringr::str_split(occ.long$occurrenceID, "-"), 
      function(i) paste(i[1], i[2], sep = "-"))
  ), 
  # Confirm total counts are still the same, in general and by eventID
  all.equal(sum(x.counts$total_count), sum(occ.long$individualCount)), 
  all.equal(
    x.counts %>% 
      group_by(as.character(event_id), location) %>% 
      summarise(count_sum = sum(total_count), .groups = "drop") %>% 
      pull(count_sum), 
    occ.long %>% 
      group_by(eventID) %>% 
      summarise(count_sum = sum(individualCount), .groups = "drop") %>% 
      pull(count_sum)
  )
)


# write to file
write_tsv(occ.long, here("data", "dwca", "occurrence.txt"), na = "")

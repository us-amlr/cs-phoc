# Examples using CS-PHOC data
# File paths assume you have cloned the repo and are using the CS-PHOC R project

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Join CSV event and count data files
library(dplyr)
library(here)
library(waldo)

x.events <- read.csv(here("data", "manuscript", "cs-phoc-events.csv"))
x.counts <- read.csv(here("data", "manuscript", "cs-phoc-counts.csv"))

x <- left_join(x.events, x.counts, by = join_by(event_id)) %>% 
  mutate(census_date = census_date_start, .after = count_id)
glimpse(x)

# Show that total_count is the derived sum of all the other _count columns
waldo::compare(
  x$total_count, 
  x %>% 
    select(-total_count) %>%
    rowwise() %>% 
    mutate(total_count = sum(c_across(ad_female_count:unk_unk_count),
                             na.rm = TRUE)) %>% 
    pull(total_count)
)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Pivot CSV count data from wide to long
library(dplyr)
library(here)
library(purrr)
library(stringr)
library(tidyr)

x.counts.wide <- read.csv(here("data", "manuscript", "cs-phoc-counts.csv"))

x.counts.long <- x.counts.wide %>% 
  select(-total_count) %>% 
  pivot_longer(ends_with("count"), 
               names_to = "age_class_sex", values_to = "count") %>% 
  filter(!is.na(count)) %>% 
  mutate(acs_split = str_split(age_class_sex, "_"), 
         age_class_pre = map_chr(acs_split, c(1)),
         age_class = case_when(
           age_class_pre ==  "ad" ~ "Adult",
           age_class_pre ==  "juv" ~ "Juvenile",
           age_class_pre ==  "pup" ~ "Pup",
           age_class_pre ==  "unk" ~ "Unknown"
         ), 
         sex = map_chr(acs_split, c(2)),
         sex = case_when(
           sex == "count" & age_class == "Pup" ~ "U", 
           sex == "female" ~ "F", 
           sex == "male" ~ "M", 
           sex == "unk" ~ "U"
         )) %>%
  relocate(age_class, sex, count, .after = "species_common") %>% 
  select(-c(age_class_sex, acs_split, age_class_pre))
glimpse(x.counts.long)

x.events <- read.csv(here("data", "manuscript", "cs-phoc-events.csv"))
x.long <- right_join(x.events, x.counts.long, by = join_by(event_id))
glimpse(x.long)


#------------------------------------------------
# Once data is long, we can create IDs used in Darwin Core Archive files. 
#   For reference, that lat/lon reported in the DwCA files is also added
x.long.dwca <- x.long %>% 
  mutate(sex = sex, #hack for RStudio formatting
         location_id = case_when(
           location == "Core census locations" ~ 1, 
           location == "Punta San Telmo" ~ 2, 
           .default = NA_integer_
         ), 
         eventID = paste(event_id, location_id, sep = "-"), 
         age_class_id = case_when(
           age_class == "Adult" ~ 1,
           age_class == "Juvenile" ~ 2,
           age_class == "Pup" ~ 3,
           age_class == "Unknown" ~ 4,
         ),
         sex_id = case_when(
           grepl("F", sex) ~ 1,
           grepl("M", sex) ~ 2,
           grepl("U", sex) ~ 3
         ),
         occurrenceID = paste(count_id, age_class_id, sex_id, sep = "-"), 
         decimalLatitude = if_else(location == "Core census locations", 
                                   -62.47, -62.4835),
         decimalLongitude = if_else(location == "Core census locations", 
                                    -60.77, -60.808)) %>% 
  select(-c(location_id, age_class_id, sex_id))



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Combining Core census location and Punta San Telmo counts, 
#   if only interested in census counts from 2009/10 and on
library(dplyr)
library(here)
library(tamatoamlr)

x.events <- read.csv(here("data", "manuscript", "cs-phoc-events.csv"))
x.counts <- read.csv(here("data", "manuscript", "cs-phoc-counts.csv"))

x.events.pst <- x.events %>% filter(surveyed_pst) 

x.counts.pst <- x.counts %>% 
  filter(event_id %in% x.events.pst$event_id) 

x.counts.pst.grouped <- x.counts.pst %>% 
  group_by(event_id, species, species_common) %>% 
  summarise(location = "Core census locations + Punta San Telmo", 
            across(ends_with("_count"), ~ tamatoamlr::sum_count(.x, na.rm = TRUE)), 
            .groups = "drop")

# # Sanity check - counts are the same for events where PST was surveyed
# waldo::compare(sum(x.counts.pst$total_count), sum(x.counts.pst$total_count))

# Join with event data
x.pst.grouped <- right_join(x.events, x.counts.pst.grouped, 
                            by = join_by(event_id)) 
glimpse(x.pst.grouped)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Download CS-PHOC data from GBIF, and briefly explore
# Also see: https://docs.ropensci.org/rgbif/index.html

library(rgbif)
library(dplyr)
library(jsonlite)
library(stringr)

# NOTE: use occ_download() for more formal downloads.
#  https://docs.ropensci.org/rgbif/reference/downloads.html
csphoc.gbif <- occ_data(
  datasetKey="2945946f-8a97-41e4-952d-f0a9438b0f2e",
  occurrenceStatus = c("PRESENT", "ABSENT"),
  limit = 100000 #dataset is currently around 14000 records
)

y.orig <- bind_rows(csphoc.gbif$PRESENT$data, csphoc.gbif$ABSENT$data)

y.program <- fromJSON(
  paste0("[", paste(str_replace_all(y.orig$dynamicProperties, "\"\"", "\""),
                    collapse=","), "]"))

y <- y.orig %>%
  select(occurrenceID, eventID,
         eventDate, year, month, day, locality,
         species, sex, lifeStage, individualCount, occurrenceStatus) %>%
  bind_cols(y.program)

# # Sanity checks showing the differences in sex/age class values for data
# #   from the CSV files vs the DwCA tables; see the readme for more details
# tamatoamlr::tableNA(x.long$sex); tamatoamlr::tableNA(y$sex)
# tamatoamlr::tableNA(x.long$age_class); tamatoamlr::tableNA(y$lifeStage)

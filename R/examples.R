# Examples using CS-PHOC data

#-------------------------------------------------------------------------------
# Download CS-PHOC data from GBIF, and format it for use
# Also see: https://docs.ropensci.org/rgbif/index.html

library(rgbif)
library(dplyr)
library(jsonlite)
library(stringr)

# NOTE: use occ_download() for more formal downloads. 
#  See https://docs.ropensci.org/rgbif/reference/downloads.html
csphoc.gbif <- occ_data(
  datasetKey="2945946f-8a97-41e4-952d-f0a9438b0f2e", 
  occurrenceStatus = c("PRESENT", "ABSENT"), 
  limit = 100000 #dataset is currently around 14000 records
)

y.orig <- bind_rows(csphoc.gbif$PRESENT$data, csphoc.gbif$ABSENT$data) 

y.dynamic <- fromJSON(
  paste0("[", paste(str_replace_all(y.orig$dynamicProperties, "\"\"", "\""), 
                    collapse=","), "]"))

y <- y.orig %>% 
  # select(occurrenceID, eventID, dynamicProperties, 
  #        eventDate, year, month, day, locality, 
  #        species, sex, lifeStage, individualCount, occurrenceStatus)
  # mutate(eventDate = as.Date(eventDate)) %>% 
  select(occurrenceID, eventID,  
         eventDate, year, month, day, locality, 
         species, sex, lifeStage, individualCount, occurrenceStatus) %>% 
  bind_cols(y.dynamic)

y %>% filter(nchar(eventDate) > 10)
# y %>% 
#   filter(nchar(eventDate) == 10) %>% 
#   rowwise() %>% 
#   filter(sum(is.na(c_across(c(year, month, day)))) > 0)
# y %>% filter(is.na(lifeStage))

tamatoamlr::tableNA(occ.long$sex); tamatoamlr::tableNA(y$sex)
tamatoamlr::tableNA(occ.long$lifeStage); tamatoamlr::tableNA(y$lifeStage)



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

# Examples of using CS-PHOC data

#-------------------------------------------------------------------------------
### Join header and count data files
library(dplyr)
library(here)

x.events <- read.csv(here("data", "manuscript", "cs-phoc-events.csv"))
x.counts <- read.csv(here("data", "manuscript", "cs-phoc-counts.csv"))

x <- left_join(x.events, x.counts, by = join_by(event_id))
glimpse(x)


#-------------------------------------------------------------------------------
### Pivot count data from wide to long
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

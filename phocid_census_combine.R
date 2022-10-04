# Combine USAMLR and INACH data

library(tidyverse)

tableNA <- function(...) table(..., useNA = 'ifany')


### Read in data, light processing
inach.header <- read.csv("inach_data/phocids_inach_cs_header.csv") %>% 
  mutate(census_days = as.numeric(difftime(as.Date(census_date_end), 
                                           as.Date(census_date_start), 
                                           units = "days"))) %>% 
  select(-week)
inach <- read.csv("inach_data/phocids_inach_cs.csv") %>% 
  mutate(pup_live_count = pup_female_count + pup_male_count, 
         notes = if_else(pup_live_count > 0, 
                         str_glue("pups: ",
                                  "{pup_female_count} females; ", 
                                  "{pup_male_count} males", 
                                  "{pup_unk_count} unknowns"), 
                         NA_character_)) %>% 
  select(-c(week, pup_female_count, pup_male_count, pup_unk_count)) %>% 
  left_join(select(inach.header, -c(season_name, census_days)), 
            by = c("census_phocid_header_id"))

usamlr.header <- read.csv("amlr_data/phocid_census_header_us_amlr.csv") %>% 
  mutate(census_phocid_header_id = as.character(census_phocid_header_id))
usamlr <- read.csv("amlr_data/phocid_census_us_amlr.csv") %>% 
  mutate(census_phocid_header_id = as.character(census_phocid_header_id), 
         research_program = "USAMLR")


### Combine data
y.header <- bind_rows(usamlr.header, inach.header) %>% 
  arrange(census_date_start)

y <- bind_rows(usamlr, inach) %>% 
  arrange(census_date, species, location) %>% 
  rowwise() %>% 
  mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE), 
         total_count_nodead = total_count-pup_dead_count) %>% 
  ungroup()

write.csv(y, row.names = FALSE, na = "", file = "phocids_cs_combined.csv")
write.csv(y.header, row.names = FALSE, na = "", 
          file = "phocids_cs_header_combined.csv")


# Explore
tableNA(y.header$census_days)

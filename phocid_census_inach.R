# Read in and play with INACH phocid census data from Renato

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(readxl)
library(lubridate)
library(DBI)
library(odbc)
library(ggplot2)

tableNA <- function(...) table(..., useNA = 'ifany')

con <- dbConnect(odbc(), Driver = "SQL Server", #"SQL Server Native Client 11.0",
                 Server = "swc-***REMOVED***-s", 
                 Database = "***REMOVED***_Test",
                 Trusted_Connection = "True")

beaches <- tbl(con, "beaches") %>% collect()


################################################################################
### Read in census data, one species to sheet
x.names.info <- c("season_name", "week", "census_date", "Location")
phocid.file.name <- "phocids Cape shirreff 97-2007 VERSION SEPT2021.xlsx"

x.cs <- readxl::read_xlsx(phocid.file.name, sheet = "Crab eater Seal", skip = 1, na = "ND") %>% 
  set_names(c(x.names.info, "ad_male_count", "ad_female_count", 
              "juv_male_count", "juv_female_count", "juv_unk_count", "notes")) %>% 
  mutate(species = "Crabeater seal", 
         census_date = ymd(census_date), 
         across(c(ad_male_count:juv_unk_count), as.numeric), 
         notes = as.character(notes))


x.ls <- readxl::read_xlsx(phocid.file.name, sheet = "Leopard Seal", skip = 1, na = "ND") %>% 
  set_names(c(x.names.info, "ad_male_count", "ad_female_count", "ad_unk_count",
              "juv_male_count", "juv_female_count", "juv_unk_count", "notes")) %>% 
  mutate(species = "Leopard seal", 
         census_date = ymd(census_date), 
         across(c(ad_male_count:juv_unk_count), as.numeric), 
         notes = as.character(notes))


x.ses <- readxl::read_xlsx(phocid.file.name, sheet = "Elephant Seal", skip = 1, na = "ND") %>% 
  set_names(c(x.names.info, "ad_male_count", "ad_female_count",
              "juv_male_count", "juv_female_count",
              "pup_mle_count", "pup_female_count", "pup_unk_count", 
              "unk_unk_count", "notes")) %>% 
  mutate(species = "Elephant seal",
         census_date = ymd(census_date), 
         across(c(ad_male_count:unk_unk_count), as.numeric), 
         notes = as.character(notes))


x.ws <- readxl::read_xlsx(phocid.file.name, sheet = "Weddell Seal", skip = 1, na = "ND") %>% 
  set_names(c(x.names.info, "ad_male_count", "ad_female_count", "ad_unk_count",
              "juv_male_count", "juv_female_count", "juv_unk_count", 
              "pup_mle_count", "pup_female_count", "pup_unk_count", "notes")) %>% 
  # Had to convert Excel file date
  mutate(species = "Weddell seal",
         census_date = ymd(census_date), 
         across(c(ad_male_count:pup_unk_count), as.numeric), 
         notes = as.character(notes))



################################################################################
# Bind all census data into one data frame, and explore
x <- bind_rows(x.cs, x.ls, x.ses, x.ws) %>% 
  select(season_name, week, census_date, Location, species, 
         starts_with("ad_"), starts_with("juv"), starts_with("pup_"), 
         unk_unk_count, notes) %>% 
  arrange(census_date, Location, species)

#-------------------------------------------------------------------------------
### Issues/questions - sent to Renato 9 Mar 2022
# There are some duplicates, across date/location/species
x.issue.dup <- x %>% 
  group_by(census_date, Location, species) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(season_name, week)
write.csv(x.issue.dup, row.names = FALSE, 
          file = "inach/phocid_census_inach_duplicates.csv")


# Season name - week - date
x.issue.header <- x %>% 
  group_by(season_name, week, census_date) %>% 
  summarise(num_records = n(), .groups = "drop") %>% 
  group_by(season_name, week) %>% 
  mutate(season_week_duplicate = if_else(n() > 1, TRUE, NA)) %>% 
  ungroup()

# x.issue.week <- x.header %>% 
#   group_by(season_name, week) %>% 
#   filter(n() > 1) %>% 
#   ungroup()

write.csv(x.issue.header, row.names = FALSE, na = "", 
          file = "inach/phocid_census_inach_headers.csv")


# Beach name values that aren't in DB beaches table
x.beach.unk <- x %>% filter(!(Location %in% beaches$name))
tableNA(x.beach.unk$Location)
x.issue.beach <- x %>% 
  filter(Location %in% c("Escondida", "Hucke-Gaete", "Pta Mann", "Pta Nacella", 
                         "Pta Olivia", "Pta Yeco"))

write.csv(x.issue.beach, row.names = FALSE, na = "", 
          file = "inach/phocid_census_inach_unkbeaches.csv")


#-------------------------------------------------------------------------------
### Plotting
x.toplot <- x %>% 
  group_by(season_name, week, census_date, species) %>% 
  summarise(across(c(ad_male_count:unk_unk_count), sum, na.rm = TRUE, 
                   .names = ), 
            .groups = "drop") %>% 
  mutate(wk_id = paste(season_name, str_pad(week, 2, pad = "0"), sep = "_")) %>% 
  # pivot_longer(ad_male_count:unk_unk_count, 
  #              names_to = c("age_class", "sex"), names_pattern = "(.*)_(.*)_count")
  pivot_longer(ad_male_count:unk_unk_count, names_pattern = "(.*)_count",
               names_to = "age_sex", values_to = "count") %>% 
  mutate(grp = paste(season_name, age_sex, sep = "_"), 
         age_sex = factor(age_sex))


i.sp <- "Elephant seal"
x.toplot %>% 
  filter(species == i.sp) %>% 
  ggplot(aes(wk_id, count, group = grp, color = age_sex)) +
  # ggplot(aes(census_date, count, group = grp, color = age_sex)) +
  geom_point() + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle(i.sp) + 
  xlab("season_week")

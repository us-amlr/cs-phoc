# Sam's early and ad-hoc CS-PHOC census data qc and explorations

library(dplyr)
library(tidyr)
library(DBI)
library(lubridate)
library(ggplot2)
library(googlesheets4)
library(tamatoamlr)

#-------------------------------------------------------------------------------
### Code to check if all species have records for all header records
# `x` here is from import_inach.R
d <- x %>% 
  filter(census_date < as.Date("2009-07-01")) %>% 
  group_by(header_id) %>% 
  summarise(census_date = list(sort(unique(census_date))), 
            has_crab = any(species == 'Crabeater seal'), 
            has_ses = any(species == 'Elephant seal'), 
            has_leop = any(species == 'Leopard seal'), 
            has_wedd = any(species == 'Weddell seal')) %>% 
  rowwise() %>% 
  filter(!all(has_crab, has_ses, has_leop, has_wedd)) %>% 
  ungroup()

paste(d$census_date)



#-------------------------------------------------------------------------------
con <- amlr_dbConnect(Database = "***REMOVED***")
census.phocid.orig <- tbl(con, "vCensus_Phocid") %>% collect()

census.phocid <- census.phocid.orig %>% 
  rowwise() %>% 
  mutate(counts_sum = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE), 
         counts_allzero = all(counts_sum == 0)) %>% 
  ungroup()


# tbl(con, "vCensus_Phocid") %>% collect()
# tbl(con, "vCensus_AFS_Study_Beach") %>% collect()
# tbl(con, "vCensus_AFS_Capewide_Pup") %>% collect()

tableNA(census.phocid$census_type)

# census.phocid <- census %>% filter(census_type == "Phocid")
# census.phocid.na <- lapply(census.phocid, function(i) sum(!is.na(i)))
# census.phocid.na[census.phocid.na == 0]
# 
# 
# census.tmp <- census %>% filter(census_type == "AFS Study Beach")
# census.tmp.na <- lapply(census.tmp, function(i) sum(!is.na(i)))
# census.tmp.na[census.tmp.na == 0]
# 
# census.tmp <- census %>% filter(census_type == "Capewide")
# census.tmp.na <- lapply(census.tmp, function(i) sum(!is.na(i)))
# census.tmp.na[census.tmp.na == 0]


#-------------------------------------------------------------------------------
### Check out rows that have 0s for all counts

tableNA(census.phocid$season_name)
tableNA(census.phocid$season_name, census.phocid$counts_allzero)

with(census.phocid %>% filter(counts_allzero), tableNA(observer))
with(census.phocid %>% filter(counts_allzero), tableNA(Location, season_name))
with(census.phocid %>% filter(counts_allzero), tableNA(Location, observer))

# This ^ suggests that it was pretty rare for an AMLR someone to enter all zeros 
#   for a beach when they didn't see any phocids (e.g., SMW did not from 2016-18)



#-------------------------------------------------------------------------------
# Identify duplicates, and write them to a Sheet for Doug

# census.phocid.dup <- census.phocid %>% 
#   group_by(census_date, Location, species) %>% 
#   filter(n() > 1) %>% 
#   arrange(census_date, Location, species) %>% 
#   ungroup()

# census.phocid.dup.time <- census.phocid %>% 
#   group_by(census_date, Location, species, time_start, time_end) %>% 
#   filter(n() > 1) %>% 
#   arrange(census_date, Location, species, time_start, time_end) %>% 
#   ungroup()

func_diff <- function(x) if_else(length(unique(x)) > 1, TRUE, NA)

census.phocid.dup.proc <- census.phocid %>% 
  group_by(census_date, Location, species) %>% 
  filter(n() > 1, season_name != "2011/12") %>% 
  mutate(diff_counts = if_any(ad_female_count:unk_unk_count, func_diff), 
         diff_counts2 = func_diff(ad_female_count) | func_diff(ad_male_count),
         diff_times = func_diff(time_start) | func_diff(time_end), 
         diff_obs = func_diff(observer), 
         diff_any = diff_counts | diff_times | diff_obs,
         djk_comments = NA_character_, smw_comments = NA_character_) %>% 
  ungroup() %>% 
  select(djk_comments, smw_comments, 
         diff_any, diff_counts, diff_times, diff_obs, census_id, 
         season_name, observer, census_date, time_start, time_end, 
         Location, species, counts_sum, ad_female_count:unk_unk_count, 
         header_notes, census_notes, census_created_dt) %>% 
  arrange(census_date, Location, species)

# googlesheets4::write_sheet( #WARNING - will overwrite comments
#   census.phocid.dup.proc, sheet = "census_phocid_duplicates_tmp",
#   ss = paste0("https://docs.google.com/spreadsheets/d/1naKQhjKdv8JsuiG3VANkeAk7uYtYYgSRgjIi7dQMObk")
# )



# #-------------------------------------------------------------------------------
# Get data for creating phocid census header

### Dates and counts per date
tableNA(census.phocid.orig$census_date, census.phocid$season_name)

tableNA(census.phocid.orig$Beach)


### arrange by dates, get diffs, then ID
cp.date.diffs <- census.phocid.orig %>%
  arrange(census_date, time_start, species) %>%
  mutate(census_date = as.Date(census_date),
         date_diff_from_prev = as.numeric(
           difftime(census_date, lag(census_date), units = "days"))) %>%
  filter(is.na(date_diff_from_prev) | date_diff_from_prev != 0) %>%
  select(season_info_id:census_type, census_date, date_diff_from_prev)

nrow(census.phocid.orig) - nrow(cp.date.diffs)
tableNA(cp.date.diffs$date_diff_from_prev)

# googlesheets4::write_sheet(
#   cp.date.diffs, sheet = "dates", 
#   ss = paste0("https://docs.google.com/spreadsheets/d/1BB4ZEnoy-AMbEADBBzKYxO3qkOEp_redhVBO8tYTR5k")
# )


#-------------------------------------------------------------------------------
# Beaches exploration

### Which dates don't have San Telmo data?
cp.dates <- sort(unique(census.phocid.orig$census_date))

pst.count <- sapply(cp.dates, function(i) {
  census.phocid.orig %>% 
    filter(census_date == i, 
           Beach == "San Telmo, Punta") %>% 
    nrow()
}, USE.NAMES = TRUE) 

pst.count.zero <- data.frame(date = as.Date(names(pst.count[pst.count == 0]))) %>% 
  # The following dates actually had San Telmo counts, eg on a different day
  # filter(date %in% c(ymd("2012-01-27"))) %>% 
  mutate(yr_tmp = if_else(month(date) < 7, year(date)-1, year(date)),
         season_name = paste0(yr_tmp, "/", yr_tmp-1999)) %>% 
  select(-yr_tmp)
tableNA(pst.count.zero$season_name)
pst.count.zero

# write.csv(pst.count.zero, row.names = FALSE, 
#           file = "db_noSanTelmo.csv")
# googlesheets4::write_sheet( #WARNING - will overwrite Sam notes
#   no_San_Telmo, sheet = "dates", 
#   ss = paste0("https://docs.google.com/spreadsheets/d/1BB4ZEnoy-AMbEADBBzKYxO3qkOEp_redhVBO8tYTR5k")
# )


### Beach names by season
tableNA(census.phocid.orig$Beach, census.phocid.orig$season_name)

season.beaches <- as.data.frame(
  tableNA(census.phocid.orig$Beach, census.phocid.orig$season_name), 
  stringsAsFactors = FALSE
) %>% 
  rename(Beach = Var1, season_name  = Var2, count = Freq) %>% 
  pivot_wider(id_cols = Beach, names_from = season_name, values_from = count) %>% 
  rowwise() %>% 
  mutate(num_seasons_nonzero = sum(c_across(`2011/12`:`2021/22`) != 0)) %>% 
  ungroup()


# googlesheets4::write_sheet( #WARNING - will overwrite Sam notes in Sheet
#   season.beaches, sheet = "beaches", 
#   ss = paste0("https://docs.google.com/spreadsheets/d/1BB4ZEnoy-AMbEADBBzKYxO3qkOEp_redhVBO8tYTR5k")
# )


#-------------------------------------------------------------------------------
# Plotting exploration
x.toplot <- census.phocid %>% 
  group_by(season_name, census_date_start, species) %>% 
  summarise(across(c(ad_female_count:unk_unk_count), sum, na.rm = TRUE, 
                   .names = ), 
            .groups = "drop") %>% 
  pivot_longer(ad_female_count:unk_unk_count, names_pattern = "(.*)_count",
               names_to = "age_sex", values_to = "count") %>% 
  mutate(grp = paste(season_name, age_sex, sep = "_"), 
         age_sex = factor(age_sex))


i.sp <- "Crabeater seal"
x.out <- x.toplot %>% 
  filter(species == i.sp) %>% 
  ggplot(aes(census_date_start, count, group = grp, color = age_sex)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ season_name, scales = "free_x") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle(i.sp)
ggsave(here("output", "amlr_plots", "crabby.png"), 
       x.out, width = 12, height = 8)


#-------------------------------------------------------------------------------
# Which count columns were used in which seasons
library(tidyverse)

x <- read.csv(here("data", "amlr_data", "phocids_cs_amlr.csv"))
x.summ.count <- x %>% 
  group_by(season_name) %>% 
  summarise(across(ad_female_count:unk_unk_count, function(i) sum(!is.na(i))))

x.summ.count.na <- x %>% 
  group_by(season_name) %>% 
  summarise(across(ad_female_count:unk_unk_count, function(i) sum(is.na(i))))


#-------------------------------------------------------------------------------


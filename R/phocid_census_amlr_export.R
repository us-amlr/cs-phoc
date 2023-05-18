# Extract and export US AMLR phcoid census data from database and write to CSVs
# NOTEs: 
#   census_phocid_header_id is renamed to header_id


library(dplyr)
library(googlesheets4)
library(here)
library(amlrPinnipeds)


con <- amlr_dbConnect(Database = "***REMOVED***")


# Get all data and write to a sheet
x <- tbl(con, "vCensus_Phocid") %>% 
  arrange(season_name, census_date, species, location_group) %>% 
  collect() %>% 
  rowwise() %>% 
  mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE), 
         total_count_nodead = sum(c_across(c(ad_female_count:pup_live_count, 
                                             unk_female_count:unk_unk_count)), 
                                  na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(header_id = census_phocid_header_id)

# write_sheet(x, ss = url.export, sheet = "phocids_cs_amlr_all")


# Select columns of data and write to a sheet
x.tosend <- x %>% 
  select(season_name, census_date_start, census_date_end, surveyed_san_telmo, 
         observer:location_group, species, 
         total_count, total_count_nodead, ad_female_count:unk_unk_count, 
         header_notes, census_notes, header_id, census_id)

# write_sheet(x.tosend, ss = url.export, sheet = "phocids_cs_amlr")
write.csv(x.tosend, row.names = FALSE,
          file = here("data", "amlr_data", "phocids_cs_amlr.csv"))

# Write header data to sheet
x.header <- tbl(con, "vCensus_Phocid_Header") %>% 
  select(season_name, census_date_start, census_date_end, census_days, 
         surveyed_san_telmo, 
         header_notes = notes, header_id = census_phocid_header_id) %>% 
  arrange(season_name, census_date_start) %>% 
  collect()

# write_sheet(x.header, ss = url.export, sheet = "phocids_cs_amlr_header")
write.csv(x.header, row.names = FALSE,
          file = here("data", "amlr_data", "phocids_cs_amlr_header.csv"))


# sanity checks
table(x$location_group)
stopifnot(
  all(x$census_type == "Phocid"), 
  0 == (x %>% 
          group_by(header_id, species, location) %>% 
          filter(n() > 1) %>% 
          nrow()), 
  all(x.header$header_id %in% x.tosend$header_id), 
  sum(is.na(x$header_id)) == 0
)

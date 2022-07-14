library(odbc)
library(dplyr)
library(googlesheets4)

con <- dbConnect(odbc(), Driver = "ODBC Driver 18 for SQL Server", 
                 Server = "swc-***REMOVED***-s", 
                 Database = "***REMOVED***",
                 Trusted_Connection = "Yes", 
                 Encrypt = "Optional")

url.export <- "https://docs.google.com/spreadsheets/d/1bzjN_uUcJxc7o-CnPEa4U69QJWVlDpCB92oteaGM_Ec"


# Get all data and write to a sheet
x <- tbl(con, "vCensus_Phocid") %>% 
  arrange(season_name, census_date, species, location_group) %>% 
  collect() %>% 
  rowwise() %>% 
  mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE), 
         total_count_nodead = sum(c_across(c(ad_female_count:pup_live_count, 
                                             unk_female_count:unk_unk_count)), 
                                  na.rm = TRUE)) %>% 
  ungroup()

# write_sheet(x, ss = url.export, sheet = "phocid_census_us_amlr_all")


# Select columns of data and write to a sheet
x.tosend <- x %>% 
  select(season_name, census_date_start, census_date_end, surveyed_san_telmo, 
         observer:location_group, species, 
         total_count, total_count_nodead, ad_female_count:unk_unk_count, 
         header_notes, census_notes, census_phocid_header_id, census_id)

write_sheet(x.tosend, ss = url.export, sheet = "phocid_census_us_amlr")


# Write header data to sheet
x.header <- tbl(con, "vCensus_Phocid_Header") %>% 
  select(season_name, census_date_start, census_date_end, census_days, 
         surveyed_san_telmo, 
         census_phocid_header_notes = notes, census_phocid_header_id) %>% 
  arrange(season_name, census_date_start) %>% 
  collect()

write_sheet(x.header, ss = url.export, sheet = "phocid_census_header_us_amlr")



# sanity checks
table(x$location_group)
stopifnot(
  all(x$census_type == "Phocid"), 
  0 == (x %>% 
          group_by(census_phocid_header_id, species, location) %>% 
          filter(n() > 1) %>% 
          nrow()), 
  all(x.header$census_phocid_header_id %in% x.tosend$census_phocid_header_id), 
  sum(is.na(x$census_phocid_header_id)) == 0
)

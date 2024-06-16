# Combine USAMLR and INACH data, and aggregate for manuscript

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
library(dplyr)
library(readr)
library(here)
library(tamatoamlr)
library(worrms)

con <- amlr_dbConnect(Database = "***REMOVED***")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Read in census header and record data from AMLR_PINNIEPDS database
cs.header.orig <- tbl(con, "vCensus_Phocid_Header") %>%
  arrange(census_date_start) %>% 
  collect() %>% 
  group_by(season_name) %>% 
  mutate(season_idx = seq_along(census_date_start), 
         header_id = census_phocid_header_id) %>% 
  ungroup() %>% 
  select(header_id, census_phocid_header_id, season_name, 
         census_date_start, census_date_end, census_days, 
         surveyed_pst, research_program) %>% 
  # TODO: temporary to avoid including 2023/24 data
  filter(census_date_start < as.Date("2023-07-01"))

cs.header <- cs.header.orig %>% select(-census_phocid_header_id)

stopifnot(
  nrow(cs.header) == nrow(collect(tbl(con, "census_phocid_header")) %>% 
                            # TODO: temporary to avoid including 2023/24 data
                            filter(census_date_start < as.Date("2023-07-01")))
)


cs.wide <- tbl(con, "vCensus_Phocid") %>% 
  arrange(census_date, species, location_group) %>% 
  rename(header_id = census_phocid_header_id) %>% 
  # TODO: temporary to avoid including 2023/24 data
  filter(census_date < as.Date("2023-07-01")) %>% 
  collect() %>% 
  select(header_id, observer, census_date, location_group, species, 
         ends_with("_count")) %>% 
  select(-pup_dead_count) %>% 
  group_by(header_id, location_group, species) %>% 
  summarise(across(c(census_date), unique), 
            across(ends_with("_count"), sum), 
            .groups = "drop") %>% 
  relocate(census_date, .after = header_id) %>% 
  arrange(census_date, species, tolower(location_group))

# # Sanity checks
# lapply(cs.wide, function(i) {
#   tableNA(amlr_season_from_date(cs.wide$census_date), is.na(i))
# })
# all(cs.wide$header_id %in% cs.header$header_id)
# tableNA(cs.wide$location_group)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Aggregate by locations, complete, and combine

### Filter by core locations, group/summarise, and complete
loc.core <- "Core census locations"
loc.pst <- "Punta San Telmo"

amlr.header <- cs.header %>% filter(research_program == "USAMLR")

cs.core.agg <- cs.wide %>% 
  # Filter for core locations, sum by header_id/species
  filter(location_group %in% c(tamatoamlr::csphoc.core.location.groups)) %>% 
  select(-location_group) %>% 
  group_by(header_id, location = loc.core, species) %>% 
  summarise(across(ends_with("_count"), sum_count),  
            .groups = "drop") %>% 
  left_join(select(cs.header, header_id, census_date_start, research_program), 
            by = join_by(header_id))

# Complete the AMLR data, and bind all back together
amlr.complete <- cs.core.agg %>% 
  filter(research_program == "USAMLR") %>% 
  complete_csphoc() %>% 
  select(-census_date_start)

cs.core.complete <- cs.core.agg %>% 
  filter(research_program == "INACH") %>% 
  bind_rows(amlr.complete) %>% 
  # mutate(location_lat = -62.47, 
  #        location_lon = -60.77,
  #        .after = location) %>% 
  select(-c(census_date_start, research_program))



### Filter for PST locations, group/summarise, and complete
pst.header <- cs.header %>% filter(surveyed_pst)

cs.pst.complete <- cs.wide %>% 
  filter(location_group %in% c(loc.pst)) %>% 
  rename(location = location_group) %>% 
  select(-c(census_date)) %>% 
  left_join(select(pst.header, header_id, census_date_start, research_program), 
            by = join_by(header_id)) %>% 
  # Insert dummy data for two surveys where PST was surveyed, 
  #   but no phocids were recorded
  add_row(census_date_start = as.Date("2009-11-01"), 
          research_program = "USAMLR", 
          header_id = pst.header %>% 
            filter(census_date_start == as.Date("2009-11-01")) %>% 
            pull(header_id), 
          location = loc.pst, 
          species = unname(pinniped.phocid.sp[4]), 
          ad_female_count = 0L, ad_male_count = 0L, ad_unk_count = 0L, 
          juv_female_count = 0L, juv_male_count = 0L, juv_unk_count = 0L, 
          pup_live_count = 0L, 
          unk_female_count = NA_integer_, unk_male_count = NA_integer_, 
          unk_unk_count = NA_integer_) %>% 
  add_row(census_date_start = as.Date("2009-11-08"), 
          research_program = "USAMLR", 
          header_id = pst.header %>% 
            filter(census_date_start == as.Date("2009-11-08")) %>% 
            pull(header_id), 
          location = loc.pst, 
          species = unname(pinniped.phocid.sp[4]), 
          ad_female_count = 0L, ad_male_count = 0L, ad_unk_count = 0L, 
          juv_female_count = 0L, juv_male_count = 0L, juv_unk_count = 0L, 
          pup_live_count = 0L, 
          unk_female_count = NA_integer_, unk_male_count = NA_integer_, 
          unk_unk_count = NA_integer_) %>% 
  complete_csphoc() %>%
  # mutate(location_lat = -62.4835, 
  #        location_lon =  -60.808,
  #        .after = location) %>% 
  select(-c(census_date_start, research_program))


### Bind core and pst records together. Create count ID
lookup.species <- tbl(con, "lookup_pinniped_species") %>% 
  filter(phocid_census == 1) %>% 
  collect() %>% 
  select(species_common = display_name, 
         species = scientific_name)

matched_taxa_tibbles <- wm_records_names(unique(lookup.species$species))
matched_taxa <- bind_rows(matched_taxa_tibbles) %>% 
  rename(scientificName = "scientificname") %>%
  select(species = scientificName, valid_AphiaID)
stopifnot(nrow(matched_taxa) == 4)

cs.core.pst <- bind_rows(cs.core.complete, cs.pst.complete)%>%
  tamatoamlr::total_count() %>%
  arrange(header_id, location, species) %>%
  relocate(location, .after = header_id) %>%
  relocate(total_count, .before = ad_female_count) %>% 
  rename(species_common = species) %>% 
  left_join(lookup.species, by = join_by(species_common)) %>% 
  left_join(matched_taxa, by = join_by(species)) %>% 
  mutate(species_common = if_else(species_common == "Elephant seal", 
                                  "Southern elephant seal", species_common), 
         location_id = case_when(
           location == loc.core ~ 1, 
           location == loc.pst ~ 2
         ), 
         count_id = paste(header_id, location_id, valid_AphiaID, sep = "-")) %>% 
  select(-c(valid_AphiaID, location_id)) %>% 
  relocate(species, species_common, .before = total_count) %>% 
  relocate(count_id, .before = header_id) %>% 
  rename(pup_count = pup_live_count)


#-------------------------------------------------------------------------------
# Rename header to event to make consistent with DwC-A files
# Done here so as to not introduce errors in previously written code
cs.events <- cs.header %>% rename(event_id = header_id)
cs.counts <- cs.core.pst %>% rename(event_id = header_id)

#-------------------------------------------------------------------------------
# Save data
write_csv(cs.events, here("data", "manuscript", "cs-phoc-events.csv"), na = "")
write_csv(cs.counts, here("data", "manuscript", "cs-phoc-counts.csv"), na = "")



#-------------------------------------------------------------------------------
# Sanity checks
library(waldo)
stopifnot(
  sum(is.na(cs.core.pst$header_id)) == 0,
  sum(is.na(cs.core.pst$species)) == 0,
  all(cs.header$header_id %in% cs.core.pst$header_id),
  all(cs.core.pst$header_id %in% cs.header$header_id),
  (nrow(cs.core.pst)) ==
    (4 * nrow(cs.header) + 4 * sum(cs.header$surveyed_pst))
)

# # To explore an unexpected difference in row counts
# d1 <- cs.core.pst %>% group_by(header_id) %>% summarise(n = n())
# d2 <- cs.header %>% 
#   mutate(n = 4L + 4L*as.integer(surveyed_pst)) %>% 
#   select(header_id, n) %>% 
#   arrange(header_id)
# waldo::compare(d1, d2)

cs.wide %>%
  filter(!(location_group %in% csphoc.core.location.groups)) %>%
  select(location_group) %>%
  tableNA()

opportunistic <- cs.wide %>% 
  filter(!(location_group %in% c(csphoc.core.location.groups, loc.pst)))

waldo::compare(
  cs.wide %>%
    summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))),
  (opportunistic %>%
     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
     cs.pst.complete %>%
     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
     cs.core.complete %>%
     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE)))) %>%
    as_tibble()
)

waldo::compare(
  cs.wide %>%
    summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>%
    as.data.frame(),
  (opportunistic %>%
     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
     cs.pst.complete %>%
     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
     cs.core.complete %>%
     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))))
)

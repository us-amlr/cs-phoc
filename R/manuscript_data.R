# Combine USAMLR and INACH data, and aggregate for manuscript

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
library(tidyverse)
library(here)
library(amlrPinnipeds)
library(waldo)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Read in census header and record data from AMLR_PINNIEPDS database
con <- amlr_dbConnect(Database = "***REMOVED***")

cs.header.orig <- tbl(con, "vCensus_Phocid_Header") %>%
  arrange(census_date_start) %>% 
  collect() %>% 
  group_by(season_name) %>% 
  mutate(season_idx = seq_along(census_date_start), 
         # header_id = paste(season_name, 
         #                   str_pad(season_idx, width = 2, pad = "0"), 
         #                   sep = "-"), 
         header_id = census_phocid_header_id) %>% 
  ungroup() %>% 
  select(header_id, census_phocid_header_id, season_name, 
         census_date_start, census_date_end, census_days, 
         surveyed_san_telmo, research_program)

cs.header <- cs.header.orig %>% select(-census_phocid_header_id)

stopifnot(
  nrow(cs.header) == nrow(collect(tbl(con, "census_phocid_header")))
)


cs.wide <- tbl(con, "vCensus_Phocid") %>% 
  arrange(census_date, species, location_group) %>% 
  rename(header_id = census_phocid_header_id) %>% 
  collect() %>% 
  # left_join(select(cs.header.orig, header_id, census_phocid_header_id), 
  #           by = "census_phocid_header_id") %>% 
  select(header_id, observer, census_date, location_group, species, 
         ends_with("_count")) %>% 
  select(-pup_dead_count) %>% 
  group_by(header_id, location = location_group, species) %>% 
  summarise(across(c(census_date), unique), 
            across(ends_with("_count"), sum), 
            .groups = "drop") %>% 
  relocate(census_date, .after = header_id) %>% 
  arrange(census_date, species, tolower(location))

# # Sanity checks
# lapply(cs.wide, function(i) {
#   tableNA(amlr_season_from_date(cs.wide$census_date), is.na(i))
# })
# all(cs.wide$header_id %in% cs.header$header_id)
# tableNA(cs.wide$location)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Aggregate by locations, complete, and combine

### Filter by core locations, group/summarise, and complete
loc.core <- "Core census locations"
loc.pst <- "Punta San Telmo"

amlr.header <- cs.header %>% filter(research_program == "USAMLR")

cs.core.agg <- cs.wide %>% 
  # Filter for core locations, sum by header_id/species
  filter(location %in% c(csphoc.core.location.groups)) %>% 
  group_by(header_id, location = loc.core, species) %>% 
  summarise(across(ends_with("_count"), sum_count),  
            # orig_record = TRUE, 
            .groups = "drop") %>% 
  left_join(select(cs.header, header_id, research_program), 
            by = join_by(header_id))

# Complete the AMLR data, and bind all back together
amlr.complete <- cs.core.agg %>% 
  filter(research_program == "USAMLR") %>% 
  csphoc_complete_aggregated(amlr.header, fill.location = loc.core)

cs.core.complete <- cs.core.agg %>% 
  filter(research_program == "INACH") %>% 
  bind_rows(amlr.complete) %>% 
  select(-research_program)



### Filter for san telmo locations, group/summarise, and complete
pst.header <- cs.header %>% filter(surveyed_san_telmo)

cs.pst.complete <- cs.wide %>% 
  filter(location %in% c(loc.pst)) %>% 
  select(-c(census_date)) %>% 
  left_join(select(pst.header, header_id, research_program), 
            by = join_by(header_id)) %>% 
  csphoc_complete_aggregated(pst.header, fill.location = loc.pst) %>% 
  select(-research_program)


### Bind core and pst records together
cs.core.pst <- bind_rows(cs.core.complete, cs.pst.complete)%>%
  amlrPinnipeds::total_count() %>%
  arrange(header_id, location, species) %>%
  relocate(location, .after = header_id) %>%
  relocate(total_count, .before = ad_female_count) %>% 
  mutate(species_common = if_else(species == "Elephant seal", 
                                  "Southern elephant seal", species), 
         species = case_when(
           species_common == "Crabeater seal" ~ "Lobodon carcinophagus", 
           species_common == "Southern elephant seal" ~ "Mirounga leonina",
           species_common == "Leopard seal" ~ "Hydrurga leptonyx",
           species_common == "Weddell seal" ~ "Leptonychotes weddellii")) %>% 
  relocate(species_common, .after = species) %>% 
  rename(pup_count = pup_live_count)


#-------------------------------------------------------------------------------
### Save data
write_csv(cs.header, file = here("manuscript", "cs-phoc-headers.csv"), na = "")
write_csv(cs.core.pst, here("manuscript", "cs-phoc-counts.csv"), na = "")


#-------------------------------------------------------------------------------
# # Sanity checks
# stopifnot(
#   sum(is.na(cs.core.pst$header_id)) == 0,
#   sum(is.na(cs.core.pst$species)) == 0,
#   all(cs.header$header_id %in% cs.core.pst$header_id),
#   all(cs.core.pst$header_id %in% cs.header$header_id),
#   (nrow(cs.core.pst)) ==
#     (4 * nrow(cs.header) + 4 * sum(cs.header$surveyed_san_telmo))
# )


# cs.wide %>%
#   filter(!(location %in% csphoc.core.location.groups)) %>%
#   select(location) %>% tableNA()

# opportunistic <- cs.wide %>%
#   filter(!(location %in% c(csphoc.core.location.groups, loc.pst)))
# # opportunistic %>%
# #   summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>%
# #   glimpse()
# # cs.pst.complete %>%
# #   summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>%
# #   glimpse()
# # cs.core.complete %>%
# #   summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>%
# #   glimpse()
# # cs.wide %>%
# #   summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>%
# #   glimpse()
# # 
# # (opportunistic %>%
# #     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
# #     cs.pst.complete %>%
# #     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
# #     cs.core.complete %>%
# #     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE)))) %>%
# #   glimpse()
# waldo::compare(
#   cs.wide %>% 
#     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
#     as.data.frame(), 
#   (opportunistic %>% 
#      summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
#      cs.pst.complete %>% 
#      summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
#      cs.core.complete %>% 
#      summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))))
# )

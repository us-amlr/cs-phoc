# Combine USAMLR and INACH data, and aggregate for manuscript

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
library(tidyverse)
library(here)
library(amlrPinnipeds)
library(waldo)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# INACH
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Read in INACH data, light processing
inach.path <- here("data", "inach_data")
inach.header <- read_csv(here(inach.path, "phocids_cs_inach_header.csv"), 
                         col_types = c("ccDDlc")) %>% 
  mutate(census_days = 1 + as.numeric(difftime(census_date_end, 
                                               census_date_start, 
                                               units = "days")))

inach.orig <- read_csv(here(inach.path, "phocids_cs_inach.csv"), 
                       col_types = "ccDccciiiiiiiicc") %>% 
  rename(census_notes = notes) 


# Group/sum INACH records by header ID, location_group, and species
inach <- inach.orig %>% 
  group_by(header_id, location_group, species) %>% 
  summarise(across(c(season_name, census_date, research_program), unique), 
            across(ends_with("_count"), sum), 
            census_notes = if_else(all(is.na(census_notes)), 
                                   NA_character_,
                                   paste(na.omit(census_notes), 
                                         collapse = "; ")),
            orig_record = TRUE, 
            .groups = "drop") %>% 
  mutate(census_notes = if_else(census_notes == "pups: 3 females, 2 males, 2 unknowns; pups: 0 females, 0 males, 1 unknowns", 
                                "pups: 3 females, 2 males, 3 unknowns", 
                                census_notes)) %>% 
  rename(location = location_group)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# US AMLR
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### Read in AMLR data from database, do light processing. 
con <- amlr_dbConnect(Database="***REMOVED***")

### Header
amlr.header.orig <- tbl(con, "vCensus_Phocid_Header") %>% 
  select(season_name, census_date_start, census_date_end, census_days, 
         surveyed_san_telmo, 
         header_notes = notes, header_id = census_phocid_header_id) %>% 
  arrange(season_name, census_date_start) %>% 
  collect()

# write.csv(amlr.header.orig, row.names = FALSE,
#           file = here("data", "amlr_data", "phocids_cs_amlr_header.csv"))

amlr.header <- amlr.header.orig %>% 
  mutate(header_id = as.character(header_id), 
         research_program = "USAMLR")


### Census data
amlr.orig <- tbl(con, "vCensus_Phocid") %>% 
  arrange(season_name, census_date, species, location_group) %>% 
  rename(header_id = census_phocid_header_id) %>% 
  collect() %>% 
  # rowwise() %>% 
  # mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE), 
  #        total_count_nodead = sum(c_across(c(ad_female_count:pup_live_count, 
  #                                            unk_female_count:unk_unk_count)), 
  #                                 na.rm = TRUE)) %>% 
  # ungroup() %>% 
  select(header_id, season_name, observer:location_group, species, 
         ends_with("_count"), census_notes, research_program)


# Aggregate up to location_group level
amlr.agg <- amlr.orig %>% 
  mutate(header_id = as.character(header_id)) %>% 
  # select(-c(observer, total_count, total_count_nodead)) %>% 
  group_by(header_id, location_group, species) %>% 
  summarise(across(c(season_name, census_date, research_program), unique), 
            across(ends_with("_count"), sum), 
            #suppressWarnings is for message from min(NA)
            time_start = suppressWarnings(min(time_start, na.rm = TRUE)), 
            time_end = suppressWarnings(min(time_end, na.rm = TRUE)), 
            # notes_count = sum(!is.na(census_notes)), 
            #there are no groups where multiple notes need to be concatenated
            census_notes = if (all(is.na(census_notes))) NA_character_ else
              as.character(na.omit(census_notes)),
            .groups = "drop") %>% 
  rename(location = location_group) %>% 
  relocate(season_name, census_date, time_start, time_end, 
           .after = header_id) %>% 
  relocate(research_program, .after = census_notes)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Aggregate up for manuscript
# Goal: Capewide counts, for each census header record.
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Combine INACH and USAMLR data
combined.header <- amlr.header %>% 
  bind_rows(inach.header) %>% 
  relocate(header_id, .after = last_col()) %>% 
  arrange(census_date_start)

combined.wide <- bind_rows(amlr.agg, inach) %>% 
  arrange(census_date, species, tolower(location)) 

lapply(combined.wide, function(i) tableNA(combined.wide$season_name, is.na(i)))
all(combined.wide$header_id %in% combined.header$header_id)
tableNA(combined.wide$location)


# Core Locations
locations.core <- c(
  "Media Luna", "Yuseff, Punta", "Larga", "Marko", "Daniel", "Modulo", 
  "Hue", "Copi", "Maderas", "Cachorros", "Chungungo", 
  "Ballena Sur", "Ballena Norte", "Bahamonde", "Nibaldo", "Roquerio", 
  "Alcazar", "Pinochet de la Barra", "Papua", "Antartico, Playa", 
  "Loberia", "Yamana, Playa", 
  
  "Golondrina, Playa", "del Lobero, Playa", "Paulina, Playa", 
  "Schiappacasse, Playa", "El Plastico, Playa", "Leopard Beach", 
  "del Canal, Playa", 
  
  "Golondrina-del Lobero", "Paulina-Aranda", 
  
  "Cape Shirreff", "Copihue", "Cerro Gajardo, Peninsula", 
  
  # TODO: ???
  "Aranda", "El Remanso, Playa"
)
# combined.wide %>% 
#   filter(location %in% c("Aranda", "El Remanso, Playa", "Paulina-Aranda")) %>% 
#   select(census_date, location) %>% 
#   tableNA()

# San Telmo
location.st <- "San Telmo, Punta"


#-------------------------------------------------------------------------------
### Filter by core locations, group/summarise, and complete
complete.fill <- list(
  ad_female_count = 0, ad_male_count = 0, ad_unk_count = 0,
  juv_female_count = 0, juv_male_count = 0, juv_unk_count = 0, 
  pup_live_count = 0, orig_record = FALSE
)

combined.core <- combined.wide %>% 
  filter(location %in% c(locations.core)) %>% 
  mutate(header_id = factor(header_id, levels = combined.header$header_id)) %>%
  # combined.core2 <- filter(location %in% c(locations.core, location.st)) %>% 
  group_by(header_id, species) %>% 
  summarise(time_start = min(time_start), 
            time_end = max(time_end), 
            # n_records = n(), 
            across(ends_with("_count"), \(x) sum(x, na.rm = TRUE)),  
            census_notes = if_else(all(is.na(census_notes)), 
                                   NA_character_,
                                   paste(na.omit(census_notes), 
                                         collapse = "; ")), 
            orig_record = TRUE, 
            .groups = "drop") %>% 
  complete(header_id, species, fill = complete.fill, explicit = FALSE) %>% 
  full_join(combined.header, by = "header_id") %>% 
  mutate(location = "Cape Shirreff core locations", 
         pup_dead_count = if_else(
           census_date_start > as.Date("2012-07-01") & is.na(pup_dead_count), 
           as.integer(0), pup_dead_count), 
         unk_female_count = if_else(
           census_date_start > as.Date("2017-07-01") & is.na(unk_female_count), 
           as.integer(0), unk_female_count), 
         unk_male_count = if_else(
           census_date_start > as.Date("2017-07-01") & is.na(unk_male_count), 
           as.integer(0), unk_male_count), 
         unk_unk_count = if_else(
           census_date_start > as.Date("2014-07-01") & is.na(unk_unk_count), 
           as.integer(0), unk_unk_count)) %>% 
  rowwise() %>%
  mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE)) %>% 
  # total_count_nodead = total_count-pup_dead_count) %>%
  ungroup() %>%
  arrange(census_date_start, species) %>% 
  relocate(season_name:research_program, .after = header_id) %>% 
  relocate(location, .after = species) %>% 
  relocate(total_count, .before = ad_female_count)


#-------------------------------------------------------------------------------
### Filter for san telmo locations, group/summarise, and complete
header.st <- combined.header %>% filter(surveyed_san_telmo)

combined.st <- combined.wide %>% 
  select(-c(season_name, research_program, census_date)) %>% 
  filter(location %in% c(location.st)) %>% 
  mutate(header_id = factor(header_id, levels = header.st$header_id)) %>% 
  complete(header_id, species, 
           fill = c(complete.fill, location = location.st), explicit = FALSE) %>% 
  full_join(header.st, by = "header_id") %>% 
  mutate(pup_dead_count = if_else(
    census_date_start > as.Date("2012-07-01") & is.na(pup_dead_count), 
    as.integer(0), pup_dead_count), 
    unk_female_count = if_else(
      census_date_start > as.Date("2017-07-01") & is.na(unk_female_count), 
      as.integer(0), unk_female_count), 
    unk_male_count = if_else(
      census_date_start > as.Date("2017-07-01") & is.na(unk_male_count), 
      as.integer(0), unk_male_count), 
    unk_unk_count = if_else(
      census_date_start > as.Date("2014-07-01") & is.na(unk_unk_count), 
      as.integer(0), unk_unk_count)) %>% 
  rowwise() %>%
  mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE)) %>% 
  # total_count_nodead = total_count-pup_dead_count) %>%
  ungroup() %>%
  arrange(census_date_start, species) %>% 
  relocate(season_name:research_program, .after = header_id) %>% 
  relocate(location, .after = species) %>% 
  relocate(total_count, .before = ad_female_count)


# TODO: include total_count_nopup?
combined.core.st <- bind_rows(combined.core, combined.st) %>% 
  arrange(census_date_start, species) %>% 
  select(-c(census_date_start:research_program, orig_record))


#-------------------------------------------------------------------------------
### Save data
# TODO: remove header_notes? Nothing in there is relevant to general users
write_csv(
  combined.header, na = "", 
  file = here("data", "cs_combined_data", "cs_phocid_census_header.csv")
)

# TODO: remove census_notes? Not sure anything in here is useful to other users,
#   especially at a 'core census locations' level.
write_csv(
  combined.core.st, na = "", 
  here("data", "cs_combined_data", "cs_phocid_census_records.csv"), 
)


#-------------------------------------------------------------------------------
# Sanity checks
stopifnot(
  sum(is.na(combined.core$header_id)) == 0, 
  sum(is.na(combined.core$species)) == 0, 
  all(combined.header$header_id %in% combined.core$header_id), 
  all(combined.core$header_id %in% combined.header$header_id), 
  nrow(combined.core) == (4 * nrow(combined.header))
)

combined.wide %>% 
  filter(!(location %in% locations.core)) %>% 
  select(location) %>% tableNA()

opportunistic <- combined.wide %>% 
  filter(!(location %in% c(locations.core, location.st))) 
opportunistic %>% 
  summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
  glimpse()
combined.st %>% 
  summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
  glimpse()
combined.core %>% 
  summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
  glimpse()
combined.wide %>% 
  summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
  glimpse()


ggplot(combined.core, aes(census_date_start, total_count)) + 
  geom_point(aes(color = species)) + 
  facet_wrap(vars(season_name), scales = "free_x") + 
  ylim(0, 450) +
  ggtitle("CS core beaches without Punta San Telmo")
ggplot(combined.core.st, aes(census_date_start, total_count)) + 
  geom_point(aes(color = species)) + 
  facet_wrap(vars(season_name), scales = "free_x") + 
  ylim(0, 450) +
  ggtitle("CS core beaches with Punta San Telmo")

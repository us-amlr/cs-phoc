# Combine USAMLR and INACH data, after some processing

library(tidyverse)

tableNA <- function(...) table(..., useNA = 'ifany')

count_summ <- function(x) {
  x %>% 
    group_by(header_id, species) %>% 
    summarise(across(ends_with("_count"), sum), .groups = "drop")
}


#-------------------------------------------------------------------------------
# INACH
#-------------------------------------------------------------------------------
### Read in INACH data, light processing
inach.header <- read.csv("inach_data/phocids_cs_inach_header.csv") %>% 
  mutate(census_date_start = as.Date(census_date_start), 
         census_date_end = as.Date(census_date_end), 
         census_days = 1 + as.numeric(difftime(as.Date(census_date_end), 
                                               as.Date(census_date_start), 
                                               units = "days"))) %>% 
  select(-week)
inach.orig <- read.csv("inach_data/phocids_cs_inach.csv") %>% 
  mutate(pup_live_count = pup_female_count + pup_male_count, 
         # census_date = as.Date(census_date), 
         notes = if_else(pup_live_count > 0, 
                         str_glue("pups: ",
                                  "{pup_female_count} females; ", 
                                  "{pup_male_count} males; ", 
                                  "{pup_unk_count} unknowns"), 
                         NA_character_)) %>% 
  select(-c(week, pup_female_count, pup_male_count, pup_unk_count)) %>% 
  rename(census_notes = notes) %>% 
  left_join(select(inach.header, -c(season_name, census_days)), 
            by = c("header_id"))


# Explore
with(inach.orig %>% 
       filter(location %in% c("Bahamonde", "Nibaldo", 
                              "Cerro Gajardo, Peninsula", "Cape Shirreff")), 
     tableNA(location, season_name))


# TODO: keep this?
### Aggregate Paso Ancho and Media Luna records
inach <- inach.orig %>% 
  mutate(location_group = case_when(
    location == "Paso Ancho" ~ "Media Luna", 
    TRUE ~ location)
  ) %>% 
  group_by(header_id, location_group, species) %>% 
  summarise(across(ends_with("_count"), sum), 
            across(
              c(research_program, season_name, surveyed_san_telmo, 
                census_date_start, census_date_end, census_date), 
              unique), 
            #confirmed that all Paso Ancho notes are NA
            census_notes = if (all(is.na(census_notes))) NA_character_ else 
              as.character(na.omit(census_notes)),
            .groups = "drop") %>% 
  rename(location = location_group) %>% 
  select(!!names(inach.orig))

# Sanity check
d1 <- count_summ(inach.orig)
d2 <- count_summ(inach)
waldo::compare(d1, d2); rm(d1, d2)



#-------------------------------------------------------------------------------
# AMLR
#-------------------------------------------------------------------------------
### Read in AMLR data, light processing. Add explicit 0 records below
amlr.header <- read.csv("amlr_data/phocids_cs_amlr_header.csv") %>% 
  mutate(header_id = as.character(header_id))
amlr.orig <- read.csv("amlr_data/phocids_cs_amlr.csv") %>% 
  mutate(header_id = as.character(header_id), 
         census_date = as.Date(census_date), 
         research_program = "USAMLR")

### Aggregate up to location_group level
names.agg <- setdiff(
  names(amlr.orig), 
  c("location_group", "observer", "census_id", 
    "total_count", "total_count_nodead")
)
amlr.agg <- amlr.orig %>% 
  select(-c(observer, total_count, total_count_nodead)) %>% 
  group_by(header_id, location_group, species) %>% 
  summarise(across(ends_with("_count"), sum), 
            across(
              c(research_program, season_name, surveyed_san_telmo, 
                census_date_start, census_date_end, census_date, 
                header_notes), 
              unique), 
            #suppressWarnings is for message from min(NA)
            time_start = suppressWarnings(min(time_start, na.rm = TRUE)), 
            time_end = suppressWarnings(min(time_end, na.rm = TRUE)), 
            # notes_count = sum(!is.na(census_notes)), 
            #there are no groups where multiple notes need to be concatenated
            census_notes = if (all(is.na(census_notes))) NA_character_ else
              as.character(na.omit(census_notes)),
            .groups = "drop") %>% 
  rename(location = location_group) %>%
  select(!!names.agg)


# Sanity checks
d1 <- count_summ(amlr.agg)
d2 <- count_summ(amlr.agg)
waldo::compare(d1, d2); rm(d1, d2)
# #doesn't add up because there aren't necessarily multiple records, fine
# nrow(amlr.orig %>% filter(location != location_group)) 
#no time NA
amlr.agg %>% filter(is.na(time_start) | is.na(time_end))


# Explore
# #reminder that there are a few instances of folks putting 0 records for beaches,
# #which will mean that these 0s will have exact times in explicit data
# amlr0 <- amlr.orig %>%
#   rowwise() %>%
#   mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), 
#                            na.rm = TRUE)) %>%
#   filter(total_count == 0)



#-------------------------------------------------------------------------------
### 'Complete' west coast beaches for AMLR data. Use location

#------------------------------------------------
## prep
# Function for AMLR explicit 0s: for new records, makes columns 0 as appropriate
func_amlr_mgmt <- function(x) {
  x %>% 
    mutate(research_program = "USAMLR", 
           pup_dead_count = if_else(
             census_date > as.Date("2012-07-01") & is.na(pup_dead_count), 
             as.integer(0), pup_dead_count), 
           unk_female_count = if_else(
             census_date > as.Date("2017-07-01") & is.na(unk_female_count), 
             as.integer(0), unk_female_count), 
           unk_male_count = if_else(
             census_date > as.Date("2017-07-01") & is.na(unk_male_count), 
             as.integer(0), unk_male_count), 
           unk_unk_count = if_else(
             census_date > as.Date("2014-07-01") & is.na(unk_unk_count), 
             as.integer(0), unk_unk_count)) %>% 
    # select(!!names(amlr.agg)) %>% #for ordering, note this removes census_days
    select(header_id, season_name, census_date, time_start, time_end, 
           location, species, ends_with("_count"), 
           census_notes, header_notes, research_program, orig_record) %>% 
    arrange(census_date, location, species)
}

# Fill list to pass to complete. The rest of the count columns 
#   depend on the season; see func_amlr_mgmt
complete.fill <- list(
  ad_female_count = 0, ad_male_count = 0, ad_unk_count = 0,
  juv_female_count = 0, juv_male_count = 0, juv_unk_count = 0, 
  pup_live_count = 0
)



# 'regular' beaches, to use to complete
location.regular.nwc <- c(
  "Media Luna", "Yuseff, Punta", "Larga", "Marko", "Daniel", "Modulo", 
  "Hue", "Copi", "Maderas", "Cachorros", "Chungungo", 
  "Ballena Sur", "Ballena Norte", "Bahamonde", "Nibaldo", "Roquerio", 
  "Alcazar", "Pinochet de la Barra", "Papua", "Antartico, Playa", 
  "Loberia", "Yamana, Playa"
)

# west coast beaches, aka those in aggregate locations
location.regular.wc <- c(
  "Golondrina, Playa", "del Lobero, Playa", "Paulina, Playa", 
  "Schiappacasse, Playa", "El Plastico, Playa", "Leopard Beach", 
  "del Canal, Playa"
)

location.regular.wc.agg <- c("Golondrina-del Lobero", "Paulina-Aranda")

# San Telmo
location.st <- "San Telmo, Punta"



# Make 'pre' data frames for adding explicit 0s
amlr.agg.pre <- amlr.agg %>%
  select(-c(season_name:surveyed_san_telmo, header_notes, 
            time_start, time_end)) %>% 
  mutate(orig_record = TRUE)

amlr.header.pre <- amlr.header %>% 
  select(header_id, season_name, census_date_end, header_notes) %>% 
  mutate(census_date_end = as.Date(census_date_end))



# Made data frame with date for each header/location combo
amlr.agg.date <- amlr.agg %>% 
  select(header_id, location, census_date_loc = census_date) %>% 
  distinct(header_id, location, .keep_all = TRUE)

# Made data frame with times for each date/location
#   If multiple times for date/location, this keeps the first record
amlr.agg.time <- amlr.agg %>% 
  select(census_date, location, time_start, time_end) %>% 
  distinct(census_date, location, .keep_all = TRUE)

# Generate data frame that has earliest/latest times, by day. 
#   To join with completed data to at least give time range for 0s
amlr.time.minmax <- amlr.orig %>% 
  group_by(census_date) %>% 
  summarise(time_start_min = suppressWarnings(min(time_start, na.rm = TRUE)), 
            time_end_max = suppressWarnings(max(time_end, na.rm = TRUE)), 
            .groups = "drop")

func_amlr_time_minmax <- function(x, y) {
  x %>% 
    left_join(y, by = "census_date") %>% 
    mutate(time_start = if_else(is.na(time_start), time_start_min, time_start), 
           time_end = if_else(is.na(time_end), time_end_max, time_end))
}


#------------------------------------------------
## For records with combined west coast locations
amlr.a.raw <- amlr.agg.pre %>% 
  filter(location %in% location.regular.wc.agg) 

# # Checks
# #Confirmed that both beaches and all 4 species are present here
# table(amlr.a.raw$location); table(amlr.a.raw$species)
# #Confirmed that header ID and date matches are all distinct
# all.equal(
#   length(unique(amlr.a.raw$header_id)), 
#   amlr.a.raw %>% select(header_id, census_date) %>% 
#     distinct() %>% nrow()
# )
# #Confirmed that location_group and location are identical
# #now not relevant after aggregating
# table(amlr.a.raw$location_group, amlr.a.raw$location)

# Complete data frame
#   Note for these records header_id and census_date are 
#     a 1:1 match, so we can nest header and census_date.
#     This is recommended here based on the assumption that the 
#     whole west coast was surveyed on the same day.
#   Number of expected records: 952 
#     (2*4*n_distinct(amlr.a.raw$header_id))
amlr.a.new <- amlr.a.raw %>% 
  complete(nesting(header_id, census_date), location, species, 
           fill = complete.fill, explicit = FALSE) %>% 
  left_join(amlr.header.pre, by = "header_id") %>%
  left_join(amlr.agg.time, by = c("census_date", "location")) %>%
  func_amlr_time_minmax(amlr.time.minmax) %>%
  func_amlr_mgmt()

# Sanity check that no counts were added and alld ates are ok
d1 <- count_summ(amlr.a.raw) %>% filter(if_any(ends_with("_count"), ~.x != 0))
d2 <- count_summ(amlr.a.new) %>% filter(if_any(ends_with("_count"), ~.x != 0))
waldo::compare(d1, d2); rm(d1, d2)
stopifnot(
  0 == (amlr.a.new %>% filter(is.na(census_date)) %>% nrow())
)

# Clean up
rm(amlr.a.raw)

# TODO: Issue 56p
View(
  amlr.a.new %>% 
    group_by(season_name) %>% 
    summarise(across(ad_female_count:unk_unk_count, function(i) sum(is.na(i))))
)


#------------------------------------------------
## For west coast records without combined beaches
amlr.b.raw <- amlr.agg.pre %>% 
  filter(location %in% location.regular.wc)

# # Checks
# #Confirmed that all 7 beaches and all 4 species are present here
# table(amlr.b.raw$location); table(amlr.b.raw$species)
# #Confirmed that header ID and date matches are all distinct
# all.equal(
#   length(unique(amlr.b.raw$header_id)),
#   amlr.b.raw %>% select(header_id, census_date) %>%
#     distinct() %>% nrow()
# )

# Complete data frame
#   Note for these records header_id and census_date are 
#     a 1:1 match, so we can nest header and census_date.
#     Like above, this is recommended here based on the assumption that the 
#     whole west coast was surveyed on the same day.
#     nrow(amlr.b.raw %>% distinct(header_id, census_date))
#   Number of expected records: 504 
#     (length(location.regular.wc)*4*n_distinct(amlr.b.raw$header_id))
amlr.b.new <- amlr.b.raw %>% 
  complete(nesting(header_id, census_date), location, species, 
           fill = complete.fill, explicit = FALSE) %>% 
  left_join(amlr.header.pre, by = "header_id") %>%
  left_join(amlr.agg.time, by = c("census_date", "location")) %>%
  func_amlr_time_minmax(amlr.time.minmax) %>% 
  func_amlr_mgmt()

# Sanity check that no counts were added
d1 <- count_summ(amlr.b.raw) %>% filter(if_any(ends_with("_count"), ~.x != 0))
d2 <- count_summ(amlr.b.new) %>% filter(if_any(ends_with("_count"), ~.x != 0))
waldo::compare(d1, d2); rm(d1, d2)

# Clean up
rm(amlr.b.raw)


#------------------------------------------------
## 'Complete' all other (non-west coast) regular beaches (Yamana to Media Luna)
amlr.c.raw <- amlr.agg %>% 
  filter(location %in% location.regular.nwc)

# Complete data frame
#   Number of expected records: 15312 
#     length(location.regular.nwc)*4*n_distinct(amlr.c.raw$header_id)
amlr.c.new <- amlr.c.raw %>% 
  select(-c(season_name:surveyed_san_telmo, header_notes), 
         -c(time_start, time_end)) %>% 
  complete(header_id, location, species, 
           fill = complete.fill, explicit = FALSE) %>% 
  # TODO: finalize questions about if location_group should be 
  #   only location that is included
  left_join(rename(amlr.header, header_notes = census_phocid_header_notes),
            by = "header_id") %>%
  # TODO: confirm we want to do this
  #end date is consistent with how census_date was assigned on import (see header notes)
  mutate(census_date = if_else(is.na(census_date), 
                               as.Date(census_date_end), census_date)) %>% 
  left_join(amlr.agg.time, by = c("census_date", "location")) %>%
  func_amlr_time_minmax(amlr.time.minmax) %>% 
  func_amlr_mgmt() %>% 
  arrange(census_date, location, species, time_start, time_end)

# Sanity check that no counts were added
d1 <- count_summ(amlr.c.raw) %>% filter(if_any(ends_with("_count"), ~.x != 0))
d2 <- count_summ(amlr.c.new) %>% filter(if_any(ends_with("_count"), ~.x != 0))
waldo::compare(d1, d2); rm(d1, d2)

# Sanity check that all times got values
with(amlr.c.new, tableNA(season_name, is.na(time_start)))

# Clean up
rm(amlr.c.raw)


#------------------------------------------------
## 'Complete' San Telmo records
amlr.d.raw <- amlr.agg %>% 
  filter(location %in% location.st)

# Sanity check - no san telmo records when Boolean is FALSE
amlr.header %>%
  filter(!surveyed_san_telmo,
         (header_id %in% amlr.d.raw$header_id)) %>% 
  nrow()

# Make dummy records for header records with no corresponding data records
header.needed <- amlr.header %>% 
  filter(surveyed_san_telmo, 
         !(header_id %in% amlr.d.raw$header_id)) %>%
  rename(header_notes = census_phocid_header_notes) %>% 
  select(-census_days) %>% 
  mutate(census_date = as.Date(census_date_end), 
         location = location.st, species = "Elephant seal", 
         ad_female_count = 0, ad_male_count = 0, ad_unk_count = 0, 
         juv_female_count = 0, juv_male_count = 0, juv_unk_count = 0, 
         pup_live_count = 0, research_program = "USAMLR")

stopifnot( #confirm which count columns are 0 vs NA
  all(as.Date(header.needed$census_date_start) < as.Date("2012-07-01"))
)


# Complete data frame
#   Add header needed records 
#   Number of expected records: 672 
#     4*(n_distinct(amlr.d.raw$header_id)+nrow(header.needed))
#     1 beach (length())
amlr.d.new <- amlr.d.raw %>% 
  bind_rows(header.needed) %>% 
  select(-c(season_name:surveyed_san_telmo, header_notes), 
         -c(time_start, time_end)) %>% 
  complete(header_id, location, species, 
           fill = complete.fill, explicit = FALSE) %>% 
  left_join(rename(amlr.header, header_notes = census_phocid_header_notes),
            by = "header_id") %>%
  # TODO: confirm we want to do this
  #end date is consistent with how census_date was assigned on import (see header notes)
  mutate(census_date = if_else(is.na(census_date), 
                               as.Date(census_date_end), census_date)) %>% 
  left_join(amlr.agg.time, by = c("census_date", "location")) %>%
  func_amlr_time_minmax(amlr.time.minmax) %>% 
  func_amlr_mgmt() %>% 
  arrange(census_date, location, species, time_start, time_end)



#-------------------------------------------------------------------------------
### Complete AMLR dataframe
amlr.opportunistic <- amlr.agg %>% 
  filter(!(location %in% c(location.regular.wc, location.regular.nwc, 
                           location.regular.wc.agg)))

amlr <- bind_rows(amlr.a.new, amlr.b.new, amlr.c.new, amlr.opportunistic) %>% 
  arrange(census_date, location, species)

#-------------------------------------------------------------------------------
# Combined
#-------------------------------------------------------------------------------
### Combine data
y.header <- bind_rows(amlr.header, inach.header) %>% 
  arrange(census_date_start)

y <- bind_rows(amlr, inach) %>% 
  arrange(census_date, species, location) %>% 
  rowwise() %>% 
  mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE), 
         total_count_nodead = total_count-pup_dead_count) %>% 
  ungroup()


### Write to CSV
write.csv(y, row.names = FALSE, na = "", 
          file = "cs_combined_data/phocids_cs_combined.csv")
write.csv(y.header, row.names = FALSE, na = "", 
          file = "cs_combined_data/phocids_cs_combined_header.csv")


#-------------------------------------------------------------------------------
# Explore
tableNA(y.header$census_days)


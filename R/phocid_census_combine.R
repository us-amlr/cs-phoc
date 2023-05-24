# Combine raw USAMLR and INACH data, after some processing

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
library(tidyverse)
library(here)
library(amlrPinnipeds)
library(waldo)

# Get sums of each count for two data frames, to confirm numbers are consistent
count_summ <- function(i, nozero = FALSE) {
  tmp <- i %>% 
    group_by(header_id, species) %>% 
    summarise(across(ends_with("_count"), sum), .groups = "drop")
  
  if (nozero) {
    tmp %>% 
      mutate(across(ends_with("_count"), ~ replace_na(.x, 0))) %>% 
      rowwise() %>% 
      filter(!all(c_across(ends_with("_count")) == 0)) %>% 
      ungroup()
  } else {
    tmp
  }
}

# Wrapper to quickly compare count_summ output
count_compare <- function(x, y, nozero = FALSE) {
  waldo::compare(count_summ(x, nozero), count_summ(y, nozero))
}

sum_all_counts <- function(i.df) {
  i.df %>% 
    summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
    sum()
}



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
# Sanity check: `count_compare(inach.orig, inach)`



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# US AMLR
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### Read in AMLR data from database, do light processing. 
# Explicit 0 records are added below
con <- amlr_dbConnect(Database="***REMOVED***")

### Header
amlr.header.orig <- tbl(con, "vCensus_Phocid_Header") %>% 
  select(season_name, census_date_start, census_date_end, census_days, 
         surveyed_san_telmo, 
         header_notes = notes, header_id = census_phocid_header_id) %>% 
  arrange(season_name, census_date_start) %>% 
  collect()

write.csv(amlr.header.orig, row.names = FALSE,
          file = here("data", "amlr_data", "phocids_cs_amlr_header.csv"))

amlr.header <- amlr.header.orig %>% 
  mutate(header_id = as.character(header_id), 
         research_program = "USAMLR")


### Census data
amlr.orig <- tbl(con, "vCensus_Phocid") %>% 
  arrange(season_name, census_date, species, location_group) %>% 
  rename(header_id = census_phocid_header_id) %>% 
  collect() %>% 
  rowwise() %>% 
  mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE), 
         total_count_nodead = sum(c_across(c(ad_female_count:pup_live_count, 
                                             unk_female_count:unk_unk_count)), 
                                  na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(season_name, census_date_start, census_date_end, surveyed_san_telmo, 
         observer:location_group, species, 
         total_count, total_count_nodead, ad_female_count:unk_unk_count, 
         header_notes, census_notes, header_id, census_id)

write.csv(amlr.orig, row.names = FALSE,
          file = here("data", "amlr_data", "phocids_cs_amlr.csv"))


# Aggregate up to location_group level
names.agg <- setdiff(
  names(amlr.orig), 
  c("location_group", "observer", "census_id", 
    "total_count", "total_count_nodead")
)
amlr.agg <- amlr.orig %>% 
  mutate(header_id = as.character(header_id)) %>% 
  select(-c(observer, total_count, total_count_nodead)) %>% 
  group_by(header_id, location_group, species) %>% 
  summarise(across(ends_with("_count"), sum), 
            across(c(season_name, surveyed_san_telmo, 
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


# Sanity checks - all ok
stopifnot(
  nrow(amlr.agg %>% filter(is.na(time_start) | is.na(time_end))) == 0, 
  all.equal(
    amlr.orig %>% 
      mutate(header_id = as.character(header_id)) %>% select(-total_count) %>% 
      count_summ(), 
    count_summ(amlr.agg)
  ), 
  sum(amlr.orig$total_count) == sum_all_counts(amlr.agg)
)


# Explore
# nrow(amlr.orig %>% filter(location != location_group)) 
# amlr0 <- amlr.orig %>%
#   rowwise() %>%
#   mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), 
#                            na.rm = TRUE)) %>%
#   filter(total_count == 0)
# ^ is a reminder that there are a few instances of entered 0 records for 
#   beaches,which will mean that these 0s will have exact times in explicit data




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### 'Complete' AMLR data with explicit zero records

#-------------------------------------------------------------------------------
## prep

# Fill list to pass to complete. The rest of the count columns 
#   depend on the season; see func_amlr_explicit
complete.fill <- list(
  ad_female_count = 0, ad_male_count = 0, ad_unk_count = 0,
  juv_female_count = 0, juv_male_count = 0, juv_unk_count = 0, 
  pup_live_count = 0, orig_record = FALSE
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
  select(-c(season_name:surveyed_san_telmo, #header_notes, 
            time_start, time_end)) %>% 
  mutate(orig_record = TRUE)

amlr.header.pre <- amlr.header %>% 
  select(header_id, season_name, census_date_end)



# Made data frame with date for each header/location combo
amlr.agg.date <- amlr.agg %>% 
  select(header_id, location, census_date_loc = census_date) %>% 
  distinct(header_id, location, .keep_all = TRUE)

# Made data frame with times for each date/location
#   If multiple times for date/location, this keeps the first record
amlr.agg.time <- amlr.agg %>% 
  select(census_date, location, time_start, time_end) %>% 
  distinct(census_date, location, .keep_all = TRUE)

# amlr.agg.time2 <- amlr.agg %>% 
#   select(census_date, location, time_start, time_end) %>%
#   distinct() %>%
#   group_by(census_date, location) %>% 
#   filter(n() > 1)

# Generate data frame that has earliest/latest times, by day. 
#   To join with completed data to at least give time range for 0s
amlr.time.minmax <- amlr.orig %>% 
  group_by(census_date) %>% 
  summarise(time_start_min = suppressWarnings(min(time_start, na.rm = TRUE)), 
            time_end_max = suppressWarnings(max(time_end, na.rm = TRUE)), 
            .groups = "drop")


# Add necessary columns to ...header.needed data frames
func_mutate_header_needed <- function(x) {
  x %>% 
    mutate(ad_female_count = 0, ad_male_count = 0, ad_unk_count = 0, 
           juv_female_count = 0, juv_male_count = 0, juv_unk_count = 0, 
           pup_live_count = 0, orig_record = FALSE) %>% 
    mutate(across(ends_with("_count"), as.integer))
}


# Function for steps for making AMLR data explicit that apply to all pieces: 
#   join in and do time things, make count columns 0 as appropriate
#   do time things means: join data frame with date/location/times to get 
#     most precise start/end times. Then join in minmax time data frame 
#     (min time_start and max time_end for each census day) 
#     to fill in remaining time NAs
func_amlr_explicit <- function(x) {
  x %>%
    left_join(amlr.agg.time, by = c("census_date", "location")) %>%
    left_join(amlr.time.minmax, by = "census_date") %>% 
    mutate(time_start = if_else(is.na(time_start), time_start_min, time_start), 
           time_end = if_else(is.na(time_end), time_end_max, time_end)) %>% 
    mutate(pup_dead_count = if_else(
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
    select(header_id, season_name, census_date, time_start, time_end, 
           location, species, ends_with("_count"), 
           census_notes, orig_record) %>% 
    arrange(census_date, location, species)
}



#-------------------------------------------------------------------------------
#------------------------------------------------
## For records with combined west coast locations
amlr.a.raw <- amlr.agg.pre %>% 
  filter(location %in% location.regular.wc.agg) 

# Get all header records for this time period
summary(amlr.a.raw$census_date)
amlr.a.header <- amlr.header %>% 
  filter(census_date_start < "2018-07-01")

amlr.a.header.needed <- amlr.a.header %>% 
  filter(!(header_id %in% amlr.a.raw$header_id)) %>% 
  # TODO: confirm we want to do dates this way
  # Need values/0s so as to not introduce NAs
  mutate(census_date = as.Date(census_date_end), 
         # all locations and species are accounted for in raw df
         location = location.regular.wc.agg[1], 
         species = "Elephant seal") %>% 
  func_mutate_header_needed() %>% 
  select(!!intersect(names(amlr.a.raw), names(.)))


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
#   Note for these records header_id and census_date are a 1:1 match, 
#     so we can nest header and census_date. We do this from 
#     the assumption that the whole west coast was surveyed on the same day.
#   Number of expected records: 1168 
#     (2*4*n_distinct(amlr.a.header$header_id))
amlr.a.new <- amlr.a.raw %>% 
  bind_rows(amlr.a.header.needed) %>% 
  complete(nesting(header_id, census_date), location, species, 
           fill = complete.fill, explicit = FALSE) %>% 
  left_join(amlr.header.pre, by = "header_id") %>%
  func_amlr_explicit()

# Sanity check that no counts were added
count_compare(amlr.a.raw, amlr.a.new, nozero = TRUE)

# Clean up
rm(amlr.a.raw, amlr.a.header, amlr.a.header.needed)


#------------------------------------------------
## For west coast records without combined beaches
amlr.b.raw <- amlr.agg.pre %>% 
  filter(location %in% location.regular.wc)

# Get all header records for this time period
summary(amlr.b.raw$census_date)
amlr.b.header <- amlr.header %>% 
  filter(census_date_start > "2018-07-01")

amlr.b.header.needed <- amlr.b.header %>% 
  filter(!(header_id %in% amlr.b.raw$header_id)) %>% 
  mutate(census_date = as.Date(census_date_end), 
         location = location.regular.wc[1], species = "Elephant seal") %>% 
  func_mutate_header_needed() %>% 
  select(!!intersect(names(amlr.b.raw), names(.)))

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
#   Number of expected records: 784
#     (length(location.regular.wc)*4*n_distinct(amlr.b.header$header_id))
amlr.b.new <- amlr.b.raw %>% 
  bind_rows(amlr.b.header.needed) %>% 
  complete(nesting(header_id, census_date), location, species, 
           fill = complete.fill, explicit = FALSE) %>% 
  left_join(amlr.header.pre, by = "header_id") %>% 
  func_amlr_explicit()

# Sanity check that no counts were added
count_compare(amlr.b.raw, amlr.b.new, nozero = TRUE)

# Clean up
rm(amlr.b.raw, amlr.b.header, amlr.b.header.needed)


#------------------------------------------------
## 'Complete' all other (non-west coast) regular beaches (Yamana to Media Luna)
amlr.c.raw <- amlr.agg.pre %>% 
  filter(location %in% location.regular.nwc)

# Get all header records for this time period - none needed to be added
stopifnot(
  0 == (amlr.header %>% 
          filter(!(header_id %in% amlr.c.raw$header_id)) %>% 
          nrow())
)

# Complete data frame
#   Number of expected records: 15312 
#     length(location.regular.nwc)*4*n_distinct(amlr.header$header_id)
amlr.c.new <- amlr.c.raw %>% 
  complete(header_id, location, species, 
           fill = complete.fill, explicit = FALSE) %>% 
  # End date catch-all is consistent with date assignment on Excel data import, 
  #   although this means there (likely) will be some that are off by one day
  #   For instance, Marko for header_id 10. 
  #   6708 records (174 unique headers) would be NA, without using end date rule
  left_join(amlr.agg.date, by = c("header_id", "location")) %>%
  left_join(amlr.header.pre, by = "header_id") %>%
  mutate(census_date_orig = census_date, 
         census_date = if_else(is.na(census_date), census_date_loc, census_date), 
         census_date = if_else(is.na(census_date), census_date_end, census_date)) %>% 
  # select(census_date_orig, census_date, census_date_loc, census_date_end, 
  #        everything())
  func_amlr_explicit() 

# Sanity check that no counts were added, and that all times got values
count_compare(amlr.c.raw, amlr.c.new, nozero = TRUE)
with(amlr.c.new, tableNA(season_name, is.na(time_start)))

# Clean up
rm(amlr.c.raw)


#------------------------------------------------
## 'Complete' San Telmo records
amlr.d.raw <- amlr.agg.pre %>% 
  filter(location %in% location.st)

# Sanity check - no san telmo records when Boolean is FALSE
stopifnot(
  0 == (amlr.header %>%
          filter(!surveyed_san_telmo, (header_id %in% amlr.d.raw$header_id)) %>% 
          nrow())
)

# Make dummy records for header records with no corresponding data records
amlr.d.header <- amlr.header %>% 
  filter(surveyed_san_telmo)

amlr.d.header.needed <- amlr.d.header %>% 
  filter(!(header_id %in% amlr.d.raw$header_id)) %>%
  mutate(census_date = as.Date(census_date_end), 
         location = location.st, species = "Elephant seal") %>% 
  func_mutate_header_needed() %>% 
  select(!!intersect(names(amlr.d.raw), names(.)))


# Complete data frame
#   Add header needed records 
#   This is for one beach, so like W coast can nest header ID and census date
#   Number of expected records: 672 
#     1*4*n_distinct(amlr.d.header$header_id)
#     (1 beach: length(location.st))
amlr.d.new <- amlr.d.raw %>% 
  bind_rows(amlr.d.header.needed) %>% 
  complete(nesting(header_id, census_date), location, species, 
           fill = complete.fill, explicit = FALSE) %>% 
  left_join(amlr.header.pre, by = "header_id") %>%
  func_amlr_explicit()

# Sanity check that no counts were added
count_compare(amlr.d.raw, amlr.d.new, nozero = TRUE)

# Clean up 
rm(amlr.d.raw, amlr.d.header, amlr.d.header.needed)



#-------------------------------------------------------------------------------
### Complete AMLR dataframe
amlr.opportunistic <- amlr.agg %>% 
  select(-c(season_name:surveyed_san_telmo, header_notes)) %>% 
  mutate(orig_record = TRUE) %>% 
  filter(!(location %in% c(location.regular.wc, location.regular.nwc, 
                           location.regular.wc.agg, location.st))) %>% 
  left_join(amlr.header.pre, by = "header_id") %>%
  select(!!names(amlr.a.new))


amlr <- bind_rows(amlr.a.new, amlr.b.new, amlr.c.new, amlr.d.new, 
                  amlr.opportunistic) %>%
  mutate(research_program = "USAMLR") %>% 
  arrange(census_date, location, species)
# left_join(select(amlr.header, header_id, census_date_start, census_date_end), 
#           by = "header_id") %>% 
# mutate(census_date_start = as.Date(census_date_start), 
#        census_date_end = as.Date(census_date_end))

# Sanity check that no counts were added, and that dates and times are ok
d1 <- amlr.orig %>% 
  mutate(header_id = as.character(header_id)) %>%  select(-total_count) %>% 
  count_summ(nozero = TRUE)
d2 <- count_summ(amlr, nozero = TRUE)
count_compare(d1, d2, nozero = TRUE)

stopifnot(
  0 == sum(is.na(amlr$census_date)), 
  0 == sum(is.na(amlr$time_start)), 
  0 == sum(is.na(amlr$time_end))
)
rm(d1, d2, amlr.a.new, amlr.b.new, amlr.c.new, amlr.d.new, amlr.opportunistic)




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Combined
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### Combine data
combined.header <- amlr.header %>% 
  bind_rows(inach.header) %>% 
  relocate(header_id) %>% 
  arrange(census_date_start)

combined.wide <- bind_rows(amlr, inach) %>%
  relocate(orig_record, .after = last_col())%>% 
  rowwise() %>% 
  mutate(total_count = sum(c_across(ad_female_count:unk_unk_count), 
                           na.rm = TRUE)) %>% 
  # total_count_nodead = total_count-pup_dead_count) %>% 
  ungroup() %>% 
  arrange(census_date, tolower(location), species) 



### Checks
tableNA(combined.header$season_name)
tableNA(combined.wide$season_name)

stopifnot(all(combined.header$research_program %in% c("INACH", "USAMLR")))

combined.join <- combined.wide %>% 
  #confirmed if this is commented then season_name.x and .y are equivalent
  select(-season_name) %>%
  left_join(combined.header, by = "header_id")

with(combined.wide, stopifnot(
  all(combined.header$research_program %in% c("INACH", "USAMLR")), 
  all(combined.header$header_id %in% header_id), 
  all(header_id %in% combined.header$header_id), 
  
  with(combined.join, all(pmap_lgl(
    list(census_date, census_date_start, census_date_end), 
    function(i, j, k) between(i, j, k)
  ))), 
  
  # all(research_program %in% c("INACH", "USAMLR")), 
  all(species %in% 
        paste(c("Crabeater", "Elephant", "Leopard", "Weddell"), "seal")), 
  
  sum(is.na(census_date)) == 0, 
  #times only apply to amlr data - checked above
  
  all(!is.na(ad_female_count)), 
  all(!is.na(ad_male_count)), 
  #`census_date > as.Date("2009-07-01")` is equivalent to 'USAMLR' filter
  all(!is.na(ad_unk_count[census_date > as.Date("2009-07-01")])),
  # all(!is.na(juv_female_count[research_program == "USAMLR"])), 
  # all(!is.na(juv_male_count[research_program == "USAMLR"])), 
  # all(!is.na(juv_unk_count[research_program == "USAMLR"])), 
  all(!is.na(pup_live_count[census_date > as.Date("2009-07-01")])), 
  
  all(is.na(pup_dead_count[census_date < as.Date("2012-07-01")])), 
  all(is.na(unk_female_count[census_date < as.Date("2017-07-01")])), 
  all(is.na(unk_male_count[census_date < as.Date("2017-07-01")])), 
  all(is.na(unk_unk_count[
    census_date < as.Date("2014-07-01") & census_date > as.Date("2009-07-01")
  ])), 
  
  all(!is.na(pup_dead_count[census_date > as.Date("2012-07-01")])), 
  all(!is.na(unk_female_count[census_date > as.Date("2017-07-01")])), 
  all(!is.na(unk_male_count[census_date > as.Date("2017-07-01")])), 
  all(!is.na(unk_unk_count[
    census_date > as.Date("2014-07-01") & census_date > as.Date("2009-07-01")
  ])), 
  
  sum_all_counts(combined.wide %>% select(-total_count)) == 
    (sum_all_counts(inach.orig) + 
       sum_all_counts(amlr.orig %>% select(-total_count)))
  
))


### Make long data
combined.long <- combined.wide %>% 
  select(-c(total_count)) %>% #, total_count_nodead)) %>%
  pivot_longer(ends_with("count"), 
               names_to = "age_class_sex", values_to = "count") %>% 
  filter(!is.na(count)) %>% 
  mutate(acs_split = str_split(age_class_sex, "_"), 
         age_class_pre = map_chr(acs_split, c(1)), 
         sex_pre = map_chr(acs_split, c(2)), 
         age_class = case_when(
           age_class_pre ==  "ad" ~ "Adult", 
           age_class_pre ==  "juv" ~ "Juvenile", 
           age_class_pre ==  "pup" & sex_pre == "live" ~ "Pup-live", 
           age_class_pre ==  "pup" & sex_pre == "dead" ~ "Pup-dead", 
           age_class_pre ==  "unk" ~ "Unknown"
         ), 
         sex = if_else(sex_pre %in% c("F", "M", "U"), 
                       sex_pre, NA_character_)) %>% 
  relocate(age_class, sex, count, .after = "species") %>% 
  select(-c(age_class_sex, acs_split, age_class_pre, sex_pre))


### Write to CSV
path.combined <- here("data", "cs_combined_data")
write.csv(combined.header, row.names = FALSE, na = "",
          file = here(path.combined, "phocids_cs_combined_header.csv"))
write.csv(combined.wide, row.names = FALSE, na = "",
          file = here(path.combined, "phocids_cs_combined_wide.csv"))
write.csv(combined.long, row.names = FALSE, na = "",
          file = here(path.combined, "phocids_cs_combined_long.csv"))


#-------------------------------------------------------------------------------
### Explore
# inach %>%
#   group_by(species, season_name) %>%
#   summarise(across(ends_with("_count"), function(i) sum(!is.na(i)))) %>%
#   View()

tableNA(combined.header$census_days)
tableNA(combined.header$season_name, combined.header$census_days)

census.multdays <- combined.wide |> 
  filter(header_id %in% (combined.header |> filter(census_days > 1) |> pull(header_id)), 
         total_count > 1)

tableNA(census.multdays$season_name)

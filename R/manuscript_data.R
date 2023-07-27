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
con <- amlr_dbConnect(Database="***REMOVED***")

cspc.header.orig <- tbl(con, "vCensus_Phocid_Header") %>%
  arrange(census_date_start) %>% 
  collect() %>% 
  group_by(season_name) %>% 
  mutate(season_idx = seq_along(census_date_start), 
         header_id = paste(season_name, 
                           str_pad(season_idx, width = 2, pad = "0"), 
                           sep = "-")) %>% 
  ungroup() %>% 
  select(header_id, census_phocid_header_id, season_name, 
         census_date_start, census_date_end, census_days, 
         surveyed_san_telmo, research_program)

cspc.header <- cspc.header.orig %>% select(-census_phocid_header_id)

stopifnot(
  nrow(cspc.header) == nrow(collect(tbl(con, "census_phocid_header")))
)


cspc.wide <- tbl(con, "vCensus_Phocid") %>% 
  arrange(census_date, species, location_group) %>% 
  collect() %>% 
  left_join(select(cspc.header.orig, header_id, census_phocid_header_id), 
            by = "census_phocid_header_id") %>% 
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
# lapply(cspc.wide, function(i) {
#   tableNA(amlr_season_from_date(cspc.wide$census_date), is.na(i))
# })
# all(cspc.wide$header_id %in% cspc.header$header_id)
# tableNA(cspc.wide$location)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# General variables and functions 

### Core Locations
# TODO: move to amlrPinnipeds as data variable
cspc.locations.core <- c(
  "Media Luna", "Yuseff, Punta", "Larga", "Marko", "Daniel", "Modulo", 
  "Hue", "Copi", "Maderas", "Cachorros", "Chungungo", 
  "Ballena Sur", "Ballena Norte", "Bahamonde", "Nibaldo", "Roquerio", 
  "Alcazar", "Pinochet de la Barra", "Papua", "Antartico, Playa", 
  "Loberia", "Yamana, Playa", 
  "Golondrina, Playa", "del Lobero, Playa", "Paulina, Playa", 
  "Schiappacasse, Playa", "El Plastico, Playa", "Leopard Beach", 
  "del Canal, Playa", "Aranda", "El Remanso, Playa", 
  "Golondrina-del Lobero", "Paulina-Aranda", 
  "Cape Shirreff", "Copihue", "Cerro Gajardo, Peninsula"
)

# San Telmo
cspc.location.pst <- "San Telmo, Punta"


### Functions used in combining
# TODO: move to amlrPinnipeds?
cspc_amlr_complete <- function(x, x.header, fill.location = NULL) {
  # Header mgmt
  x.header <- x.header %>% filter(research_program == "USAMLR")
  
  header.id.class <- class(x.header$header_id)
  as_header_id_func <- if (header.id.class == "character") {
    as.character
  } else if (header.id.class == "integer") {
    as.integer
  } else {
    stop("Invalid header_id type")
  }
  
  # Fill variables
  cspc.fill <- list(
    ad_female_count = 0, ad_male_count = 0, ad_unk_count = 0,
    juv_female_count = 0, juv_male_count = 0, juv_unk_count = 0, 
    pup_live_count = 0, research_program = "USAMLR"#, orig_record = FALSE
  )
  
  if (!is.null(fill.location)) 
    cspc.fill <- c(cspc.fill, location = fill.location)
  
  # Check that this is only US AMLR data
  stopifnot(
    "research_program" %in% names(x), 
    all(x$research_program == "USAMLR")
  )
  
  x %>% 
    # Make to-complete columns factors to ensure all are created
    mutate(header_id = fct(as.character(header_id), 
                           as.character(x.header$header_id)), 
           species = fct(species, amlrPinnipeds::pinniped.phocid.sp)) %>%
    complete(header_id, species, fill = cspc.fill, explicit = FALSE) %>% 
    mutate(header_id = as_header_id_func(header_id), 
           species = as.character(species)) %>% 
    left_join(select(x.header, header_id, census_date_start), 
              by = "header_id") %>% 
    mutate(species = species, #hack for rstudio line formatting
           unk_female_count = if_else(
             census_date_start > ymd("2017-07-01") & is.na(unk_female_count), 
             as.integer(0), unk_female_count), 
           unk_male_count = if_else(
             census_date_start > ymd("2017-07-01") & is.na(unk_male_count), 
             as.integer(0), unk_male_count), 
           unk_unk_count = if_else(
             (census_date_start > ymd("2014-07-01") & is.na(unk_unk_count)) |
               (species == "Elephant seal" & is.na(unk_unk_count)),
             as.integer(0), unk_unk_count)) %>% 
    select(-census_date_start)
}


cspc_sum <- function(x, na.rm = TRUE) {
  if_else(all(is.na(x)), NA_integer_, sum(x, na.rm = na.rm))
}

cspc_total_count <- function(x, na.rm = TRUE) {
  x %>% 
    rowwise() %>%
    mutate(total_count = sum(c_across(ends_with("_count")), na.rm = TRUE)) %>% 
    ungroup()
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Aggregate by locations, complete, and combine

### Filter by core locations, group/summarise, and complete
# cspc.fill <- list(
#   ad_female_count = 0, ad_male_count = 0, ad_unk_count = 0,
#   juv_female_count = 0, juv_male_count = 0, juv_unk_count = 0, 
#   pup_live_count = 0, orig_record = FALSE
# )
loc.core <- "Core census locations"

cspc.core.agg <- cspc.wide %>% 
  # Filter for core locations, sum by header_id/species
  filter(location %in% c(cspc.locations.core)) %>% 
  group_by(header_id, species, location = loc.core) %>% 
  summarise(across(ends_with("_count"), cspc_sum),  
            # orig_record = TRUE, 
            .groups = "drop") %>% 
  left_join(select(cspc.header, header_id, research_program), 
            by = join_by(header_id))

# Complete the AMLR data, and bind all back together
amlr.complete <- cspc.core.agg %>% 
  filter(research_program == "USAMLR") %>% 
  cspc_amlr_complete(cspc.header, fill.location =  loc.core)

cspc.core.complete <- cspc.core.agg %>% 
  filter(research_program == "INACH") %>% 
  bind_rows(amlr.complete) %>% 
  select(-research_program)



### Filter for san telmo locations, group/summarise, and complete
pst.header <- cspc.header %>% filter(surveyed_san_telmo)

cspc.pst.complete <- cspc.wide %>% 
  filter(location %in% c(cspc.location.pst)) %>% 
  select(-c(census_date)) %>% 
  left_join(select(pst.header, header_id, research_program), 
            by = join_by(header_id)) %>% 
  cspc_amlr_complete(pst.header, fill.location = cspc.location.pst) %>% 
  select(-research_program)


### Bind core and pst records together
cspc.core.pst <- bind_rows(cspc.core.complete, cspc.pst.complete)%>%
  cspc_total_count() %>%
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

# # Sanity check
# d.header <- read_csv(here("output", "cs_phocid_census_header.csv")) %>% 
#   arrange(census_date_start) %>% 
#   group_by(season_name) %>% 
#   mutate(season_idx = seq_along(census_date_start), 
#          header_id_new = paste(season_name, 
#                                str_pad(season_idx, width = 2, pad = "0"), 
#                                sep = "-")) %>% 
#   ungroup()
# d <- read_csv(here("output", "cs_phocid_census_records.csv"))
# d1 <- d %>% 
#   select(header_id, location, species_common, season_name) %>% 
#   left_join(select(d.header, header_id, header_id_new), 
#                    by = join_by(header_id)) %>% 
#   select(header_id = header_id_new, location, species_common, season_name) %>% 
#   arrange(header_id, location, species_common)
# d2 <- cspc.core.pst %>% 
#   select(header_id, location, species_common) %>% 
#   mutate(location = if_else(location == "Core census locations",
#                             "CS core census locations", location)) %>% 
#   left_join(select(cspc.header, header_id, season_name), 
#             by = join_by(header_id)) %>% 
#   arrange(header_id, location, species_common)
# waldo::compare(d1, d2)
# waldo::compare(
#   d1 %>% filter(season_name != "1999/00"), 
#   d2 %>% filter(season_name != "1999/00")
# )


# TODO: provide a long version of this stuff as well
# cspc.core.pst.long <- cspc.core.pst %>% 
#   select(-c(total_count)) %>% #, total_count_nodead)) %>%
#   pivot_longer(ends_with("count"), 
#                names_to = "age_class_sex", values_to = "count") %>% 
#   filter(!is.na(count)) %>% 
#   mutate(acs_split = str_split(age_class_sex, "_"), 
#          age_class_pre = map_chr(acs_split, c(1)), 
#          sex_pre = map_chr(acs_split, c(2)), 
#          age_class = case_when(
#            age_class_pre ==  "ad" ~ "Adult", 
#            age_class_pre ==  "juv" ~ "Juvenile", 
#            age_class_pre ==  "pup" & sex_pre == "live" ~ "Pup-live", 
#            age_class_pre ==  "pup" & sex_pre == "dead" ~ "Pup-dead", 
#            age_class_pre ==  "unk" ~ "Unknown"
#          ), 
#          sex = if_else(sex_pre %in% c("F", "M", "U"), 
#                        sex_pre, NA_character_)) %>% 
#   relocate(age_class, sex, count, .after = "species") %>% 
#   select(-c(age_class_sex, acs_split, age_class_pre, sex_pre))



#-------------------------------------------------------------------------------
### Save data
write_csv(cspc.header, file = here("output", "cspc_header.csv"), na = "")
write_csv(cspc.core.pst, here("output", "cspc_records.csv"), na = "")


#-------------------------------------------------------------------------------
# Sanity checks
stopifnot(
  sum(is.na(cspc.core.pst$header_id)) == 0, 
  sum(is.na(cspc.core.pst$species)) == 0, 
  all(cspc.header$header_id %in% cspc.core.pst$header_id), 
  all(cspc.core.pst$header_id %in% cspc.header$header_id), 
  (nrow(cspc.core.pst) + 5) == #the 5 is for missing Weddell records
    (4 * nrow(cspc.header) + 4 * sum(cspc.header$surveyed_san_telmo))
)
waldo::compare(
  cspc.wide %>% 
    summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
    as.data.frame(), 
  (opportunistic %>% 
     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
     cspc.pst.complete %>% 
     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
     cspc.core.complete %>% 
     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))))
)

# cspc.wide %>% 
#   filter(!(location %in% cspc.locations.core)) %>% 
#   select(location) %>% tableNA()
# 
# opportunistic <- cspc.wide %>% 
#   filter(!(location %in% c(cspc.locations.core, cspc.location.pst))) 
# opportunistic %>% 
#   summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
#   glimpse()
# cspc.pst.complete %>% 
#   summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
#   glimpse()
# cspc.core.complete %>% 
#   summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
#   glimpse()
# cspc.wide %>% 
#   summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) %>% 
#   glimpse()
# 
# (opportunistic %>% 
#     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
#     cspc.pst.complete %>% 
#     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))) +
#     cspc.core.complete %>% 
#     summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE)))) %>% 
#   glimpse()


#-------------------------------------------------------------------------------
### Create table with column descriptors
x <- read_csv(here("output", "cspc_records.csv"), 
              col_types = "cccciiiiiiiiiii") %>% 
  as_data_frame()

tbl1 <- tribble(
  ~Column, ~Description,
  "header_id",      "A unique identifier with which to join data records with survey-level information",
  "location",       "The location for the corresponding count data",
  "species",        "The scientific name of the phocid species", 
  "species_common", "The common name of the phocid species",
  "total_count",	    "The sum of all of the other '_count' columns. I.e., the total count for the corresponding census/location/species", 
  "ad_female_count",  "Aggregate count of adult females for the corresponding census/location/species", 
  "ad_male_count", 	  "Aggregate count of adult males",
  "ad_unk_count", 	  "Aggregate count of adults of unknown sex",
  "juv_female_count", "Aggregate count of juvenile females",
  "juv_male_count", 	"Aggregate count of juvenile males",
  "juv_unk_count", 	  "Aggregate count of juveniles of unknown sex",
  "pup_count", 	      "Aggregate count of pups (less than one year old)",
  "unk_female_count", "Aggregate count of females of unknown age class",
  "unk_male_count", 	"Aggregate count of males of unknown age class",
  "unk_unk_count",    "Aggregate count of animals of unknown sex or unknown age class"
)

write_csv(tbl1, file = here("output", "table1.csv"), na = "")

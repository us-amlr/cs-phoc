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
         header_id = paste(season_name, 
                           str_pad(season_idx, width = 2, pad = "0"), 
                           sep = "-")) %>% 
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
  collect() %>% 
  left_join(select(cs.header.orig, header_id, census_phocid_header_id), 
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
# lapply(cs.wide, function(i) {
#   tableNA(amlr_season_from_date(cs.wide$census_date), is.na(i))
# })
# all(cs.wide$header_id %in% cs.header$header_id)
# tableNA(cs.wide$location)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# General variables and functions 

### Core Locations
# TODO: move to amlrPinnipeds as data variable
cs.locations.core <- c(
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
cs.location.pst <- "San Telmo, Punta"


### Functions used in combining
# TODO: move to amlrPinnipeds
cs_agg_amlr_complete <- function(x, x.header, fill.location = NULL) {
  # Checks
  stopifnot(
    "research_program" %in% names(x), 
    all(x$research_program == "USAMLR")
  )
  
  
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
  cs.fill <- list(
    ad_female_count = 0, ad_male_count = 0, ad_unk_count = 0,
    juv_female_count = 0, juv_male_count = 0, juv_unk_count = 0, 
    pup_live_count = 0, research_program = "USAMLR"#, orig_record = FALSE
  )
  if (!is.null(fill.location)) cs.fill <- c(cs.fill, location = fill.location)
  
  
  # Complete, and finish processing
  x.out <- x %>% 
    # Make to-complete columns factors to ensure all are created
    mutate(header_id = fct(as.character(header_id), 
                           as.character(x.header$header_id)), 
           species = fct(species, amlrPinnipeds::pinniped.phocid.sp)) %>%
    complete(header_id, species, fill = cs.fill, explicit = FALSE) %>% 
    mutate(header_id = as_header_id_func(header_id), 
           species = as.character(species)) %>% 
    left_join(select(x.header, header_id, census_date_start), 
              by = "header_id") %>% 
    # These fills are based on PI info about when these columns were used
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
    select(-census_date_start) %>% 
    select(header_id, location, species, everything()) 
  
  stopifnot(
    identical(names(x.out), names(x)), 
    identical(sapply(x.out, class), sapply(x, class))
  )
  
  x.out
}


cs_sum <- function(x, na.rm = TRUE) {
  if_else(all(is.na(x)), NA_integer_, sum(x, na.rm = na.rm))
}

cs_total_count <- function(x, na.rm = TRUE) {
  x %>% 
    rowwise() %>%
    mutate(total_count = sum(c_across(ends_with("_count")), na.rm = TRUE)) %>% 
    ungroup()
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Aggregate by locations, complete, and combine

### Filter by core locations, group/summarise, and complete
loc.core <- "Core census locations"

cs.core.agg <- cs.wide %>% 
  # Filter for core locations, sum by header_id/species
  filter(location %in% c(cs.locations.core)) %>% 
  group_by(header_id, location = loc.core, species) %>% 
  summarise(across(ends_with("_count"), cs_sum),  
            # orig_record = TRUE, 
            .groups = "drop") %>% 
  left_join(select(cs.header, header_id, research_program), 
            by = join_by(header_id))

# Complete the AMLR data, and bind all back together
amlr.complete <- cs.core.agg %>% 
  filter(research_program == "USAMLR") %>% 
  cs_agg_amlr_complete(cs.header, fill.location = loc.core)

cs.core.complete <- cs.core.agg %>% 
  filter(research_program == "INACH") %>% 
  bind_rows(amlr.complete) %>% 
  select(-research_program)



### Filter for san telmo locations, group/summarise, and complete
pst.header <- cs.header %>% filter(surveyed_san_telmo)

cs.pst.complete <- cs.wide %>% 
  filter(location %in% c(cs.location.pst)) %>% 
  select(-c(census_date)) %>% 
  left_join(select(pst.header, header_id, research_program), 
            by = join_by(header_id)) %>% 
  cs_agg_amlr_complete(pst.header, fill.location = cs.location.pst) %>% 
  select(-research_program)


### Bind core and pst records together
cs.core.pst <- bind_rows(cs.core.complete, cs.pst.complete)%>%
  cs_total_count() %>%
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


# TODO: provide a long version of this stuff as well
# cs.core.pst.long <- cs.core.pst %>% 
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
write_csv(cs.header, file = here("output", "cs-phoc-header.csv"), na = "")
write_csv(cs.core.pst, here("output", "cs-phoc-records.csv"), na = "")


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
# 
# 
# cs.wide %>%
#   filter(!(location %in% cs.locations.core)) %>%
#   select(location) %>% tableNA()
# 
# opportunistic <- cs.wide %>%
#   filter(!(location %in% c(cs.locations.core, cs.location.pst)))
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


#-------------------------------------------------------------------------------
### Create table with column descriptors
tbl1.ref <- read_csv(here("output", "cs-phoc-records.csv"), 
              col_types = "cccciiiiiiiiiii") %>% 
  as.data.frame()

tbl1 <- tribble(
  ~Column, ~`Data Type`, ~Description,
  "header_id",      "character", "A unique identifier with which to join data records with survey-level information",
  "location",       "character", "The location for the corresponding count data",
  "species",        "character", "The scientific name of the phocid species", 
  "species_common", "character", "The common name of the phocid species",
  "total_count",	    "integer", "The sum of all of the other '_count' columns. I.e., the total count for the corresponding census/location/species", 
  "ad_female_count",  "integer", "Aggregate count of adult females for the corresponding census/location/species", 
  "ad_male_count", 	  "integer", "Aggregate count of adult males",
  "ad_unk_count", 	  "integer", "Aggregate count of adults of unknown sex",
  "juv_female_count", "integer", "Aggregate count of juvenile females",
  "juv_male_count", 	"integer", "Aggregate count of juvenile males",
  "juv_unk_count", 	  "integer", "Aggregate count of juveniles of unknown sex",
  "pup_count", 	      "integer", "Aggregate count of pups (less than one year old)",
  "unk_female_count", "integer", "Aggregate count of females of unknown age class",
  "unk_male_count", 	"integer", "Aggregate count of males of unknown age class",
  "unk_unk_count",    "integer", "Aggregate count of animals of unknown sex and unknown age class"
)

write_csv(tbl1, file = here("output", "manuscript", "Table1.csv"), na = "")

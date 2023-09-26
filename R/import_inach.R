# Read in and explore INACH phocid census data from Renato
# This script writes the combined INACH data to both 
#   'inach_data/phocids_inach_cs.csv', and the AMLR PINNIPEDS database

library(tidyverse)
library(readxl)
library(lubridate)
library(amlrPinnipeds)
library(here)

con <- amlr_dbConnect(Database = "***REMOVED***")
write.files <- FALSE
write.to.db <- FALSE

beaches <- tbl(con, "beaches") %>% collect()
season.info <- tbl(con, "season_info") %>% collect()


################################################################################
### Read in census data, one species to sheet
x.info.names <- c("season_name", "week", "census_date", "location")
x.info.types <- c("text", "numeric", "date", "text")
inach.data <- here("data", "pre", "inach_data")

phocid.file.name <- here(
  # inach.data, "phocids Cape shirreff 97-2007 VERSION july22.xlsx"
  inach.data, "phocids Cape shirreff 97-2007 VERSION JULY 23.xlsx"
)
# "skip", "guess", "logical", "numeric", "date", "text" or "list"

x.cs <- read_xlsx(phocid.file.name, sheet = "Crab eater Seal", 
                  col_names = c(x.info.names, 
                                "ad_male_count", "ad_female_count", 
                                "juv_male_count", "juv_female_count", "juv_unk_count", 
                                "notes"), 
                  col_types = c(x.info.types, rep("numeric", 5), "text"), 
                  skip = 2, na = "ND") %>% 
  mutate(species = "Crabeater seal", 
         census_date = as.Date(census_date))


x.ls <- read_xlsx(phocid.file.name, sheet = "Leopard Seal", 
                  col_names = c(x.info.names, 
                                "ad_male_count", "ad_female_count", "ad_unk_count", 
                                "juv_male_count", "juv_female_count", "juv_unk_count", 
                                "notes"), 
                  col_types = c(x.info.types, rep("numeric", 6), "text"), 
                  skip = 2, na = "ND") %>% 
  mutate(species = "Leopard seal", 
         census_date = as.Date(census_date))


x.ses <- read_xlsx(phocid.file.name, sheet = "Elephant Seal", 
                   col_names = c(x.info.names, 
                                 "ad_male_count", "ad_female_count",
                                 "juv_male_count", "juv_female_count",
                                 "pup_male_count", "pup_female_count", "pup_unk_count", 
                                 "unk_unk_count", 
                                 "notes"), 
                   col_types = c(x.info.types, rep("numeric", 8), "text"), 
                   skip = 2, na = "ND") %>% 
  mutate(species = "Elephant seal",
         census_date = as.Date(census_date))


x.ws <- read_xlsx(phocid.file.name, sheet = "Weddell Seal", 
                  col_names = c(x.info.names, 
                                "ad_male_count", "ad_female_count", "ad_unk_count",
                                "juv_male_count", "juv_female_count", "juv_unk_count", 
                                "pup_male_count", "pup_female_count", "pup_unk_count", 
                                "notes"), 
                  col_types = c(x.info.types, rep("numeric", 9), "text"), 
                  skip = 2, na = "ND") %>% 
  mutate(species = "Weddell seal",
         census_date = as.Date(census_date))


# Bind all census data into one data frame
x.orig <- bind_rows(x.cs, x.ls, x.ses, x.ws)


### Checks of data
check_func_na <- function(i) {
  all(
    all(!is.na(i$season_name)), all(!is.na(i$week)), 
    all(!is.na(i$census_date)), all(!is.na(i$location))
  )
}

check_func_season <- function(i) {
  season.yr.1 <- if_else(month(i$census_date) > 7, 
                         year(i$census_date), year(i$census_date)-1)
  season.chr <- paste(season.yr.1, substr(season.yr.1+1, 3, 4), sep = "/")
  
  identical(i$season_name, season.chr)
}

stopifnot(
  all(x.orig$season_name %in% season.info$season_name), 
  check_func_na(x.orig), 
  check_func_season(x.orig)
)



################################################################################
# Organize combined data frames

# Remove Feb 2007 dates based on notes from Renato
inach.dates.toremove <- c(
  as.Date("2007-02-01"), as.Date("2007-02-08"), as.Date("2007-02-16")
)
x.torm <- x.orig %>% filter((census_date %in% inach.dates.toremove))

### Census data
x <- x.orig %>% 
  filter(!(census_date %in% inach.dates.toremove)) %>% 
  mutate(header_id = paste(season_name, str_pad(week, 2, "left", 0), 
                           sep = "-w"), 
         research_program = "INACH", 
         location = case_when(
           location == "All Cape" ~ "Cape Shirreff",
           location == "Angosta" ~ "Angosta",
           location == "Antartico" ~ "Antartico",
           location == "Bahamondes" ~ "Bahamonde",
           location == "El Canal" ~ "del Canal",
           location == "El Lobero" ~ "del Lobero",
           location == "El Modulo" ~ "Modulo",
           location %in% c("El Remanso", "Remanso") ~ "El Remanso",
           location == "Golondrina" ~ "Golondrina",
           location == "Nibaldo Bahamondes" ~ "Peninsula Cerro Gajardo",
           location %in% c("paulina", "Paulina") ~ "Paulina",
           location == "Pinochet de La Barra" ~ "Pinochet de la Barra",
           location == "Plastico" ~ "El Plastico",
           location == "Pocitas" ~ "Pocitas",
           location == "Pta Poblete" ~ "Punta Poblete",
           location %in% c("Pta Ventana", "Punta Ventana") ~ "Ventana",
           location %in% c("Schiappacasse") ~ "Schiappacasse",
           location == "Yamana" ~ "Yamana",
           TRUE ~ location
         ), 
         location_group = if_else(location == "Paso Ancho",
                                  "Media Luna", location), 
         # TODO: confirm with Renato
         juv_female_count = if_else(
           header_id == "1998/99-w03" & location == 'Larga' & species == 'Elephant seal', 
           as.integer(0), juv_female_count), 
         pup_unk_count = if_else(
           header_id == "1999/00-w03" & location == 'Media Luna' & species == 'Elephant seal', 
           as.integer(0), pup_unk_count)) %>% 
  nest(pup_counts = c(pup_female_count, pup_male_count, pup_unk_count)) %>% 
  mutate(pup_live_count = map_int(pup_counts, function(i) {
    if_else(all(is.na(i)), NA_integer_, sum(i, na.rm = TRUE))
  })) %>% 
  unnest(cols = c(pup_counts)) %>% 
  mutate(notes = case_when(
    is.na(pup_live_count) ~ NA_character_, 
    pup_live_count > 0 ~ str_glue("pups: ",
                                  "{pup_female_count} females, ", 
                                  "{pup_male_count} males, ", 
                                  "{pup_unk_count} unknowns"), 
    # NOTE: confirmed that don't need to keep any other notes from INACH data
    TRUE ~ NA_character_)) %>% 
  select(-c(pup_female_count, pup_male_count, pup_unk_count)) %>% 
  select(header_id, season_name, census_date, location, location_group, #week, 
         species, starts_with("ad_"), starts_with("juv"), starts_with("pup_"), 
         unk_unk_count, notes, research_program) %>% 
  arrange(census_date, location, species)

# Sanity checks on row numbers, beach names, and rogue NAs
stopifnot(
  nrow(x.orig) == (nrow(x) + nrow(x.torm)), 
  all(x$location %in% beaches$name), 
  0 == nrow(x %>% 
              group_by(header_id, species) %>% 
              filter(if_any(ends_with("_count"), ~ any(is.na(.)) & any(!is.na(.))))), 
  sum(x.orig %>% 
        summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE)))) == 
    sum(x %>% summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE)))) + 
    sum(x.torm %>% summarise(across(ends_with("_count"), \(x) sum(x, na.rm = TRUE))))
)


### Census header data
x.header <- x %>% 
  group_by(header_id, season_name) %>% 
  summarise(census_date_start = min(census_date), 
            census_date_end = max(census_date), 
            surveyed_san_telmo = FALSE, 
            research_program = "INACH", 
            .groups = "drop")


# ### Write to files
# if (write.files) {
#   write.csv(x, row.names = FALSE, 
#             file = here(inach.data, "cs-phoc-inach.csv"))
#   write.csv(x.header, row.names = FALSE, 
#             file = here(inach.data, "cs-phoc-inach_header.csv"))
# }


### Write to database
if (write.to.db) {
  rm(con)
  con <- amlr_dbConnect(Database = "***REMOVED***")
  
  # Prep header records and write to database
  x.header.todb <- x.header %>% 
    select(-c(header_id, season_name, research_program))
  DBI::dbAppendTable(con, "census_phocid_header", x.header.todb)
  
  # Pull header info to get IDs, and get IDs to census records
  db.header <- tbl(con, "census_phocid_header") %>% 
    filter(census_date_start < as.Date("2009-07-01")) %>% 
    select(-surveyed_san_telmo) %>% 
    collect() %>% 
    full_join(select(x.header, header_id, census_date_start, census_date_end), 
              by = join_by(census_date_start, census_date_end)) %>% 
    select(header_id, census_phocid_header_id)
  stopifnot(nrow(db.header) == nrow(x.header))
  
  beaches.fordb <- beaches %>% select(Beach_ID = ID, location = name)
  
  # Prep census records and write to database
  # note: explicit zero records were imported
  x.todb <- x %>% 
    left_join(db.header, by = join_by(header_id)) %>% 
    left_join(beaches.fordb, by = join_by(location)) %>% 
    select(-c(header_id, season_name, location, location_group)) %>% 
    mutate(census_type = "Phocid", exclude_count = 0)
  stopifnot(all(!is.na(x.todb$Beach_ID)), 
            all(!is.na(x.todb$census_phocid_header_id)))
  DBI::dbAppendTable(con, "census", x.todb)
}

# # DB import - Sanity check
# d1 <- x %>%
#   select(-c(header_id, season_name)) %>%
#   arrange(census_date, species, location) %>%
#   mutate(across(ends_with("_count"), as.integer))
# d2 <- tbl(con, "vCensus_Phocid") %>%
#   filter(census_date < as.Date("2009-07-01")) %>%
#   rename(notes = census_notes) %>%
#   collect() %>%
#   select(!!names(d1)) %>%
#   arrange(census_date, species, location)
# waldo::compare(d1, d2)




################################################################################
################################################################################
################################################################################
# Explore INACH data for potential issues
#   NOTE: to run again, would need to comment out line removing 'week' from x

#-------------------------------------------------------------------------------
### Issues/questions - v1 sent to Renato 9 Mar 2022
# Check for duplicates across date/location/species
x.issue.dup <- x %>% 
  group_by(census_date, location, species) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(season_name, week)
# write.csv(x.issue.dup, row.names = FALSE,
#           file = "inach_out/phocid_census_inach_duplicates_20220714.csv")


# Check for duplicates across Season name/week/date
#   Note that these are ok if the census happened on multiple days
x.issue.header <- x %>% 
  group_by(season_name, week, census_date) %>% 
  summarise(num_records = n(), .groups = "drop") %>% 
  group_by(season_name, week) %>% 
  mutate(season_week_duplicate = if_else(n() > 1, TRUE, NA)) %>% 
  ungroup()

# x.issue.week <- x.header %>%
#   group_by(season_name, week) %>%
#   filter(n() > 1) %>%
#   ungroup()

# write.csv(x.issue.header, row.names = FALSE, na = "", 
#           file = "inach_out/phocid_census_inach_headers.csv")


# Check for beach names that have not been handled and aren't in beaches table
x.beach.unk <- x %>% filter(!(location %in% beaches$name))
tableNA(x.beach.unk$location)
# x.issue.beach <- x %>% 
#   filter(location %in% c("Pta Olivia", "Rocas Yeco", "Rodrigo y Hucke-Gaete"))

# write.csv(x.beach.unk, row.names = FALSE, na = "",
#           file = "inach_out/phocid_census_inach_unkbeaches_20220714.csv")


# # Explore pup count columns
# sum(xor(is.na(x$pup_female_count), is.na(x$pup_male_count)))
with(x.orig, tableNA(is.na(pup_female_count), is.na(pup_male_count), is.na(pup_unk_count)))
with(x.orig, tableNA(is.na(pup_female_count), is.na(pup_male_count)))

# Check there aren't overlapping Nibaldo/Bahamonde and 
# Cerro Gajardo, Peninsula records
x %>% 
  filter(location %in% c("Bahamonde", "Nibaldo", "Cerro Gajardo, Peninsula")) %>% 
  group_by(header_id, species) %>% 
  filter(any(location %in% c("Bahamonde", "Nibaldo")), 
         any(location == "Cerro Gajardo, Peninsula"))



#-------------------------------------------------------------------------------
### Plotting
x.toplot <- x %>% 
  group_by(season_name, week, census_date, species) %>% 
  summarise(across(c(ad_male_count:unk_unk_count), sum, na.rm = TRUE, 
                   .names = ), 
            .groups = "drop") %>% 
  mutate(wk_id = paste(season_name, str_pad(week, 2, pad = "0"), sep = "_")) %>% 
  # pivot_longer(ad_male_count:unk_unk_count, 
  #              names_to = c("age_class", "sex"), names_pattern = "(.*)_(.*)_count")
  pivot_longer(ad_male_count:unk_unk_count, names_pattern = "(.*)_count",
               names_to = "age_sex", values_to = "count") %>% 
  mutate(grp = paste(season_name, age_sex, sep = "_"), 
         age_sex = factor(age_sex))


plot_func <- function(x.toplot, i.sp) {
  x.toplot %>% 
    filter(species == i.sp) %>% 
    ggplot(aes(wk_id, count, group = grp, color = age_sex)) +
    # ggplot(aes(census_date, count, group = grp, color = age_sex)) +
    geom_point() + 
    geom_line() + 
    # facet_grid(vars(season_name)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    ggtitle(i.sp) + 
    xlab("season_week")
}

plot_func_facet <- function(x.toplot, i.sp) {
  plot_func(x.toplot, i.sp) +
    facet_wrap(vars(season_name), scales = "free_x")
}

plot_func(x.toplot, "Crabeater seal")
plot_func(x.toplot, "Leopard seal")
plot_func(x.toplot, "Elephant seal")
plot_func(x.toplot, "Weddell seal")

plot_func_facet(x.toplot, "Crabeater seal")
plot_func_facet(x.toplot, "Leopard seal")
plot_func_facet(x.toplot, "Elephant seal")
plot_func_facet(x.toplot, "Weddell seal")

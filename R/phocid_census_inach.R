# Read in and explore INACH phocid census data from Renato
# This script writes the combined INACH data to 'inach_data/phocids_inach_cs.csv'

library(tidyverse)
library(readxl)
library(lubridate)
library(odbc)
library(here)

tableNA <- function(...) table(..., useNA = 'ifany')

con <- dbConnect(odbc(), Driver = "ODBC Driver 18 for SQL Server",
                 Server = "swc-***REMOVED***-s", 
                 Database = "***REMOVED***",
                 Trusted_Connection = "Yes", 
                 Encrypt = "optional")

beaches <- tbl(con, "beaches") %>% collect()
season.info <- tbl(con, "season_info") %>% collect()


################################################################################
### Read in census data, one species to sheet
x.info.names <- c("season_name", "week", "census_date", "location")
x.info.types <- c("text", "numeric", "date", "text")
inach.data <- here("data", "inach_data")

phocid.file.name <- here(
  inach.data, "phocids Cape shirreff 97-2007 VERSION july22.xlsx"
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
# Organize combined data frame
x <- x.orig  %>% 
  arrange(census_date, location, species) %>% 
  mutate(header_id = paste(season_name, str_pad(week, 2, "left", 0), 
                           sep = "-"), 
         research_program = "INACH", 
         location = case_when(
           location == "All Cape" ~ "Cape Shirreff",
           location == "Angosta" ~ "Angosta, Playa",
           location == "Antartico" ~ "Antartico, Playa",
           location == "Bahamondes" ~ "Bahamonde",
           location == "El Canal" ~ "del Canal, Playa",
           location == "El Lobero" ~ "del Lobero, Playa",
           location == "El Modulo" ~ "Modulo",
           location %in% c("El Remanso", "Remanso") ~ "El Remanso, Playa",
           location == "Golondrina" ~ "Golondrina, Playa",
           location == "Nibaldo Bahamondes" ~ "Cerro Gajardo, Peninsula",
           location %in% c("paulina", "Paulina") ~ "Paulina, Playa",
           location == "Pinochet de La Barra" ~ "Pinochet de la Barra",
           location == "Plastico" ~ "El Plastico, Playa",
           location == "Pocitas" ~ "Pocitas, Playa",
           # location == "Pta Oliva" ~ "Alcazar",
           location == "Pta Poblete" ~ "Poblete, Punta",
           location %in% c("Pta Ventana", "Punta Ventana") ~ "Ventana",
           # Roca granito
           # location %in% c("Rocas Yeco", "Schiappacasse") ~ "Schiappacasse, Playa",
           location %in% c("Schiappacasse") ~ "Schiappacasse, Playa",
           # Rodrigo y Hucke-Gaete
           location == "Yamana" ~ "Yamana, Playa",
           TRUE ~ location
         )) %>% 
  select(header_id, season_name, week, census_date, location, species, 
         starts_with("ad_"), starts_with("juv"), starts_with("pup_"), 
         unk_unk_count, notes, research_program)

x.header <- x %>% 
  group_by(header_id, season_name, week) %>% 
  summarise(census_date_start = min(census_date), 
            census_date_end = max(census_date), 
            surveyed_san_telmo = FALSE, 
            .groups = "drop")

write.csv(x, row.names = FALSE, file = here(inach.data, "phocids_cs_inach.csv"))
write.csv(x.header, row.names = FALSE, 
          file = here(inach.data, "phocids_cs_inach_header.csv"))


################################################################################
# Explore INACH data for potential issues

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
with(x, tableNA(is.na(pup_female_count), is.na(pup_male_count), is.na(pup_unk_count)))
with(x, tableNA(is.na(pup_female_count), is.na(pup_male_count)))


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

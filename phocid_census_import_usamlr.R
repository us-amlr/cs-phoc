# Import data from Mike's Excel files

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)
library(DBI)
library(odbc)

tableNA <- function(...) table(..., useNA = 'ifany')
source("C:/SMW/Databases/***REMOVED***/Scripts-issues/mutate_location.R")

con <- dbConnect(odbc(), Driver = "SQL Server", #"SQL Server Native Client 11.0",
                 Server = "swc-***REMOVED***-s", 
                 Database = "***REMOVED***_Test",
                 Trusted_Connection = "True")

census <- tbl(con, "census") %>% collect()
census.phocid <- tbl(con, "vCensus_Phocid") %>% collect() %>% 
  mutate(census_date = ymd(census_date)) %>% 
  rowwise() %>% 
  mutate(count_sum = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE)) %>% 
  ungroup()
beaches <- tbl(con, "beaches") %>% collect()
# lookup.age.class <- tbl(con, "lookup_age_class") %>% collect()


### Read original Excel files
col.types <- c(
  "numeric", "date", rep("text", 13), rep("numeric", 33), rep("numeric", 2), 
  "date", rep("numeric", 4)
)

x.200910.orig <- read_xlsx(
  "15-Weekly Phocid census/Weekly Phocid Census 2009-10.xlsx", skip = 2, 
  col_types = col.types
)
x.201011.orig <- read_xlsx( #same names as x.200910
  "15-Weekly Phocid census/Weekly Phocid Census 2010-11.xlsx", skip = 2, 
  col_types = col.types
)
x.201112.orig <- read_xlsx( #same names as x.200910
  "15-Weekly Phocid census/Weekly Phocid Census 2011-12.xlsx", skip = 2
)
x.201213.orig <- read_xlsx(
  "15-Weekly Phocid census/Weekly Phocid Census 2012-13.xlsx", 
  sheet = "Weekly Phocid Census 2012-13", skip = 1
)
x.201213.orig.wx <- read_xlsx(
  "15-Weekly Phocid census/Weekly Phocid Census 2012-13.xlsx", 
  sheet = "Sheet1"
)


# waldo::compare(names(x.200910.orig), names(x.201011.orig))
# waldo::compare(names(x.200910.orig), names(x.201112.orig))
# waldo::compare(names(x.200910.orig), names(x.201213.orig)) #diff
# 
# waldo::compare(sapply(x.200910.orig, class), sapply(x.201011.orig, class))
# waldo::compare(sapply(x.200910.orig, class), sapply(x.201112.orig, class)) #diff types
# waldo::compare(sapply(x.201011.orig, class), sapply(x.201112.orig, class)) #diff types

### Duplicates
census_dup <- function(x) {
  x %>% 
    group_by(census_date, Beach, species) %>% 
    filter(n() > 1) %>% 
    arrange(census_date, Beach, species) %>% 
    ungroup()
}

census.phocid.dup <- census.phocid %>% census_dup()
tableNA(census.phocid.dup$season_name)


################################################################################
# Processing function
census_proc <- function(x.orig) {
  ### Get totals from sheet
  x.total <- x.orig %>% 
    select(week = `WEEK #...50`, census_date = `Date...51`, SES:CRB) %>% 
    rowwise() %>% 
    filter(all(c(!is.na(week), !is.na(census_date), !is.na(c_across(SES:CRB))))) %>% 
    ungroup()
  
  
  ### Start procesing raw census data
  # x.orig <- x.201011.orig
  x.proc <- x.orig %>% 
    select(week = `WEEK #...1`, observer = Obs., census_date = Date...2, 
           time_start = `Start time`, time_end = `End time`, 
           species = Species, code = Code, sex_age_class = `Sex/age class`, 
           `P. Media Luna`:`Punta San Telmo`) %>% 
    mutate(sex_age_class = tolower(sex_age_class),
           time_start = as.character(chron::times(suppressWarnings(as.numeric(time_start)))),
           time_end = as.character(chron::times(suppressWarnings(as.numeric(time_end)))),
           census_column = case_when(
             sex_age_class == "female" ~ "ad_female_count", 
             sex_age_class == "juv female" ~ "juv_female_count", 
             sex_age_class == "juv male" ~ "juv_male_count", 
             sex_age_class == "juv unk" ~ "juv_unk_count", 
             sex_age_class == "male" ~ "ad_male_count", 
             sex_age_class == "pup" ~ "pup_live_count", 
             sex_age_class == "unknown" ~ "ad_unk_count",
             TRUE ~ NA_character_
           )) %>% 
    fill(week, species, #observer, census_date, time_start, time_end, 
         .direction = "down")
  print(tableNA(x.proc$species, x.proc$code))
  print(as.data.frame(tableNA(x.proc$sex_age_class, x.proc$census_column)) %>% filter(Freq != 0))
  
  ### Extract 'header' info
  x.info <- x.proc %>% 
    select(week, observer, census_date, time_start, time_end) %>% 
    filter(!is.na(observer) | !is.na(census_date) | !is.na(time_start) | !is.na(time_end)) %>% 
    distinct()
  
  ### Finish processing to get in same format as census table
  # TODO: fix the 'header' info, for consistency
  x.census <- x.proc %>% 
    select(!c(observer, census_date, time_start, time_end)) %>% 
    pivot_longer(cols = `P. Media Luna`:`Punta San Telmo`, 
                 names_to = "Location", values_to = "count") %>% 
    replace_na(list(count = 0)) %>% 
    mutate_location() %>% 
    mutate(Location = case_when( #For matching with beaches.Name value
      Location == "P. Delphin to P. La Caverna" ~ "La Caverna", 
      Location == "P. Golondrina to P. del Lobero" ~ "Golondrina-del Lobero", 
      Location == "P. Paulina to P. Aranda" ~ "Paulina-Aranda", 
      TRUE ~ Location
    )) %>% 
    left_join(select(beaches, Location = Name, Beach_ID = ID), by = "Location") %>% 
    # TODO: "P. Golondrina to P. del Lobero", "P. Delphin to P. La Caverna", "P. Paulina to P. Aranda"
    select(!c(sex_age_class, code)) %>% 
    filter(!is.na(census_column)) %>% 
    pivot_wider(names_from = census_column, values_from = count) %>% 
    rowwise() %>% 
    mutate(count_sum = sum(c_across(pup_live_count:ad_unk_count), na.rm = TRUE), 
           count_allzero = count_sum == 0) %>% 
    ungroup() %>% 
    select(week, species, Location, Beach_ID, ad_female_count, ad_male_count, 
           ad_unk_count, juv_male_count, juv_female_count, juv_unk_count, 
           everything())
  
  ### Messages and error checks
  print("BEACHES TO FIX") 
  print(paste(sort(unique(filter(x.census, !(Location %in% beaches$Name))$Location)), 
              collapse = "; "))
  # Check that there are no duplicates
  stopifnot(nrow(distinct(x.census, species, week, Location)) == nrow(x.census))
  print("COUNTS")
  print(paste("All zero:", sum(!x.census$count_allzero), 
              "out of", nrow(x.census)))
  
  x.census.summ <- x.census %>% 
    mutate(census_date = NA_POSIXct_, 
           code = case_when(
             species == "Elephant seal" ~ "SES", 
             species == "Weddell seal" ~ "WED", 
             species == "Leopard seal" ~ "LEP", 
             species == "Crabeater seal" ~ "CRB"
           )) %>% 
    group_by(week, census_date, code) %>% 
    summarise(count = sum(count_sum), .groups = "drop") %>% 
    pivot_wider(names_from = code, values_from = count) %>% 
    select(!!names(x.total))
  print(waldo::compare(
    select(x.total, -census_date), select(x.census.summ, -census_date)
  ))
  
  list(info = x.info, census = x.census %>% rename(Beach = Location), 
       total = x.total, summ = x.census.summ)
}


################################################################################
### Process data from seasons
#-------------------------------------------------------------------------------
x.200910.list <- census_proc(x.200910.orig)
x.201011.list <- census_proc(x.201011.orig)
x.201112.list <- census_proc(x.201112.orig)

x.201213 <- x.201213.orig %>% 
  rename(census_date = Date, Location = beach, 
         ad_female_count = female_count, ad_male_count = male_count, 
         ad_unk_count = unknown_count, juv_unk_count = juv_unknown_count, 
         pup_live_count = pups_live_count, pup_dead_count = pups_dead_count) %>% 
  mutate(species = str_to_sentence(species), 
         Location = case_when( #For matching with beaches.Name value
           Location == "Punta Delphin to La Caverna" ~ "Punta Delphin-La Caverna", 
           Location == "Playa Golondrina to P. del Lobero" ~ "Golondrina-del Lobero", 
           Location == "Playa Paulina to Aranda" ~ "Paulina-Aranda", 
           TRUE ~ Location
         )) %>% 
  mutate_location() %>% 
  rowwise() %>% 
  mutate(count_sum = sum(c_across(ad_female_count:pup_dead_count), 
                         na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(x.201213.orig.wx, by = c("census_wx_id" = "ID")) %>% 
  mutate(census_date = ob_date)

# all(x.201213$TOTAL == x.201213$count_sum) #TRUE
x.201213.dup <- x.201213 %>% census_dup() #nrow = 20


#-------------------------------------------------------------------------------
### Compare this data with data in DB for 2011/12 and 2012/13
y.201112 <- census.phocid %>% filter(season_name == "2011/12")
y.201213 <- census.phocid %>% filter(season_name == "2012/13")


### 2011/12
x.201112 <- x.201112.list$census %>% 
  filter(!count_allzero) %>% 
  left_join(distinct(x.201112.list$info, week, .keep_all = TRUE), 
            by = "week")
z.201112 <- full_join(
  y.201112 %>% select(census_id, observer, census_date, Location, species, 
                      ad_female_count:unk_unk_count, count_sum), 
  x.201112 %>% select(week, observer, census_date, species, Location, 
                      ad_female_count:count_sum), 
  by = c("census_date", "species", "Location")
)

# Appears to be 30 records not in the database
# which(is.na(z.201112$census_id))
z.201112.nodb <- z.201112 %>% filter(is.na(census_id))
z.201112.noexcel <- z.201112 %>% filter(is.na(week))
z.201112.mismatch <- z.201112 %>% 
  filter(ad_female_count.x != ad_female_count.y | ad_male_count.x != ad_male_count.y | 
           ad_unk_count.x != ad_unk_count.y | juv_female_count.x != juv_female_count.y | 
           juv_male_count.x != juv_male_count.y | juv_unk_count.x != juv_unk_count.y |
           pup_live_count.x != pup_live_count.y | 
           count_sum.x != count_sum.y)
# Mismatch: 4 Feb 2012: db is wrong, should be one ad female
#   Other 4 line up with duplicate issue


### 2012/13
z.201213 <- full_join(
  y.201213 %>% select(census_id, observer, census_date, Location, species, 
                      ad_female_count:unk_unk_count, count_sum), 
  x.201213 %>% select(ID, census_date = ob_date, species, Location, 
                      ad_female_count:pup_dead_count, count_sum), 
  by = c("census_date", "species", "Location")
)


z.201213.nodb <- z.201213 %>% filter(is.na(census_id)) 
#Ridge wallow was mapped to DB in the database. 
#   However, this is the reason for the M Luna eseal dup on 12-28-2012
z.201213.noexcel <- z.201213 %>% filter(is.na(ID)) 
#Larga/largo mismatch between x and y
z.201213.mismatch <- z.201213 %>% 
  filter(ad_female_count.x != ad_female_count.y | ad_male_count.x != ad_male_count.y | 
           ad_unk_count.x != ad_unk_count.y | juv_female_count.x != juv_female_count.y | 
           juv_male_count.x != juv_male_count.y | juv_unk_count.x != juv_unk_count.y |
           pup_live_count.x != pup_live_count.y | pup_dead_count.x != pup_dead_count.y | 
           count_sum.x != count_sum.y)
# Appears to mostly be due to duplicates?

################################################################################
x.info.all <- bind_rows(x.200910$info, x.201011$info, x.201112$info)

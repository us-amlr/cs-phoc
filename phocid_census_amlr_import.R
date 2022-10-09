# Import data from Mike's Excel files for 2009/10 - 2012/13. 
# This script has been updated to account for the census_phocid_header table
# See here for decisions: 
#   https://docs.google.com/document/d/1B3xK8ba0YQE4y3MPw5E04O7IaYOz94DuljDAfA4-7iY

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)
library(odbc)

tableNA <- function(...) table(..., useNA = 'ifany')
source("C:/SMW/Databases/***REMOVED***/Scripts-general/mutate_location.R")

con <- dbConnect(odbc(), Driver = "SQL Server", #"SQL Server Native Client 11.0",
                 Server = "swc-***REMOVED***-s", 
                 Database = "***REMOVED***",
                 # Database = "***REMOVED***_Test",
                 Trusted_Connection = "True")

census <- tbl(con, "census") %>% collect()
census.phocid <- tbl(con, "vCensus_Phocid") %>% collect() %>% 
  mutate(census_date = ymd(census_date)) %>% 
  rowwise() %>% 
  mutate(count_sum = sum(c_across(ad_female_count:unk_unk_count), na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(season_info_id:species, count_sum, everything())
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
x_201112_time <- function(x) {
  as.character(as.numeric(hms::as_hms(format(x, format = "%H:%M:%S")))/(24*60*60))
}
x.201112.orig <- read_xlsx( #same names as x.200910
  "15-Weekly Phocid census/Weekly Phocid Census 2011-12.xlsx", skip = 2
) %>% 
  mutate(`Start time` = x_201112_time(`Start time`), 
         `End time` = x_201112_time(`End time`))

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
    group_by(census_date, location, species) %>% 
    filter(n() > 1) %>% 
    mutate(count_sum_diff = length(unique(count_sum)) > 1) %>% 
    arrange(census_date, location, species) %>% 
    ungroup() %>% 
    select(season_info_id:count_sum, count_sum_diff, everything())
}

census.phocid.dup <- census_dup(census.phocid)
tableNA(census.phocid.dup$season_name)
# googlesheets4::write_sheet( #WARNING - will overwrite Sam notes
#   census.phocid.dup, sheet = "census_phocid_duplicates",
#   ss = paste0("https://docs.google.com/spreadsheets/d/1naKQhjKdv8JsuiG3VANkeAk7uYtYYgSRgjIi7dQMObk")
# )



################################################################################
# Processing function
census_proc <- function(x.orig) {
  ### Get totals from sheet
  x.total <- x.orig %>% 
    select(week = `WEEK #...50`, census_date = `Date...51`, SES:CRB) %>% 
    rowwise() %>% 
    filter(all(c(!is.na(week), !is.na(census_date), !is.na(c_across(SES:CRB))))) %>%
    ungroup()
  
  x.renamed <- x.orig %>% 
    select(week = `WEEK #...1`, observer = Obs., census_date = Date...2, 
           time_start = `Start time`, time_end = `End time`, 
           species = Species, code = Code, sex_age_class = `Sex/age class`, 
           `P. Media Luna`:`Punta San Telmo`)
  
  ### Extract 'header' info
  x.header <- x.renamed %>% 
    select(week:time_end) %>% 
    mutate(observer = str_to_upper(str_remove(observer, "`")), 
           census_date = as.Date(census_date), 
           time_start = as.character(chron::times(suppressWarnings(as.numeric(time_start)))),
           time_end = as.character(chron::times(suppressWarnings(as.numeric(time_end)))),) %>% 
    fill(week, .direction = "down") %>% 
    filter(if_any(observer:time_end, ~ !is.na(.)), 
           !(census_date == as.Date("2010-03-02") & week == 19)) %>% 
    group_by(week) %>% 
    summarise(census_date_start = min(census_date, na.rm = TRUE), 
              census_date_end = max(census_date, na.rm = TRUE), 
              surveyed_san_telmo = if_else(
                census_date_start %in% c(ymd("2010-12-03"), ymd("2010-12-18")), 
                FALSE, TRUE), 
              notes = paste("Imported from Excel files. Observers:", 
                            paste(na.omit(unique(observer)), collapse = ", ")), 
              time_start = min(time_start, na.rm = TRUE), 
              time_end = max(time_end, na.rm = TRUE))
  
  x.header <- x.header %>% 
    mutate(notes = if_else(
      census_date_start == census_date_end, notes, 
      paste0(notes, ". Note that associated census records were assigned ", 
             "the census_date_end value because Excel files did not specify ", 
             "which counts were from which day")
    ))
  
  
  ### Start processing raw census data
  x.proc <- x.renamed %>% 
    mutate(sex_age_class = tolower(sex_age_class),
           # time_start = as.character(chron::times(suppressWarnings(as.numeric(time_start)))),
           # time_end = as.character(chron::times(suppressWarnings(as.numeric(time_end)))),
           census_column = case_when(
             sex_age_class == "female" ~ "ad_female_count", 
             sex_age_class == "male" ~ "ad_male_count", 
             sex_age_class == "unknown" ~ "ad_unk_count",
             sex_age_class == "juv female" ~ "juv_female_count", 
             sex_age_class == "juv male" ~ "juv_male_count", 
             sex_age_class == "juv unk" ~ "juv_unk_count", 
             sex_age_class == "pup" ~ "pup_live_count", 
             TRUE ~ NA_character_
           )) %>% 
    fill(week, species, #observer, census_date, time_start, time_end, 
         .direction = "down")
  print(tableNA(x.proc$species, x.proc$code))
  print(as.data.frame(tableNA(x.proc$sex_age_class, x.proc$census_column)) %>% 
          filter(Freq != 0))
  
  ### Finish processing to get in same format as census table
  x.census <- x.proc %>% 
    select(!c(observer, census_date, time_start, time_end)) %>% 
    pivot_longer(cols = `P. Media Luna`:`Punta San Telmo`, 
                 names_to = "location", values_to = "count") %>% 
    replace_na(list(count = 0)) %>% 
    mutate_location() %>% 
    mutate(research_program = "USAMLR", 
           location = case_when( #For matching with beaches.name value
             location == "P. Delphin to P. La Caverna" ~ "La Caverna", 
             location == "P. Golondrina to P. del Lobero" ~ "Golondrina-del Lobero", 
             location == "P. Paulina to P. Aranda" ~ "Paulina-Aranda", 
             TRUE ~ location
           )) %>% 
    # left_join(select(beaches, location = name, Beach_ID = ID), by = "location") %>% 
    left_join(select(beaches, location = name, Beach_ID = ID), by = "location") %>% 
    left_join(select(x.header, week, census_date = census_date_end, 
                     time_start, time_end), by = "week") %>% 
    select(!c(sex_age_class, code)) %>% 
    filter(!is.na(census_column)) %>% 
    pivot_wider(names_from = census_column, values_from = count) %>% 
    rowwise() %>% 
    mutate(observer = NA_character_, 
           count_sum = sum(c_across(contains("count")), na.rm = TRUE), 
           count_allzero = count_sum == 0) %>% 
    ungroup() %>% 
    select(week, observer, census_date, time_start, time_end, 
           location, Beach_ID, species, 
           ad_female_count, ad_male_count, ad_unk_count, 
           juv_male_count, juv_female_count, juv_unk_count, 
           pup_live_count, everything(), research_program)
  
  ### Messages and error checks
  # TODO: change to $name
  b.bad <- sort(unique(filter(x.census, !(location %in% beaches$name))$location))
  if (length(b.bad) > 0) {
    print("BEACHES TO FIX") 
    print(paste(sort(unique(filter(x.census, !(location %in% beaches$name))$location)), 
                collapse = "; "))
  }
  # Check that there are no duplicates
  stopifnot(nrow(distinct(x.census, species, week, location)) == nrow(x.census))
  # print("COUNTS")
  # print(paste("All zero:", sum(!x.census$count_allzero), 
  #             "out of", nrow(x.census)))
  
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
  
  x.census <- x.census %>% 
    rename(Beach = location) %>% 
    filter(!count_allzero) #%>% 
  # select(-c(count_sum, count_allzero))
  
  list(header = x.header %>% select(-c(time_start, time_end)), 
       census = x.census, total = x.total, summ = x.census.summ)
}


################################################################################
################################################################################
# Process data from 'new' seasons, and import

#-------------------------------------------------------------------------------
x.200910.list <- census_proc(x.200910.orig)
x.201011.list <- census_proc(x.201011.orig)

# View(x.201011.list$header)
# View(x.201011.list$census)


### 2009/10 specific processing based on comments
x.200910.list$header <- x.200910.list$header %>% 
  mutate(census_date_start = if_else(census_date_start == dmy("12-Dec-09"), 
                                     ymd("2009-12-11"), census_date_start), 
         notes = case_when( 
           census_date_start == as.Date("2009-11-21") ~ 
             paste0(notes, 
                    ". Excel sheet had comments specifying who surveyed which beach"), 
           TRUE ~ notes))

beach.11dec09 <- c("Ballena Sur", "Ballena Norte", "La Caverna", 
                   "Bahamonde", "Nibaldo", "Roquerio")
beach.21nov09.rb <- c("San Telmo, Punta", "Maderas", "Copi", "Chungungo", 
                      "Ballena Norte")
beach.21nov09.meg <- c("Marko", "Larga", "Yuseff, Punta", "Media Luna")
beach.21nov09.rmb <- c("Loberia", "Angosta, Playa", "Pinochet de la Barra", 
                       "Alcazar", "Nibaldo", "Bahamonde")
x.200910.list$census <- x.200910.list$census %>% 
  mutate(time_start = case_when(
    census_date == dmy("21-Nov-09") & Beach %in% beach.21nov09.rb ~ "11:30:00", 
    census_date == dmy("21-Nov-09") & Beach %in% beach.21nov09.meg ~ "11:40:00", 
    census_date == dmy("21-Nov-09") & Beach %in% beach.21nov09.rmb ~ "11:30:00", 
    TRUE ~ time_start
  ), 
  time_end = case_when(
    census_date == dmy("21-Nov-09") & Beach %in% beach.21nov09.rb ~ "14:30:00", 
    census_date == dmy("21-Nov-09") & Beach %in% beach.21nov09.meg ~ "13:10:00", 
    census_date == dmy("21-Nov-09") & Beach %in% beach.21nov09.rmb ~ "14:35:00", 
    TRUE ~ time_end
  ), 
  census_date = case_when(
    census_date == ymd("2009-12-12") & Beach %in% beach.11dec09 ~ ymd("2009-12-11"), 
    TRUE ~ census_date
  ), 
  observer = case_when(
    census_date == dmy("21-Nov-09") & Beach %in% beach.21nov09.rb ~ "RB", 
    census_date == dmy("21-Nov-09") & Beach %in% beach.21nov09.meg ~ "MEG", 
    census_date == dmy("21-Nov-09") & Beach %in% beach.21nov09.rmb ~ "RMB", 
    census_date == ymd("2009-12-11") & Beach %in% beach.11dec09 ~ "RB", 
    TRUE ~ NA_character_
  ))


#-------------------------------------------------------------------------------
# Import data to ***REMOVED*** - done 8 March 2022

### Header data
header.all <- bind_rows(x.200910.list$header, x.201011.list$header)
header.toimport <- header.all %>% select(-week)

# DBI::dbAppendTable(con, "census_phocid_header", header.toimport)


### Get header IDs for census records
db.header <- tbl(con, "census_phocid_header") %>% 
  collect() %>% 
  mutate(census_date_start = as.Date(census_date_start), 
         census_date_end = as.Date(census_date_end))

### Join with census and header records, process, and import
x.200910.census <- x.200910.list$census %>% 
  left_join(select(inner_join(db.header, x.200910.list$header, 
                              by = c("census_date_start", "census_date_end",
                                     "surveyed_san_telmo", "notes")), 
                   week, census_phocid_header_id), 
            by = "week") %>% 
  mutate(census_type = "Phocid") %>% 
  select(census_phocid_header_id, census_type, everything(), 
         -c(week, Beach, count_sum, count_allzero))

x.201011.census <- x.201011.list$census %>% 
  left_join(select(inner_join(db.header, x.201011.list$header, 
                              by = c("census_date_start", "census_date_end",
                                     "surveyed_san_telmo", "notes")), 
                   week, census_phocid_header_id), 
            by = "week") %>% 
  mutate(census_type = "Phocid") %>% 
  select(census_phocid_header_id, census_type, everything(), 
         -c(week, Beach, count_sum, count_allzero))

census.toimport <- bind_rows(x.200910.census, x.201011.census)
stopifnot(
  nrow(census.toimport) == nrow(x.200910.list$census) + nrow(x.201011.list$census)
)
# DBI::dbAppendTable(con, "census", census.toimport)


#------------------------------------------------
### 9 Oct 2022: Update 2009/10 records with times
# 21 Nov 2009 are (were) only record in DB with times

# con.update <- dbConnect(odbc(), Driver = "ODBC Driver 18 for SQL Server",
#                         Server = "swc-***REMOVED***-s", 
#                         Database = "***REMOVED***",
#                         Trusted_Connection = "Yes", Encrypt = "optional")

x.toupdate <- tbl(con.update, "census") %>% 
  filter(census_type == "Phocid", 
         between(census_date, "2009-10-01", "2010-05-01"), 
         is.na(time_start)) %>%
  select(ID, census_date, Beach_ID, species) %>% 
  collect() %>% 
  mutate(census_date = as.Date(census_date))

x.toupdate.merge <- x.toupdate %>% 
  left_join(select(x.200910.census, census_date, Beach_ID, species, 
                   time_start, time_end), 
            by = c("census_date", "Beach_ID", "species"))
# mutate(sql_update = glue::glue("select *, ", 
#                                "time_start = '{time_start}', ", 
#                                "time_end = '{time_end}' ", 
#                                "from census where ID = {ID}"))


update <- dbSendQuery(
  con.update, 'UPDATE census SET "time_start"=?, "time_end"=? WHERE ID=?'
)
dbBind(update, select(x.toupdate.merge, time_start, time_end, ID))  # send the updated data



################################################################################
################################################################################
# Compare Excel and DB data from 2011/12 and 2012/13

#-------------------------------------------------------------------------------
### Confirm that data for 2012/13 was simply exported from the database, 
###   and thus this comparison is not useful. (confirmed 8 Mar 2022)

x.201213 <- x.201213.orig %>% 
  rename(census_date = Date, location = beach, 
         ad_female_count = female_count, ad_male_count = male_count, 
         ad_unk_count = unknown_count, juv_unk_count = juv_unknown_count, 
         pup_live_count = pups_live_count, pup_dead_count = pups_dead_count, 
         notes_census = notes, created_dt_census = created_dt) %>% 
  mutate(species = str_to_sentence(species), 
         location = case_when( #For matching with beaches.Name value
           location == "Punta Delphin to La Caverna" ~ "Punta Delphin-La Caverna", 
           location == "Playa Golondrina to P. del Lobero" ~ "Golondrina-del Lobero", 
           location == "Playa Paulina to Aranda" ~ "Paulina-Aranda", 
           TRUE ~ location
         )) %>% 
  mutate_location() %>% 
  rowwise() %>% 
  mutate(count_sum = sum(c_across(ad_female_count:pup_dead_count), 
                         na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(x.201213.orig.wx, by = c("census_wx_id" = "ID")) %>% 
  rename(notes_wx = notes, created_dt_wx = created_dt) %>% 
  mutate(census_date = ob_date)


func_201213_summ <- function(x) {
  x %>% 
    mutate(str_unique = paste(census_date, species, location, sep = "_")) %>% 
    group_by(census_date, species, location, str_unique) %>% 
    summarise(num_records = n(), 
              across(contains("_count"), sum, na.rm = TRUE), 
              .groups = "drop")
}

ex.summ <- x.201213 %>% func_201213_summ()
db.summ <- census.phocid %>%
  filter(season_name == "2012/13") %>% 
  func_201213_summ()

ex.summ %>% filter(!(str_unique %in% db.summ$str_unique)) %>% glimpse()
db.summ %>% filter(!(str_unique %in% ex.summ$str_unique)) %>% glimpse()
# Differences:
#   Two differences due to different (corrected) beach names
#   ex.summ has one record for location 'Ridge wallow', which was imported as Media Luna

join.summ <- inner_join(ex.summ, db.summ)
nojoin.summ <- bind_rows(ex.summ, db.summ) %>% 
  filter(!(str_unique %in% join.summ$str_unique))
# ^ all differences are accounted for and can be disregarded



#-------------------------------------------------------------------------------
# 2011/12 comparison and reimport. Reimport done on 1 April 2022
# NOTE 29 June 2022: 16 duplicate records were imported for 2011/12. 
#   These were deleted using Access and history note added in 2019/20 Issue 56d
x.201112.list <- census_proc(x.201112.orig)
y.201112 <- census.phocid %>% filter(season_name == "2011/12")


##### Compare raw and DB data from 2011/12
x.201112.header <- x.201112.list$header
y.201112.header <- tbl(con, "census_phocid_header") %>% 
  filter(between(census_date_start, as.Date("2011-10-01"), as.Date("2012-04-01"))) %>% 
  collect() %>% 
  mutate(census_date_start = as.Date(census_date_start), 
         census_date_end = as.Date(census_date_end))


### 2011/12
x.201112 <- x.201112.list$census %>% 
  # left_join(rename(x.201112.list$header, header_notes = notes), 
  #           by = "week") %>% 
  rename(location = Beach)

# No dups from Excel
x.201112 %>% mutate(season_info_id = 0) %>% census_dup()


# Join by date + location + species + 
z.201112 <- full_join(
  y.201112 %>% 
    select(census_id, census_date, location, species, 
           ad_female_count:pup_live_count, count_sum), 
  x.201112 %>% 
    select(week, census_date, location, species, 
           ad_female_count:pup_live_count, count_sum), 
  by = c("census_date", "location", "species",
         "ad_female_count", "ad_male_count", "ad_unk_count",
         "juv_female_count", "juv_male_count", "juv_unk_count",
         "pup_live_count", "count_sum")
) %>% 
  select(census_id, week, everything())

# ISSUES
z.201112.mismatch <- z.201112 %>% 
  filter(is.na(census_id) | is.na(week) | 
           duplicated(paste(census_date, location, species))) %>% 
  # Records on 27-28 Jan are fine - db has actual (differentiated) date, cool
  filter(!(census_date %in% c(as.Date("2012-01-27"), as.Date("2012-01-28")))) %>% 
  arrange(census_date, location, species)

# googlesheets4::write_sheet(
#   z.201112.mismatch, sheet = "phocid_mismatch_2011/12", 
#   ss = "https://docs.google.com/spreadsheets/d/1naKQhjKdv8JsuiG3VANkeAk7uYtYYgSRgjIi7dQMObk"
# )

# dup.201112 <- y.201112 %>% census_dup() #all captured above now



##### Get observer values, prep for re-import
obs.201112 <- y.201112 %>% 
  select(observer, census_date, Beach = location, species, ad_female_count:pup_live_count)
tmp.beach <- c("Yamana, Playa", "Golondrina-del Lobero", "Paulina-Aranda", "Larga", 
               "Media Luna", "Yuseff, Punta", "Modulo South")

z <- x.201112.list$census %>% 
  mutate(census_date = case_when(
    census_date == as.Date("2012-01-28") & (Beach %in% tmp.beach) ~ as.Date("2012-01-27"), 
    TRUE ~ census_date
  )) %>% 
  select(-observer) %>% 
  left_join(obs.201112, by = c("census_date", "Beach", "species",
                               "ad_female_count", "ad_male_count", "ad_unk_count",
                               "juv_male_count", "juv_female_count", "juv_unk_count",
                               "pup_live_count")) %>% 
  mutate(observer = case_when(
    census_date == as.Date("2011-11-23") & is.na(observer) ~ "JRW", 
    census_date == as.Date("2011-11-23") & observer == "KWP" & 
      Beach %in% c("Bahamonde", "Nibaldo", "Paulina-Aranda", "San Telmo, Punta") ~ "JRW", 
    is.na(observer) & census_date == as.Date("2011-12-30") ~ "JRW", 
    Beach == "Cachorros East" & census_date == as.Date("2012-01-20") ~ "JRW", 
    Beach != "Cachorros East" & observer == "JRW" & 
      census_date == as.Date("2012-01-20") ~ "DJK", 
    TRUE ~ observer
  )) %>% 
  select(week, observer, everything())

# Sanity checks to ensure no data will be unintentionally dropped
z %>% filter(is.na(observer))

summ1 <- y.201112 %>% 
  group_by(census_date, location, species) %>% 
  summarise(count = n(), count_sum_sum = sum(count_sum), 
            id1 = "summ1", .groups = "drop")
summ2 <- z %>% 
  group_by(census_date, location = Beach, species) %>% 
  summarise(count = n(), count_sum_sum = sum(count_sum), 
            id2 = "summ2", .groups = "drop")

waldo::compare(summ1, summ2)
summ <- full_join(summ1, summ2, 
                  by = c("census_date", "location", "species", "count",
                         "count_sum_sum")) %>% 
  filter(is.na(id1) | is.na(id2)) %>% 
  arrange(census_date, location, species)


### Join with census and header records, process, and import
db.header <- tbl(con, "census_phocid_header") %>% 
  collect() %>% 
  mutate(census_date_start = as.Date(census_date_start), 
         census_date_end = as.Date(census_date_end))

census.toimport <- z %>% 
  left_join(select(db.header, census_date = census_date_start, census_phocid_header_id), 
            by = "census_date") %>% 
  mutate(census_type = "Phocid", 
         census_phocid_header_id = if_else(
           census_date == as.Date("2012-01-28"), 
           db.header$census_phocid_header_id[db.header$census_date_end == as.Date("2012-01-28")], 
           census_phocid_header_id)) %>% 
  select(census_phocid_header_id, census_type, everything(), 
         -c(week, Beach, count_sum, count_allzero))

census.toimport %>% 
  group_by(census_phocid_header_id) %>% 
  summarise(count = n(), 
            dates = paste(sort(unique(census_date)), collapse = ", "), 
            .groups = "drop")

stopifnot(
  all(!is.na(census.toimport$census_phocid_header_id)), 
  nrow(census.toimport) == nrow(z)
)

# # delete census records, then add
# sql.base <- paste("from census", "where census_type = 'Phocid' AND",
#                   "census_date BETWEEN '2011-10-01' AND '2012-04-01'")
# rs <- dbSendQuery(con, paste("select *", sql.base))
# d <- dbFetch(rs)
# dbClearResult(rs)
# 
# rs <- dbSendQuery(con, paste("DELETE", sql.base))
# dbHasCompleted(rs)
# dbClearResult(rs)
# 
# DBI::dbAppendTable(con, "census", census.toimport)

# # update history records
# sql.history.base <- paste(
#   "history_notes = 'Reimported from Excel file'", 
#   "from history_census", 
#   "where ABS(DATEDIFF(hour, GETDATE(), history_dt)) < 2 AND history_chg_type = 'DELETE'"
# )
# rs <- dbSendQuery(con, paste("select *,", sql.history.base))
# dbFetch(rs); dbClearResult(rs)
# 
# rs <- dbSendQuery(con, paste("UPDATE history_census SET", sql.history.base))
# dbHasCompleted(rs); dbClearResult(rs)

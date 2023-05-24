library(tidyverse)
library(here)
library(amlrPinnipeds)
library(glue)
library(ggExtra)

### Read in data
con <- amlr_dbConnect(Database = "***REMOVED***")

x <- read.csv(here("data/cs_combined_data/phocids_cs_combined_long.csv"), 
              na.strings = "") %>% 
  mutate(census_date = as.Date(census_date))
x.header <- read.csv(here("data/cs_combined_data/phocids_cs_combined_header.csv"))


### Let's go exploring!
x.summ.date <- x %>% 
  group_by(season_name) %>% 
  summarise(n_dates = n_distinct(census_date), 
            min_date = min(census_date), 
            max_date = max(census_date))


### Make a visualization of census dates by year from the header data
d <- x.header %>% 
  mutate(season_name = as.factor(season_name), 
         season_int = as.numeric(substr(season_name, 1, 4)), 
         research_program = as.factor(research_program), 
         census_date_start = as.Date(census_date_start), 
         census_date_end = as.Date(census_date_end), 
         # census_month_day = format(census_date_start, "%b-%d"), 
         # census_yday = yday(census_date_start), 
         tmp_year = if_else(month(census_date_start) >= 7, 1999, 2000), 
         tmp_date = mdy(format(census_date_start, glue("%m-%d-{tmp_year}"))))


g <- ggplot(d, aes(x = tmp_date, y = season_int)) +
  geom_point() +
  labs(title = "Dates of Cape Shirreff phocid census by season", 
       x = "Date", y = "Season") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + 
  scale_y_continuous(breaks = unique(d$season_int), labels = unique(d$season_name))
g
g <- ggMarginal(g, type = "histogram", fill="transparent", margins = "y")
g
ggsave(here("output", "census_timing.png"), g, width = 10, height = 6)

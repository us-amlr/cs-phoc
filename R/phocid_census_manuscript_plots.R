# Make plots!

library(tidyverse)
library(stringr)
library(here)
library(amlrPinnipeds)
library(glue)
library(cowplot)

con <- amlr_dbConnect(Database = "***REMOVED***")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Read in data, and do some prep
z.header <- read_csv(here("output", "cspc_header.csv"), 
                     col_types = "ccDDilc")

z <- read_csv(here("output", "cspc_records.csv"), 
              col_types = "cccciiiiiiiiiii")

yr.pad.all <- str_pad(c(98, 99, 00, 1:23), width = 2, pad = "0")
# season.name.levels <- paste(1997:2022, yr.pad.all, sep = "/")
season.name.levels.inach <- paste(1997:2008, yr.pad.all[1:12], sep = "/")
season.name.levels.amlr <- paste(2009:2022, yr.pad.all[13:26], sep = "/")
season.name.levels <- c(season.name.levels.inach, season.name.levels.amlr)

header.toplot <- z.header %>% 
  mutate(season_name = fct(season_name, levels = season.name.levels), 
         # season_int = as.numeric(substr(season_name, 1, 4)), 
         research_program = as.factor(research_program), 
         census_date_start = as.Date(census_date_start), 
         census_date_end = as.Date(census_date_end), 
         plot_year = if_else(month(census_date_start) >= 7, 1999, 2000), 
         plot_date_start = mdy(format(census_date_start, 
                                      glue("%m-%d-{plot_year}"))))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### Make a visualization of census dates by year from the header data
g1 <- ggplot(header.toplot, aes(x = plot_date_start, y = season_name)) +
  geom_point(aes(col = research_program,
                 # shape = surveyed_san_telmo, 
                 size = census_days)) +
  labs(title = "Cape Shirreff phocid census surveys, by season", 
       x = "Date", y = "Season") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  guides(color = guide_legend(title = "Research program", order = 1),
         # shape = guide_legend(title = "PST", order = 3), 
         size = guide_legend(title = "Census days", order = 2)) + 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + 
  scale_y_discrete(drop = FALSE) + 
  scale_size(breaks = 1:3)
g1

# library(ggExtra)
# g1marg <- ggMarginal(g1, type = "histogram", fill="transparent", margins = "y")
# ggsave(here("output", "census_timing.png"), g1marg, width = 10, height = 6)

g2 <- ggplot(header.toplot, aes(x = season_name)) + 
  geom_histogram(stat = "count") + 
  scale_x_discrete(drop = FALSE) + 
  labs(y = "Count") + 
  coord_flip() +
  # theme_minimal() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
g2

g.grid <- plot_grid(g1, g2, rel_widths = c(8, 1), align = "h", axis = "tb")
g.grid
ggsave(here("output", "census_timing.png"), g.grid, width = 10, height = 6)

rm(g.grid, g1, g2)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Census timing
x <- tbl(con, "vCensus_Phocid") %>% 
  filter(!is.na(time_start), !is.na(time_end)) %>% 
  collect() %>% 
  mutate(census_dt_start = ymd_hms(paste(census_date, time_start)), 
         census_dt_end = ymd_hms(paste(census_date, time_end)), 
         census_interval = interval(census_dt_start, census_dt_end), 
         census_dt_midpoint = (
           int_start(census_interval) + 
             (int_end(census_interval) - int_start(census_interval))/2), 
         census_dt_midpoint_24h = as.numeric(
           format(census_dt_midpoint, format="%H")), 
         census_hours = as.numeric(difftime(census_dt_end, census_dt_start, 
                                            units = "hours")))

summary(x$census_date)
summary(x$census_hours)

gg.st.end <- x %>% 
  select(census_dt_start, census_dt_end) %>% 
  pivot_longer(c(census_dt_start, census_dt_end), 
               names_to = "dt_type", values_to = "dt") %>% 
  mutate(type = case_when(str_detect(dt_type, "start") ~ "start", 
                          str_detect(dt_type, "end") ~ "end"), 
         dt_hours = as.numeric(format(dt, format="%H"))) %>% 
  ggplot(aes(x=dt_hours, fill=type)) +
  geom_bar(alpha=0.6, position = 'identity') + 
  ggtitle("Census record start and end times") + 
  xlab("Hour (24h)") + 
  scale_x_continuous(limits = c(6, 22), breaks = 0:24, minor_breaks = NULL)


gg.mid <- ggplot(x, aes(census_dt_midpoint_24h)) + 
  geom_bar() + 
  ggtitle("Census record midpoint times") + 
  xlab("Hour (24h)") + 
  scale_x_continuous(limits = c(6, 22), breaks = 0:24, minor_breaks = NULL)

gg.hours <- ggplot(x, aes(census_hours)) + 
  geom_histogram(binwidth = 0.5) + 
  ggtitle("Census record survey windows (hours)") + 
  xlab("Hours")

plot_grid(gg.st.end, gg.mid, gg.hours, ncol = 1)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# For multi-day records, should we use the start or end date?
# Answer: based on quantity we should use the start date
date.test <- tbl(con, "vCensus_Phocid") %>% 
  filter(census_days > 1) %>% 
  collect() %>% 
  mutate(date_match = case_when((census_date_start == census_date) ~ "start", 
                                (census_date_end == census_date) ~ "end", 
                                .default = "nomatch")) 

tableNA(date.test$date_match)
date.test %>% 
  group_by(census_date_start) %>% 
  summarise(census_days = unique(census_days), 
            n_start = sum(date_match == "start"), 
            n_nomatch = sum(date_match == "nomatch"), 
            n_end = sum(date_match == "end"))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Join with header.toplot to get variables for plot axes
z.toplot <- left_join(z, header.toplot, by = join_by(header_id))
# i <- amlrPinnipeds::pinniped.phocid.sp[1]


#-------------------------------------------------------------------------------
# Core census locations
z.toplot.core <- z.toplot %>% filter(location == "Core census locations")


### Dot counts
# Try same as above, with counts controlled by dot size
for (i in amlrPinnipeds::pinniped.phocid.sp) {
  print(i)
  if (i == 'Elephant seal') i <- "Southern elephant seal"
  i.toplot <- z.toplot.core %>% filter(species_common == i)
  
  g.curr <- ggplot(i.toplot, aes(x = plot_date_start, y = season_name)) +
    geom_point(aes(size = total_count)) +
    labs(title = paste("Cape Shirreff phocid census counts", 
                       "Core census locations", i, sep = " - "),
         x = "Date", y = "Season") +
    theme(axis.text.x = element_text(angle = 90)) +
    guides(size = guide_legend(title = "Count")) + 
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_discrete(drop=FALSE)
  
  i.filename <- str_replace(tolower(i), " ", "_")
  ggsave(here("output", "scatterplot", paste0(i.filename, ".png")), 
         g.curr, width = 10, height = 6)
}; rm(i, i.toplot, g.curr)


### Line charts
# Try same as above, with counts controlled by dot size
for (i in amlrPinnipeds::pinniped.phocid.sp) {
  print(i)
  if (i == 'Elephant seal') i <- "Southern elephant seal"
  i.toplot <- z.toplot.core %>% filter(species_common == i)
  
  g.curr <- ggplot(i.toplot, aes(x = plot_date_start, y = total_count)) +
    geom_point(aes(color = season_name)) +
    geom_line(aes(color = season_name)) + 
    labs(title = paste("Cape Shirreff phocid census counts", 
                       "Core census locations", i, sep = " - "),
         x = "Date", y = "Count") +
    theme(axis.text.x = element_text(angle = 90)) +
    guides(color = guide_legend(title = "Season")) + 
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
  g.curr
  
  i.filename <- str_replace(tolower(i), " ", "_")
  ggsave(here("output", "line_graph", paste0(i.filename, ".png")), 
         g.curr, width = 10, height = 6)
}; rm(i, i.toplot, g.curr)


#-------------------------------------------------------------------------------
# Core census locations + PST; from 2009/10 on
cspc_sum <- function(x, na.rm = TRUE) {
  if_else(all(is.na(x)), NA_integer_, sum(x, na.rm = na.rm))
}

z.toplot.combo <- z %>% 
  group_by(header_id, species_common) %>% 
  summarise(location = "Core + PST", 
            across(ends_with("_count"), cspc_sum),  
            .groups = "drop") %>% 
  left_join(header.toplot, by = join_by(header_id)) %>% 
  filter(census_date_start > ymd("2009-07-01"), 
         surveyed_san_telmo) %>% 
  mutate(season_name = as.character(season_name), 
         season_name = fct(season_name, levels = season.name.levels.amlr))



### Dot counts
# Try same as above, with counts controlled by dot size
for (i in amlrPinnipeds::pinniped.phocid.sp) {
  print(i)
  if (i == 'Elephant seal') i <- "Southern elephant seal"
  i.toplot <- z.toplot.combo %>% filter(species_common == i)
  
  g.curr <- ggplot(i.toplot, aes(x = plot_date_start, y = season_name)) +
    geom_point(aes(size = total_count)) +
    labs(title = paste("Cape Shirreff phocid census counts", 
                       "Core+PST locations", i, sep = " - "),
         x = "Date", y = "Season") +
    theme(axis.text.x = element_text(angle = 90)) +
    guides(size = guide_legend(title = "Count")) + 
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_discrete(drop=FALSE)
  
  i.filename <- paste0("core+pst_", str_replace(tolower(i), " ", "_"))
  ggsave(here("output", "scatterplot", paste0(i.filename, ".png")), 
         g.curr, width = 10, height = 6)
}; rm(i, i.toplot, g.curr)


### Line charts
# Try same as above, with counts controlled by dot size
for (i in amlrPinnipeds::pinniped.phocid.sp) {
  print(i)
  if (i == 'Elephant seal') i <- "Southern elephant seal"
  i.toplot <- z.toplot.combo %>% filter(species_common == i)
  
  g.curr <- ggplot(i.toplot, aes(x = plot_date_start, y = total_count)) +
    geom_point(aes(color = season_name)) +
    geom_line(aes(color = season_name)) + 
    labs(title = paste("Cape Shirreff phocid census counts", 
                       "Core+PST locations", i, sep = " - "),
         x = "Date", y = "Count") +
    theme(axis.text.x = element_text(angle = 90)) +
    guides(color = guide_legend(title = "Season")) + 
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
  g.curr
  
  i.filename <- paste0("core+pst_", str_replace(tolower(i), " ", "_"))
  ggsave(here("output", "line_graph", paste0(i.filename, ".png")), 
         g.curr, width = 10, height = 6)
}; rm(i, i.toplot, g.curr)

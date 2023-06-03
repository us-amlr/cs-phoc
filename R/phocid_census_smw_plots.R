# Make plots!

library(tidyverse)
library(here)
library(amlrPinnipeds)
library(glue)
library(cowplot)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Read in data, and do some prep
z.header <- read_csv(here("output", "cs_phocid_census_header.csv"), 
                     col_types = "ccDDilc")

z <- read_csv(here("output", "cs_phocid_census_records.csv"), 
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

# Figures for CS-PHOC data paper
# NOTE: 'header' and 'event' may be used interchangeably in this script

library(ggplot2)
library(ggspatial)
library(cowplot)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(here)
library(viridis)
library(stringr)
library(tamatoamlr)
library(glue)
library(readr)
library(lubridate)
library(tidyr)
library(odbc)

save.image <- TRUE

viridis.twocolor <- viridis(3)[1:2]
csphoc.labels.size = 15

here.csv <- here("data", "manuscript")
here.fig.tbl <- here("figures")
here.fig.other <- here("figures", "other")



###############################################################################
###############################################################################
# Code to create CS map (Figure 1)

#-------------------------------------------------------------------------------
# Make two medium res maps

### Get object from rnaturalearthhires, and set common variables
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

g.map <- ggplot(data = world) +
  geom_sf() +
  theme_bw()

# g.map
# crs.laea <- "+proj=laea +lat_0=-75 +lon_0=-15 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "
crs.laea <- paste("+proj=laea +lat_0=-90 +lon_0=-15 +x_0=0 +y_0=0", 
                  "+ellps=GRS80 +units=m +no_defs")

# Define limits
xlim.ssi <- c(-63.0, -57)
ylim.ssi <- c(-64, -61.5)
xlim.cs <- c(-60.9, -60.6)
ylim.cs <- c(-62.56, -62.45)

ssi.sfc.4326 <- st_as_sfc(st_bbox(c(xmin = xlim.ssi[1], xmax = xlim.ssi[2], 
                                    ymax = ylim.ssi[2], ymin = ylim.ssi[1]), 
                                  crs = st_crs(4326)))
ssi.sfc.laea <- st_transform(ssi.sfc.4326, crs.laea)


### Map of whole region
g.region <- g.map +
  coord_sf(crs = crs.laea, expand = FALSE, 
           xlim = c(-4070000, 4070000), ylim = c(-4000000, 4000000)) + 
  geom_rect(xmin = min(st_coordinates(ssi.sfc.laea)[, "X"]), 
            xmax = max(st_coordinates(ssi.sfc.laea)[, "X"]), 
            ymin = min(st_coordinates(ssi.sfc.laea)[, "Y"]), 
            ymax = max(st_coordinates(ssi.sfc.laea)[, "Y"]), 
            fill = NA, colour = "black", linewidth = 1) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        # panel.border = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


### Map of SSI
g.ssi <- g.map + 
  coord_sf(xlim = xlim.ssi, ylim = ylim.ssi, 
           crs = st_crs(4326), expand = FALSE) + 
  geom_rect(xmin = min(xlim.cs), xmax = max(xlim.cs), 
            ymin = min(ylim.cs), ymax = max(ylim.cs), 
            fill = NA, colour = "black", linewidth = 0.5) + 
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotate(geom = "text", x = -58.4, y = -63.25, label = "Antarctic Peninsula", 
           fontface = "italic", color = "black", size = 4, angle = 31) + 
  annotate(geom = "text", x = -58.7, y = -62.5, label = "South Shetland Islands", 
           fontface = "italic", color = "black", size = 4, angle = 31) + 
  annotate(geom = "text", x = -61.4, y = -62.38, label = "Cape Shirreff",
           fontface = "italic", color = "black", size = 3, angle = 0) +
  xlab(NULL) +
  ylab(NULL)



### Map of Cape Shirreff

# # Initial attempt using a marked up PNG file to indicate locations
# # Deprecated in favor of code-driven plot below
# cs.image <- grid::rasterGrob(
#   png::readPNG("C:/SMW/Pinnipeds/CS-PHOC/manuscript-scidata/CS map - shaded.png"),
#   width=ggplot2::unit(1,"npc"),
#   height=ggplot2::unit(1,"npc")
# )
# g.cs <- ggplot() + 
#   annotation_custom(cs.image, -Inf, Inf, -Inf, Inf) +
#   geom_text()

# Code-driven plot of CS and locations
cs.poly <- st_read(file.path("../CS Polygon and fur seal beaches_Adahood", 
                             "Inach_Shirreff_poly.shp"))
cs.line <- st_cast(st_geometry(cs.poly), "LINESTRING")
pst.sf <- st_linestring(matrix(c(c(613900, 613500, 612800, 612780), 
                                 c(3070200, 3069940, 3070000, 3069850)), 
                               ncol = 2)) %>% 
  st_sfc(crs = st_crs(cs.line)) %>% 
  st_sf(data.frame(type = "Punta San Telmo")) %>% 
  st_set_geometry("geometry")

# Create polygons with which to intersect linestrings to draw beaches as a layer
minmax_to_sfc <- function(x1, x2, y1, y2, crs) {
  x.df <- data.frame(lon = c(x1, x2, x2, x1, x1), lat = c(y1, y1, y2, y2, y1))
  st_sfc(
    st_polygon(list(matrix(c(x.df$lon, x.df$lat), ncol = 2))), 
    crs = crs
  )
}

y1.min <- 3069990
cs.corepolys.sfc <- c(
  st_sfc(
    st_polygon(list(matrix(c(c(615000, 614775, 614200, 615000, 615000), 
                             c(y1.min, y1.min, 3070300, 3070300, y1.min)), 
                           ncol = 2))), 
    crs = st_crs(cs.line)
  ), 
  minmax_to_sfc(614400, 615300, 3070300, 3071680, st_crs(cs.line)), 
  minmax_to_sfc(614400, 615300, 3071810, 3072000, st_crs(cs.line)), 
  minmax_to_sfc(614000, 614400, 3071810, 3071980, st_crs(cs.line)),
  
  
  minmax_to_sfc(614000, 614210, 3072020, 3072400, st_crs(cs.line)), 
  minmax_to_sfc(613400, 614210, 3072400, 3072750, st_crs(cs.line)), 
  minmax_to_sfc(612800, 613400, 3072000, 3072260, st_crs(cs.line)),
  minmax_to_sfc(612800, 614100, 3070220, 3071900, st_crs(cs.line))
)
# plot(st_geometry(cs.line), axes = TRUE); plot(cs.corepolys.sfc, add = TRUE)

cs.core <- st_intersection(cs.corepolys.sfc, cs.line) %>% 
  st_sf(data.frame(type = "Core census locations")) %>% 
  st_set_geometry("geometry")

sf.locations <- bind_rows(cs.core, pst.sf)

g.cs <- ggplot() +
  geom_sf(data = cs.poly) +
  geom_sf(data = sf.locations, aes(col = type, fill = type), lwd = 3) + 
  scale_color_manual(values = viridis.twocolor) +
  scale_fill_manual(values = viridis.twocolor) +
  coord_sf(crs = st_crs(4326)) +
  guides(fill = guide_legend(title = NULL), 
         color = guide_legend(title = NULL)) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         height = unit(2, "cm"), width = unit(2, "cm"),
                         style = north_arrow_fancy_orienteering)


# Grid
grid.map <- plot_grid(
  plot_grid(g.region, g.ssi, nrow = 2, align = "v", axis = "lr", 
            labels = c("a", "b"), label_size = csphoc.labels.size), 
  g.cs, ncol = 2, rel_widths = c(1, 1.3), 
  labels = c(NA, "c"), label_size = csphoc.labels.size
)

if (save.image) {
  ggsave(here("figures", "Fig1_csphoc_map.pdf"), 
         grid.map, width = 9, height = 6, bg = "white") 
} else {
  print(grid.map)
}



###############################################################################
###############################################################################
# General code for data-dependent figures

### Functions for plot colors
scale_color_csphoc <- function() {
  # scale_color_npg()
  # scale_color_brewer(palette = "Set2")
  scale_color_viridis(discrete = TRUE)
}
scale_fill_csphoc <- function() {
  # scale_fill_npg()
  # scale_fill_brewer(palette = "Set2")
  scale_fill_viridis(discrete = TRUE)
}


### Read in data
z.header <- read_csv(here(here.csv, "cs-phoc-events.csv"), 
                     col_types = "ccDDilc")
nrow(z.header)

z.count <- read_csv(here(here.csv, "cs-phoc-counts.csv"), 
                    col_types = "ccccciiiiiiiiiii")


### Make general data frames and plotting variables
programs.seasons <- z.header %>% 
  select(season_name, research_program) %>% 
  distinct()
season.name.levels.inach <- programs.seasons %>% 
  filter(research_program == "INACH") %>% 
  pull(season_name)
season.name.levels.amlr <- programs.seasons %>% 
  filter(research_program == "USAMLR") %>% 
  pull(season_name)
season.name.levels <- c(season.name.levels.inach, season.name.levels.amlr)

header.toplot <- z.header %>% 
  mutate(season_name = factor(season_name, levels = season.name.levels), 
         # season_int = as.numeric(substr(season_name, 1, 4)), 
         research_program = as.factor(research_program), 
         census_date_start = as.Date(census_date_start), 
         census_date_end = as.Date(census_date_end), 
         plot_year = if_else(month(census_date_start) >= 7, 1999, 2000), 
         plot_date_start = mdy(format(census_date_start, 
                                      glue("%m-%d-{plot_year}"))))

count.toplot <- left_join(z.count, header.toplot, by = join_by(event_id))
count.toplot.core <- count.toplot %>% 
  filter(location == "Core census locations")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Fig 2: Make a visualization of census dates by year from the header data
g1 <- ggplot(header.toplot, aes(x = plot_date_start, y = season_name)) +
  geom_point(aes(col = research_program, size = census_days)) +
  labs(x = "Date", y = "Season") + 
  scale_y_discrete(drop = FALSE) + 
  scale_size(breaks = 1:3) +
  scale_color_manual(values = viridis.twocolor) +
  guides(color = guide_legend(title = "Research program", order = 1),
         size = guide_legend(title = "Survey window (days)", order = 2)) +  
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")
# g1

g2 <- ggplot(header.toplot, aes(x = season_name)) + 
  geom_histogram(stat = "count") + 
  scale_x_discrete(drop = FALSE) + 
  labs(y = "Number of censuses") + 
  coord_flip() + 
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# g2

g.grid <- plot_grid(g1, g2, rel_widths = c(8, 1.5), align = "h", axis = "tb")
# g.grid

if (save.image) {
  ggsave(here(here.fig.tbl, "Fig2_csphoc_censuses2.pdf"), 
         g.grid, width = 10, height = 6)
} else {
  print(g.grid)
}

rm(g.grid, g1, g2)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Fig 3: Census timing - start and end times
con <- dbConnect(odbc(), filedsn = here("dsn", "amlr-pinniped-db-prod.dsn"))

csphoc.times <- tbl(con, "vCensus_Phocid") %>% 
  filter(!is.na(time_start), !is.na(time_end)) %>% 
  collect() %>% 
  mutate(census_dt_start = ymd_hms(paste(census_date, time_start)), 
         census_dt_end = ymd_hms(paste(census_date, time_end)), 
         census_interval = interval(census_dt_start, census_dt_end), 
         census_dt_midpoint = (
           int_start(census_interval) + 
             (int_end(census_interval) - int_start(census_interval))/2), 
         census_dt_midpoint_24h = as.numeric(
           format(round(census_dt_midpoint, units="hours"), format="%H")), 
         census_hours = as.numeric(difftime(census_dt_end, census_dt_start, 
                                            units = "hours")), 
         census_hours_bar = round(census_hours/0.5) * 0.5)

# summary(csphoc.times$census_date)
# summary(csphoc.times$census_hours)
nrow(csphoc.times)

gg.st.end <- csphoc.times %>% 
  select(census_dt_start, census_dt_end) %>% 
  pivot_longer(c(census_dt_start, census_dt_end), 
               names_to = "dt_type", values_to = "dt") %>% 
  mutate(type = case_when(str_detect(dt_type, "start") ~ "start", 
                          str_detect(dt_type, "end") ~ "end"), 
         type = factor(type, levels = c("start", "end")), 
         dt_hours = as.numeric(format(dt, format="%H"))) %>% 
  ggplot(aes(x=dt_hours, fill=type)) +
  geom_bar(alpha=0.6, position = 'identity') + 
  guides(fill = guide_legend(title = "Time")) + 
  # scale_fill_discrete(limits = c("start", "end")) + 
  ggtitle("Start and end times") + 
  xlab("Hour (24h)") + 
  ylab("Number of records") + 
  scale_x_continuous(limits = c(6, 22), breaks = 0:24, minor_breaks = NULL) +
  scale_fill_csphoc() +
  theme_bw()


gg.mid <- ggplot(csphoc.times, aes(census_dt_midpoint_24h)) + 
  geom_bar() + 
  ggtitle("Midpoint times") + 
  xlab("Hour (24h)") + 
  ylab("Number of records") + 
  scale_x_continuous(limits = c(6, 22), breaks = 0:24, minor_breaks = NULL) +
  theme_bw()

gg.hours <- ggplot(csphoc.times, aes(census_hours_bar)) + 
  geom_bar() + 
  ggtitle("Durations") + 
  xlab("Hours") + 
  ylab("Number of records") + 
  scale_x_continuous(breaks = 0:10) +
  theme_bw()

g.timing <- plot_grid(
  gg.st.end, gg.mid, gg.hours, ncol = 1, 
  labels = c("a", "b", "c"), label_size = csphoc.labels.size
)
# g.timing

if (save.image) {
  ggsave(here(here.fig.tbl, "Fig3_csphoc_survey_times.pdf"), 
         g.timing, width = 5, height = 10)
} else {
  print(g.timing)
}

rm(con, csphoc.times, g.timing, gg.st.end, gg.mid, gg.hours)

#-------------------------------------------------------------------------------
# For multi-day records, should we use the start or end date? Answer below
con <- dbConnect(odbc(), filedsn = here("dsn", "amlr-pinniped-db-prod.dsn"))

date.test <- tbl(con, "vCensus_Phocid") %>% 
  filter(census_days > 1) %>% 
  collect() %>% 
  mutate(date_match = case_when((census_date_start == census_date) ~ "start", 
                                (census_date_end == census_date) ~ "end", 
                                .default = "nomatch")) #due to 3-day census

# Answer: based on quantity, we should use the start date
tableNA(date.test$date_match) 

date.test %>% 
  group_by(census_date_start) %>% 
  summarise(census_days = unique(census_days), 
            n_start = sum(date_match == "start"), 
            n_nomatch = sum(date_match == "nomatch"), 
            n_end = sum(date_match == "end"))

rm(con, date.test)





#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Fig 4: Group counts by month and season group, before plotting
season.groups <- function(i) {
  case_when(
    i %in% c("1997/98", "1998/99", "1999/00", "2000/01", "2001/02") ~ "1997-2002", 
    i %in% c("2002/03", "2003/04", "2004/05", "2005/06", "2006/07") ~ "2002-2007", 
    i %in% c("2009/10", "2010/11", "2011/12", "2012/13") ~ "2009-2013", 
    i %in% c("2013/14", "2014/15", "2015/16", "2016/17") ~ "2013-2017", 
    i %in% c("2017/18", "2018/19", "2019/20", "2021/22", "2022/23") ~ "2017-2023"
  )
}


x <- count.toplot.core %>% 
  mutate(census_month = month(census_date_start), 
         census_week = week(census_date_start), 
         season_group = season.groups(season_name))

x.grp <- x %>% 
  group_by(location, species, species_common, season_group, census_month) %>% 
  summarise(total_count_mean = mean(total_count , na.rm = TRUE), 
            total_count_sd = sd(total_count), 
            .groups = "drop") %>% 
  mutate(total_count_sd = replace_na(total_count_sd, 0), 
         plot_year = if_else(census_month >= 7, 1999, 2000), 
         plot_date_start = mdy(glue("{census_month}-01-{plot_year}")))

g.grp <- x.grp %>% 
  filter(census_month %in% c(11, 12, 1, 2)) %>% 
  ggplot(aes(x = plot_date_start, y = total_count_mean, 
             color = season_group, group = season_group)) +
  # geom_point() +
  geom_point(aes(size = total_count_sd)) +
  geom_line() +
  facet_wrap(vars(species), nrow = 1, scales = "free_y") + 
  xlab("Month") + ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        strip.text = element_text(face = "italic")) +
  guides(color = guide_legend(title = "Season group", order = 1), 
         size = guide_legend(title = "Count sd")) +
  scale_color_csphoc()
# scale_x_date(date_breaks = "1 week", date_labels = "%b %d")



#------------------------------------------------
# Plot: season on x axis, count on y, color-coded by species
months.curr <- c(11, 12, 1, 2)
y.month <- count.toplot.core %>% 
  mutate(census_month = month(census_date_start)) %>% 
  filter(census_month %in% months.curr) %>% 
  mutate(census_month = factor(census_month, levels = months.curr, 
                               labels = month.name[months.curr])) %>% 
  group_by(location, species, species_common, season_name, census_month) %>% 
  summarise(total_count_mean = mean(total_count , na.rm = TRUE), 
            total_count_sd = sd(total_count), 
            .groups = "drop") %>% 
  mutate(total_count_sd = replace_na(total_count_sd, 0))


legend.italic <- element_text(face = "italic", size = 9)

# Plot without SES for sake of scale
g.month.clw <- y.month %>% 
  filter(!(species_common %in% "Southern elephant seal")) %>%
  ggplot(aes(season_name, total_count_mean, 
             color = species, group = species)) +
  geom_point() +
  geom_line() + 
  geom_errorbar(aes(ymin = total_count_mean-total_count_sd,
                    ymax = total_count_mean+total_count_sd),
                width = 0.3) +
  facet_wrap(vars(census_month), nrow = 1) + 
  xlab("Season") + ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  guides(color = guide_legend(title = "Species", 
                              label.theme = legend.italic)) +
  scale_color_csphoc()

#------------------------------------------------
# Plot elephant seals, with age/sex class
y.long <- count.toplot.core %>% 
  filter(species_common == "Southern elephant seal") %>% 
  mutate(census_month = month(census_date_start)) %>% 
  filter(census_month %in% months.curr) %>% 
  mutate(census_month = factor(census_month, levels = months.curr, 
                               labels = month.name[months.curr])) %>% 
  select(-total_count) %>% 
  pivot_longer(ends_with("_count"), names_to = "age_sex_orig", 
               values_to = "count") %>% 
  mutate(age_sex_orig = age_sex_orig, 
         age_sex = case_when(
           str_detect(age_sex_orig , "juv_") ~ "juv_count", 
           .default = age_sex_orig
         )) %>% 
  filter(!is.na(count)) %>% 
  group_by(location, species, species_common, season_name, census_month, 
           age_sex) %>%
  summarise(count_mean = mean(count , na.rm = TRUE),
            count_sd = sd(count),
            .groups = "drop") %>%
  mutate(count_sd = replace_na(count_sd, 0)) %>% 
  filter(!(age_sex %in% c("unk_female_count", "unk_male_count", "ad_unk_count")))

g.month.ses <- y.long %>% 
  ggplot(aes(season_name, count_mean, 
             color = age_sex, group = age_sex)) +
  geom_point(aes(size = count_sd)) +
  geom_line(aes(linetype = species)) + 
  facet_wrap(vars(census_month), nrow = 1) + 
  xlab("Season") + ylab("Count") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  guides(linetype = guide_legend(title = "Species", order = 1, 
                                 label.theme = legend.italic),
         color = guide_legend(title = "Age class + sex", order = 2), 
         size = guide_legend(title = "Count sd", order = 3)) +
  scale_color_csphoc()



#------------------------------------------------
grid.count <- plot_grid(
  g.grp, g.month.clw, g.month.ses, ncol = 1, align = "v", axis = "lr", 
  labels = c("a", "b", "c"), label_size = csphoc.labels.size
)

if (save.image) {
  ggsave(here(here.fig.tbl, "Fig4_csphoc_counts.pdf"), 
         grid.count, width = 20, height = 12)
} else {
  print(grid.count)
}


###############################################################################
###############################################################################
## Create table with column descriptors (table 1)
tbl1.ref <- read_csv(here(here.csv, "cs-phoc-counts.csv"), 
                     col_types = "ccccciiiiiiiiiii") %>% 
  as.data.frame()

tbl1 <- data.frame(
  Column = names(tbl1.ref), 
  data_type = vapply(tbl1.ref, class, as.character(1))
) %>% 
  mutate(Description = case_when(
    Column == "count_id"~          "A unique identifier for each count record",
    Column == "event_id"~          "The unique identifier for event records, can be used to join count records with event records",
    Column == "location" ~         "The Cape Shirreff location",
    Column == "species" ~          "The scientific name of the phocid species", 
    Column == "species_common" ~   "The common name of the phocid species",
    Column == "total_count" ~      "The sum of all of the other '_count' columns, i.e., the total count for the corresponding census/location/species", 
    Column == "ad_female_count" ~  "Aggregate count of adult females for the corresponding census/location/species", 
    Column == "ad_male_count" ~    "Aggregate count of adult males",
    Column == "ad_unk_count" ~     "Aggregate count of adults of unknown sex",
    Column == "juv_female_count" ~ "Aggregate count of juvenile females",
    Column == "juv_male_count" ~   "Aggregate count of juvenile males",
    Column == "juv_unk_count" ~    "Aggregate count of juveniles of unknown sex",
    Column == "pup_count" ~        "Aggregate count of pups (young of the year, less than one year old) of any sex",
    Column == "unk_female_count" ~ "Aggregate count of unknown age class females",
    Column == "unk_male_count" ~   "Aggregate count of unknown age class males",
    Column == "unk_unk_count" ~    "Aggregate count of phocids of unknown age class and unknown sex"
  )) %>% 
  rename(`Data type` = data_type)

if (save.image) {
  write_tsv(tbl1, file = here(here.fig.tbl, "Table1_csphoc.tsv"), na = "")
} else {
  tbl1 %>% glimpse()
}


################################################################################
################################################################################
## These line and dot plots are intended for exploration only


#-------------------------------------------------------------------------------
# Core census locations

# ### Dot charts - Core locations
# for (i in tamatoamlr::pinniped.phocid.sp) {
#   print(i)
#   if (i == 'Elephant seal') i <- "Southern elephant seal"
#   i.toplot <- count.toplot.core %>% filter(species_common == i)
#   
#   g.curr <- ggplot(i.toplot, aes(x = plot_date_start, y = season_name)) +
#     geom_point(aes(size = total_count)) +
#     labs(title = paste("Cape Shirreff phocid census counts", 
#                        "Core census locations", i, sep = " - "),
#          x = "Date", y = "Season") +
#     theme(axis.text.x = element_text(angle = 90)) +
#     guides(size = guide_legend(title = "Count")) + 
#     scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
#     scale_y_discrete(drop=FALSE)
#   
#   i.filename <- str_replace(tolower(i), " ", "_")
#   if (save.image)
#     ggsave(here(here.fig.other, "scatterplot", paste0(i.filename, ".png")), 
#            g.curr, width = 10, height = 6)
# }; rm(i, i.toplot, g.curr)
# 
# 
# ### Line charts - Core locations
# for (i in tamatoamlr::pinniped.phocid.sp) {
#   print(i)
#   if (i == 'Elephant seal') i <- "Southern elephant seal"
#   i.toplot <- count.toplot.core %>% filter(species_common == i) 
#   #, season_name == "2003/04")
#   
#   g.curr <- ggplot(i.toplot, aes(x = plot_date_start, y = total_count)) +
#     geom_point(aes(color = season_name)) +
#     geom_line(aes(color = season_name)) + 
#     labs(title = paste("Cape Shirreff phocid census counts", 
#                        "Core census locations", i, sep = " - "),
#          x = "Date", y = "Count") +
#     theme(axis.text.x = element_text(angle = 90)) +
#     guides(color = guide_legend(title = "Season")) + 
#     scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
#   # print(g.curr)
#   
#   i.filename <- str_replace(tolower(i), " ", "_")
#   if (save.image)
#     ggsave(here.fig.other, "line_graph", paste0(i.filename, ".png")),
#            g.curr, width = 10, height = 6)
# }; rm(i, i.toplot, g.curr)


#-------------------------------------------------------------------------------
# Core census locations + PST; from 2009/10 on
# cspc_sum <- function(x, na.rm = TRUE) {
#   if_else(all(is.na(x)), NA_integer_, sum(x, na.rm = na.rm))
# }
# 
# count.toplot.combo <- z.count %>% 
#   group_by(event_id, species_common) %>% 
#   summarise(location = "Core + PST", 
#             across(ends_with("_count"), cspc_sum),  
#             .groups = "drop") %>% 
#   left_join(header.toplot, by = join_by(event_id)) %>% 
#   filter(census_date_start > ymd("2009-07-01"), 
#          surveyed_san_telmo) %>% 
#   mutate(season_name = as.character(season_name), 
#          season_name = factor(season_name, levels = season.name.levels.amlr))
# 
# 
# 
# ### Dot counts - Core + PST locations
# for (i in tamatoamlr::pinniped.phocid.sp) {
#   print(i)
#   if (i == 'Elephant seal') i <- "Southern elephant seal"
#   i.toplot <- count.toplot.combo %>% filter(species_common == i)
#   
#   g.curr <- ggplot(i.toplot, aes(x = plot_date_start, y = season_name)) +
#     geom_point(aes(size = total_count)) +
#     labs(title = paste("Cape Shirreff phocid census counts", 
#                        "Core+PST locations", i, sep = " - "),
#          x = "Date", y = "Season") +
#     theme(axis.text.x = element_text(angle = 90)) +
#     guides(size = guide_legend(title = "Count")) + 
#     scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
#     scale_y_discrete(drop=FALSE)
#   
#   i.filename <- paste0("core+pst_", str_replace(tolower(i), " ", "_"))
#   if (save.image)
#     ggsave(here(here.fig.other, "scatterplot", paste0(i.filename, ".png")), 
#            g.curr, width = 10, height = 6)
# }; rm(i, i.toplot, g.curr)
# 
# 
# ### Line charts - Core + PST locations
# for (i in tamatoamlr::pinniped.phocid.sp) {
#   print(i)
#   if (i == 'Elephant seal') i <- "Southern elephant seal"
#   i.toplot <- count.toplot.combo %>% filter(species_common == i)
#   
#   g.curr <- ggplot(i.toplot, aes(x = plot_date_start, y = total_count)) +
#     geom_point(aes(color = season_name)) +
#     geom_line(aes(color = season_name)) + 
#     labs(title = paste("Cape Shirreff phocid census counts", 
#                        "Core+PST locations", i, sep = " - "),
#          x = "Date", y = "Count") +
#     theme(axis.text.x = element_text(angle = 90)) +
#     guides(color = guide_legend(title = "Season")) + 
#     scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
#   g.curr
#   
#   i.filename <- paste0("core+pst_", str_replace(tolower(i), " ", "_"))
#   if (save.image)
#     ggsave(here(here.fig.other, "line_graph", paste0(i.filename, ".png")), 
#            g.curr, width = 10, height = 6)
# }; rm(i, i.toplot, g.curr)

################################################################################
################################################################################

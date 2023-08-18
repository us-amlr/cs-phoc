# Make plots!

library(tidyverse)
library(stringr)
library(here)
library(amlrPinnipeds)
library(glue)
library(cowplot)
library(viridis)
library(ggsci)

con <- amlr_dbConnect(Database = "***REMOVED***")
save.image <- TRUE

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


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Read in data, and do some prep
z.header <- read_csv(here("manuscript", "cs-phoc-headers.csv"), 
                     col_types = "ccDDilc")
nrow(z.header)

z <- read_csv(here("manuscript", "cs-phoc-counts.csv"), 
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
  labs(title = "CS-PHOC surveys, by season", 
       x = "Date", y = "Season") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  guides(color = guide_legend(title = "Research program", order = 1),
         # shape = guide_legend(title = "PST", order = 3), 
         size = guide_legend(title = "Days", order = 2)) + 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + 
  scale_y_discrete(drop = FALSE) + 
  scale_size(breaks = 1:3) +
  # scale_color_csphoc()
  # scale_color_brewer(palette = "Set2")
  scale_color_manual(values = viridis(3)[1:2])
# g1

g2 <- ggplot(header.toplot, aes(x = season_name)) + 
  geom_histogram(stat = "count") + 
  scale_x_discrete(drop = FALSE) + 
  # labs(y = "# of surveys") + 
  labs(y = "Number of surveys") + 
  coord_flip() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# g2

g.grid <- plot_grid(g1, g2, rel_widths = c(8, 1.5), align = "h", axis = "tb")
# g.grid

if (save.image)
  ggsave(here("manuscript", "figures+tables", "Fig2_census_surveys.png"), 
         g.grid, width = 10, height = 6)

rm(g.grid, g1, g2)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Census timing - start and end times
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
  guides(fill = guide_legend(title = "Time type")) + 
  # scale_fill_discrete(limits = c("start", "end")) + 
  ggtitle("Census record start and end times") + 
  xlab("Hour (24h)") + 
  ylab("Number of records") + 
  scale_x_continuous(limits = c(6, 22), breaks = 0:24, minor_breaks = NULL) +
  scale_fill_csphoc()

gg.mid <- ggplot(csphoc.times, aes(census_dt_midpoint_24h)) + 
  geom_bar() + 
  ggtitle("Midpoint time of census records") + 
  xlab("Hour (24h)") + 
  ylab("Number of records") + 
  scale_x_continuous(limits = c(6, 22), breaks = 0:24, minor_breaks = NULL)

gg.hours <- ggplot(csphoc.times, aes(census_hours_bar)) + 
  geom_bar() + 
  ggtitle("Length time of census records") + 
  xlab("Hours") + 
  ylab("Number of records") + 
  scale_x_continuous(breaks = 0:10)

g.timing <- plot_grid(gg.st.end, gg.mid, gg.hours, ncol = 1)
# g.timing

if (save.image)
  ggsave(here("manuscript", "figures+tables", "Fig3_census_record_times.png"), 
         g.timing, width = 5, height = 10)
rm(csphoc.times, g.timing, gg.st.end, gg.mid, gg.hours)

#------------------------------------------------
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
z.toplot <- left_join(z, header.toplot, by = join_by(header_id))
z.toplot.core <- z.toplot %>% filter(location == "Core census locations")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# These line and dot plots are intended for exploration only


#-------------------------------------------------------------------------------
# Core census locations

# ### Dot charts - Core locations
# for (i in amlrPinnipeds::pinniped.phocid.sp) {
#   print(i)
#   if (i == 'Elephant seal') i <- "Southern elephant seal"
#   i.toplot <- z.toplot.core %>% filter(species_common == i)
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
#     ggsave(here("output", "scatterplot", paste0(i.filename, ".png")), 
#            g.curr, width = 10, height = 6)
# }; rm(i, i.toplot, g.curr)
# 
# 
# ### Line charts - Core locations
# for (i in amlrPinnipeds::pinniped.phocid.sp) {
#   print(i)
#   if (i == 'Elephant seal') i <- "Southern elephant seal"
#   i.toplot <- z.toplot.core %>% filter(species_common == i) 
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
#     ggsave(here("output", "line_graph", paste0(i.filename, ".png")),
#            g.curr, width = 10, height = 6)
# }; rm(i, i.toplot, g.curr)


#-------------------------------------------------------------------------------
# Core census locations + PST; from 2009/10 on
# cspc_sum <- function(x, na.rm = TRUE) {
#   if_else(all(is.na(x)), NA_integer_, sum(x, na.rm = na.rm))
# }
# 
# z.toplot.combo <- z %>% 
#   group_by(header_id, species_common) %>% 
#   summarise(location = "Core + PST", 
#             across(ends_with("_count"), cspc_sum),  
#             .groups = "drop") %>% 
#   left_join(header.toplot, by = join_by(header_id)) %>% 
#   filter(census_date_start > ymd("2009-07-01"), 
#          surveyed_san_telmo) %>% 
#   mutate(season_name = as.character(season_name), 
#          season_name = fct(season_name, levels = season.name.levels.amlr))
# 
# 
# 
# ### Dot counts - Core + PST locations
# for (i in amlrPinnipeds::pinniped.phocid.sp) {
#   print(i)
#   if (i == 'Elephant seal') i <- "Southern elephant seal"
#   i.toplot <- z.toplot.combo %>% filter(species_common == i)
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
#     ggsave(here("output", "scatterplot", paste0(i.filename, ".png")), 
#            g.curr, width = 10, height = 6)
# }; rm(i, i.toplot, g.curr)
# 
# 
# ### Line charts - Core + PST locations
# for (i in amlrPinnipeds::pinniped.phocid.sp) {
#   print(i)
#   if (i == 'Elephant seal') i <- "Southern elephant seal"
#   i.toplot <- z.toplot.combo %>% filter(species_common == i)
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
#     ggsave(here("output", "line_graph", paste0(i.filename, ".png")), 
#            g.curr, width = 10, height = 6)
# }; rm(i, i.toplot, g.curr)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Fig 3: Group counts by month and season group, before plotting
season.groups <- function(i) {
  case_when(
    i %in% c("1997/98", "1998/99", "1999/00", "2000/01", "2001/02") ~ "1997-2002", 
    i %in% c("2002/03", "2003/04", "2004/05", "2005/06", "2006/07") ~ "2002-2007", 
    i %in% c("2009/10", "2010/11", "2011/12", "2012/13") ~ "2009-2013", 
    i %in% c("2013/14", "2014/15", "2015/16", "2016/17") ~ "2013-2017", 
    i %in% c("2017/18", "2018/19", "2019/20", "2021/22", "2022/23") ~ "2017-2023"
  )
  # case_when(
  #   i %in% c("1997/98", "1998/99", "1999/00") ~ "1997-2000", 
  #   i %in% c("2000/01", "2001/02", "2002/03") ~ "2000-2003", 
  #   i %in% c("2003/04", "2004/05", "2005/06", "2006/07") ~ "2003-2007", 
  #   i %in% c("2009/10", "2010/11", "2011/12") ~ "2009-2012", 
  #   i %in% c("2012/13", "2013/14", "2014/15") ~ "2012-2015", 
  #   i %in% c("2015/16", "2016/17", "2017/18") ~ "2015-2018", 
  #   i %in% c("2018/19", "2019/20") ~ "2018-2020", 
  #   i %in% c("2021/22", "2022/23") ~ "2021-2023"
  # )
}


x <- z.toplot.core %>% 
  mutate(census_month = month(census_date_start), 
         census_week = week(census_date_start), 
         season_group = season.groups(season_name))

x.grp <- x %>% 
  group_by(location, species, species_common, season_group, 
           census_month) %>% 
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
  # geom_errorbar(aes(ymin = total_count_mean-total_count_sd, 
  #                   ymax = total_count_mean+total_count_sd), 
  #               width = 5) + 
  facet_wrap(vars(species), nrow = 1, scales = "free_y") + 
  # ggtitle(paste("Cape Shirreff phocid census counts", 
  #               "Core locations", "Monthly averages", sep = " - ")) + 
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
y.month <- z.toplot.core %>% 
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
  # ggtitle(paste("Cape Shirreff phocid census counts", 
  #               "Core locations", "Monthly averages", sep = " - ")) + 
  xlab("Season") + ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  guides(color = guide_legend(title = "Species", 
                              label.theme = legend.italic)) +
  scale_color_csphoc()

# # Plot with log scale
# y.month %>% 
#   # filter(!(species_common %in% "Southern elephant seal")) %>%
#   ggplot(aes(season_name, total_count_mean, 
#              color = species, group = species)) +
#   geom_point() +
#   geom_line() + 
#   # geom_errorbar(aes(ymin = total_count_mean-total_count_sd,
#   #                   ymax = total_count_mean+total_count_sd),
#   #               width = 0.3) +
#   facet_wrap(vars(census_month), nrow = 1) + 
#   xlab("Season") + ylab("Count") +
#   scale_y_continuous(trans='log10') + 
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   guides(color = guide_legend(title = "Species", 
#                               label.theme = legend.italic)) 


# # Plot elephant seals...
# y.month %>% 
#   filter((species_common %in% "Southern elephant seal")) %>%
#   ggplot(aes(season_name, total_count_mean, group = species)) +
#   geom_point() +
#   geom_line() + 
#   geom_errorbar(aes(ymin = total_count_mean-total_count_sd,
#                     ymax = total_count_mean+total_count_sd),
#                 width = 0.3) +
#   facet_wrap(vars(census_month), nrow = 1) + 
#   ggtitle(paste("Cape Shirreff phocid census counts", 
#                 "Core locations", "Monthly averages", sep = " - ")) + 
#   xlab("Season") + ylab("Count") +
#   theme(axis.text.x = element_text(angle = 90)) +
#   guides(color = guide_legend(title = "Species")) 

#------------------------------------------------
# Plot elephant seals, with age/sex class
y.long <- z.toplot.core %>% 
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

# y.long2 <- z.toplot.core %>% 
#   filter(species_common == "Southern elephant seal") %>% 
#   mutate(census_month = month(census_date_start)) %>% 
#   filter(census_month %in% months.curr) %>% 
#   mutate(census_month = factor(census_month, levels = months.curr, 
#                                labels = month.name[months.curr])) %>% 
#   select(-total_count) %>% 
#   pivot_longer(ends_with("_count"), names_to = "age_sex", values_to = "count") %>% 
#   filter(!is.na(count), 
#          !(age_sex %in% c("unk_female_count", "unk_male_count", "ad_unk_count"))) %>% 
#   mutate(age_sex = if_else(str_detect(age_sex, "juv"), "juv_count", age_sex)) %>% 
#   group_by(location, species, species_common, season_name, census_month, age_sex) %>%
#   summarise(count_mean = mean(count , na.rm = TRUE),
#             count_sd = sd(count), .groups = "drop") %>% 
#   mutate(count_sd = replace_na(count_sd, 0)) 

g.month.ses <- y.long %>% 
  ggplot(aes(season_name, count_mean, 
             color = age_sex, group = age_sex)) +
  geom_point(aes(size = count_sd)) +
  geom_line(aes(linetype = species)) + 
  # geom_errorbar(aes(ymin = count_mean-count_sd,
  #                   ymax = count_mean+count_sd),
  #               width = 0.3) +
  # facet_wrap(vars(age_sex, census_month), nrow = 5) + 
  facet_wrap(vars(census_month), nrow = 1) + 
  # ggtitle("CSPHOC") + 
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
  labels = c('A', 'B', 'C'), label_size = 15
)

if (save.image)
  ggsave(here("manuscript", "figures+tables", "Fig4_census_counts.png"), 
         grid.count, width = 20, height = 12)


# Code to create CS map (Figure 1)

library(ggplot2)
library(ggspatial)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(cowplot)
library(here)
library(viridis)


#-------------------------------------------------------------------------------
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

g.map <- ggplot(data = world) +
  geom_sf() +
  theme_bw()

# g.map
crs.laea <- "+proj=laea +lat_0=-75 +lon_0=-15 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "

# Define limits
xlim.ssi <- c(-65.0, -53.5)
ylim.ssi <- c(-65.35, -60.5)
xlim.cs <- c(-60.9, -60.6)
ylim.cs <- c(-62.56, -62.45)

ssi.sfc.4326 <- st_as_sfc(st_bbox(c(xmin = xlim.ssi[1], xmax = xlim.ssi[2], 
                                    ymax = ylim.ssi[2], ymin = ylim.ssi[1]), 
                                  crs = st_crs(4326)))
ssi.sfc.laea <- st_transform(ssi.sfc.4326, crs.laea)


#-------------------------------------------------------------------------------
# Maps

### of whole region
g.region <- g.map +
  coord_sf(crs = crs.laea) + 
  geom_rect(xmin = min(st_coordinates(ssi.sfc.laea)[, "X"]), 
            xmax = max(st_coordinates(ssi.sfc.laea)[, "X"]), 
            ymin = min(st_coordinates(ssi.sfc.laea)[, "Y"]), 
            ymax = max(st_coordinates(ssi.sfc.laea)[, "Y"]), 
            fill = NA, colour = "black", linewidth = 1) +
  theme(panel.border = element_blank())


### of SSI
g.ssi <- g.map + 
  coord_sf(xlim = xlim.ssi, ylim = ylim.ssi, 
           crs = st_crs(4326), expand = FALSE) + 
  geom_rect(xmin = min(xlim.cs), xmax = max(xlim.cs), 
            ymin = min(ylim.cs), ymax = max(ylim.cs), 
            fill = NA, colour = "black", linewidth = 0.5) + 
  annotation_scale(location = "tl", width_hint = 0.5) +
  # annotation_north_arrow(location = "tr", which_north = "true", 
  #                        pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
  #                        style = north_arrow_fancy_orienteering) + 
  annotate(geom = "text", x = -60, y = -64.2, label = "Antarctic Peninsula", 
           fontface = "italic", color = "black", size = 4, angle = 38) + 
  annotate(geom = "text", x = -58, y = -61.5, label = "South Shetland Islands", 
           fontface = "italic", color = "black", size = 4, angle = 0) + 
  annotate(geom = "text", x = mean(xlim.cs), y = -62.3, label = "Cape Shirreff", 
           fontface = "italic", color = "black", size = 3, angle = 0) +
  xlab(NULL) +
  ylab(NULL)



### of Cape Shirreff

# cs.image <- grid::rasterGrob(
#   png::readPNG("C:/SMW/Pinnipeds/CS-PHOC/manuscript-scidata/CS map - shaded.png"),
#   width=ggplot2::unit(1,"npc"),
#   height=ggplot2::unit(1,"npc")
# )
# g.cs <- ggplot() + 
#   annotation_custom(cs.image, -Inf, Inf, -Inf, Inf) +
#   geom_text()

ad.cs <- st_read("../CS Polygon and fur seal beaches_Adahood/Inach_Shirreff_poly.shp")
ad.cs.line <- st_cast(st_geometry(ad.cs), "LINESTRING")
pst.sfc <- st_sfc(
  st_linestring(matrix(c(c(613900, 613700, 613000, 612980), 
                         c(3070200, 3070000, 3070000, 3069900)), 
                       ncol = 2)), 
  crs = st_crs(ad.cs.line)
)
pst.sf <- st_sf(pst.sfc, data.frame(type = "Punta San Telmo")) %>% 
  st_set_geometry("geometry")

minmax_to_sfc <- function(x1, x2, y1, y2, crs) {
  x.df <- data.frame(lon = c(x1, x2, x2, x1, x1), 
                     lat = c(y1, y1, y2, y2, y1))
  st_sfc(
    st_polygon(list(matrix(c(x.df$lon, x.df$lat), ncol = 2))), 
    crs = crs
  )
}

y1.min <- 3069990
cs.cor.loc.sfc <- c(
  st_sfc(
    st_polygon(list(matrix(c(c(615000, 614775, 614200, 615000, 615000), 
                             c(y1.min, y1.min, 3070300, 3070300, y1.min)), 
                           ncol = 2))), 
    crs = st_crs(ad.cs.line)
  ), 
  minmax_to_sfc(614400, 615300, 3070300, 3071680, st_crs(ad.cs.line)), 
  minmax_to_sfc(614400, 615300, 3071810, 3072000, st_crs(ad.cs.line)), 
  minmax_to_sfc(614000, 614400, 3071810, 3071980, st_crs(ad.cs.line)),
  
  
  minmax_to_sfc(614000, 614210, 3072020, 3072400, st_crs(ad.cs.line)), 
  minmax_to_sfc(613400, 614210, 3072400, 3072750, st_crs(ad.cs.line)), 
  minmax_to_sfc(612800, 613400, 3072020, 3072260, st_crs(ad.cs.line)),
  minmax_to_sfc(612800, 614100, 3070220, 3071900, st_crs(ad.cs.line))
)
# plot(st_geometry(ad.cs.line), axes = TRUE); plot(cs.cor.loc.sfc, add = TRUE)

ad.cs.col <- st_intersection(cs.cor.loc.sfc, ad.cs.line)
cs.core <- st_sf(ad.cs.col, data.frame(type = "Core census locations")) %>% 
  st_set_geometry("geometry")

# loc.levels <- c("Core census locations", "Punta San Telmo", 
#                 "CS mainland")
sf.locations <- bind_rows(cs.core, pst.sf)
# sf.locations <- ad.cs %>%
#   mutate(type = "CS mainland") %>%
#   bind_rows(cs.core, pst.sf) %>%
#   mutate(type = factor(type, levels = loc.levels), 
#          lwd = if_else(type == "CS mainland", 0.5, 2))

g.cs <- ggplot() +
  geom_sf(data = ad.cs) +
  geom_sf(data = sf.locations, aes(col = type, fill = type), lwd = 3) + 
  # scale_colour_grey() + scale_fill_grey() +
  # scale_fill_manual(values = gray.colors(3)[c(1, 2)]) +
  # scale_colour_manual(values = gray.colors(3)[c(1, 2)]) +
  scale_color_manual(values = viridis(3)[1:2]) +
  scale_fill_manual(values = viridis(3)[1:2]) +
  # scale_fill_brewer(palette = "Set2") +
  # scale_color_brewer(palette = "Set2") +
  # scale_fill_viridis(option = "H", discrete = TRUE) + 
  # scale_color_viridis(option = "H", discrete = TRUE) + 
  coord_sf(crs = st_crs(4326)) +
  guides(fill = guide_legend(title = NULL), 
         color = guide_legend(title = NULL)) +
  theme_bw() + 
  theme(legend.position = "bottom")
# g.cs


### Grid
grid.map <- plot_grid(
  plot_grid(g.region, g.ssi, nrow = 2, align = "v", axis = "lr"), 
  g.cs, ncol = 2, rel_widths = c(1, 1.3)
)
# grid.map

ggsave(here("manuscript", "figures+tables", "Fig1_cs_map.png"), 
       grid.map, width = 9, height = 6, bg = "white")




#-------------------------------------------------------------------------------
# ### Testing
# # https://doi.org/10.5285/bc71347d-298a-4df3-88b0-cb9a908db166
# z <- st_read("../add_coastline_high_res_line_v7_5.shp/add_coastline_high_res_line_v7_5.shp")
# st_crs(z)
# st_bbox(z)
# # plot(st_geometry(z))
# 
# xmin <- -2662876
# xmax <- -1500000
# ymin <- 1000000
# ymax <- 2000000
# 
# cs.bbox <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), 
#                    crs = st_crs(3031))
# 
# cs.sfc.start <- st_intersection(st_geometry(z), st_as_sfc(cs.bbox))
# plot(cs.sfc.start, axes = TRUE)
# 
# 
# cs.pt <- st_sfc(st_point(c(-2661599.948916, 1488341.291007)), 
#                 crs = 3031)
# cs.circle <- st_buffer(cs.pt, dist = 3700)
# 
# 
# cs.sfc <- st_intersection(st_geometry(z), cs.circle)
# plot(cs.sfc, axes = TRUE)
# plot(cs.circle, add = TRUE)
# plot(cs.pt, add = TRUE)
# 
# cs.4326 <- st_transform(cs.sfc2, 4326)
# 
# 
# plot(cs.4326, axes = TRUE)

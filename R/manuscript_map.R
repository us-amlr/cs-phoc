library(ggplot2)
library(ggspatial)
# theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(cowplot)
library(here)


#-------------------------------------------------------------------------------
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

g.map <- ggplot(data = world) +
  geom_sf() +
  theme_bw()

# g.map

# Define limits
xlim.ssi <- c(-65, -53.5)
ylim.ssi <- c(-65, -60.5)
xlim.cs <- c(-60.9, -60.6)
ylim.cs <- c(-62.56, -62.45)


#-------------------------------------------------------------------------------
# Maps

### of whole region
g.region <- g.map + 
  coord_sf(xlim = c(-90, -30), ylim = c(-72, -45), 
           crs = st_crs(4326), expand = FALSE) + 
  # annotation_north_arrow(location = "tr", which_north = "true", 
  #                        pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  geom_rect(xmin = min(xlim.ssi), xmax = max(xlim.ssi), 
            ymin = min(ylim.ssi), ymax = max(ylim.ssi), 
            fill = NA, colour = "black", linewidth = 1.5)



# g.map + 
#   coord_sf(crs = st_crs(3031))

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
g.cs <- g.map + 
  coord_sf(xlim = xlim.cs, ylim = ylim.cs, 
           crs = st_crs(4326), expand = FALSE) +
  ggtitle("Hi-res map with core census locations todo")

### Grid
g.grid2 <- plot_grid(plot_grid(g.region, g.ssi, nrow = 2), g.cs, 
                     ncol=2)
ggsave(here("output", "cs_map.png"), g.grid2, width = 10, height = 6)




#-------------------------------------------------------------------------------
# https://doi.org/10.5285/bc71347d-298a-4df3-88b0-cb9a908db166
z <- st_read("../add_coastline_high_res_line_v7_5.shp/add_coastline_high_res_line_v7_5.shp")
st_crs(z)
st_bbox(z)
# plot(st_geometry(z))

xmin <- -2662876
xmax <- -1500000
ymin <- 1000000
ymax <- 2000000

cs.bbox <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), 
                   crs = st_crs(3031))

cs.sfc.start <- st_intersection(st_geometry(z), st_as_sfc(cs.bbox))
plot(cs.sfc.start, axes = TRUE)


cs.pt <- st_sfc(st_point(c(-2661599.948916, 1488341.291007)), 
                crs = 3031)
cs.circle <- st_buffer(cs.pt, dist = 3700)


cs.sfc <- st_intersection(st_geometry(z), cs.circle)
plot(cs.sfc, axes = TRUE)
plot(cs.circle, add = TRUE)
plot(cs.pt, add = TRUE)

cs.4326 <- st_transform(cs.sfc2, 4326)


plot(cs.4326, axes = TRUE)


library(tmap)
tmap_mode("view")
tm_shape(cs.4326) + tm_lines()


ad.afs <- st_read("../CS Polygon and fur seal beaches_Adahood/Fur_Seal_Beaches.shp")
ad.cs <- st_read("../CS Polygon and fur seal beaches_Adahood/Inach_Shirreff_poly.shp")

plot(st_geometry(ad.cs), axes = TRUE)
plot(st_geometry(ad.afs), add = TRUE, col = "red")

plot(st_transform(st_geometry(ad.cs), 4326), axes = TRUE)

ggplot(data = ad.cs) +
  geom_sf() +
  theme_bw() + 
  coord_sf(crs = st_crs(4326))
  annotation_scale(location = "tl", width_hint = 0.5)
  

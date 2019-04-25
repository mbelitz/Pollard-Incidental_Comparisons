library(tidyverse)

basemap = map_data('state')

grid_1deg <- readOGR("data/gis", "grid_1deg")
grid_1deg$polyids <- 1:nrow(grid_1deg)

grid_1deg_points <- fortify(grid_1deg)


lat_range = c(20,60)
lon_range = c(-150,50)

ggplot() + 
  geom_polygon(data = basemap, aes(x=long, y = lat, group = group), fill=NA, color='black', size=1.5) +
  scale_x_continuous(breaks=seq(lon_range[1],lon_range[2],10), minor_breaks = seq(lon_range[1],lon_range[2],1)) +
  scale_y_continuous(breaks=seq(lat_range[1],lat_range[2],10), minor_breaks = seq(lat_range[1],lat_range[2],1)) + 
  theme_bw() +
  coord_fixed(1.3) +
  labs(x='Longitude',y='Latitude') + 
  geom_polygon(data = grid_1deg_points, aes(x = long, y = lat, group = group),
  color = "black", fill = NA, size = 0.2)

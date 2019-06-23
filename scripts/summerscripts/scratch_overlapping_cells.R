library(sf)
library(raster)
library(dplyr)

source("scripts/summerscripts/read_clean_occurrence_data.R")

# create total butterfly dataframe for project
total_buts <- total_buts %>% 
  dplyr::filter(year < 2019) %>% 
  dplyr::filter(longitude >= -94 & longitude <= -76) %>% 
  dplyr::filter(latitude >= 36 & latitude <= 42) %>% 
  dplyr::mutate(lat_bin = 1*floor(latitude/1) + 1/1,
                lon_bin = 1*floor(longitude/1) + 1/1)

total_buts <- total_buts %>% 
  mutate(lat_bin = lat_bin - 0.5,
         lon_bin = lon_bin - 0.5)

spp_count <- total_buts %>% 
  group_by(year, lon_bin, lat_bin, scientific_name) %>% 
  summarise(count = n())

incidental <- spp_count %>% 
  filter(count > 5) %>% 
  filter(year >= 2014)

# read in Elise data

pollard <- read.csv(file = "data/PollardDataDensity_Grid.csv", stringsAsFactors = FALSE)
pollard <- pollard %>% 
  rename(lat_bin = latcell, lon_bin = longcell, scientific_name = Scientific.Name)

# join together to see where the greatest dd is

all_data <- full_join(incidental, pollard, by = c("lat_bin", "lon_bin", "scientific_name", "year"))

all_data <- all_data %>% 
  mutate(combined_obs = count + n) %>% 
  dplyr::select(scientific_name, year, lat_bin, lon_bin, count, n, combined_obs)

# look into overlap

overlap <-all_data %>% 
  filter(!is.na(combined_obs))

overlap_10 <- overlap %>% 
  filter(count >= 10)

write.csv(overlap_10, file = "outputs/comparable_cells.csv", row.names = FALSE)

overlap_dens <- overlap %>% 
  filter(count >= 10) %>% 
  group_by(year, scientific_name) %>% 
  summarise(cellcount = n())

overlap_dens_allyears <- overlap %>% 
  filter(count >= 10) %>% 
  group_by(scientific_name) %>%
  summarise(cellcount = n())

# visualize data

e <- as(raster::extent(-94, -76, 36, 42), "SpatialPolygons") %>% 
 sf::st_as_sf()

grid <- st_make_grid(e, cellsize = c(1,1)) %>% 
  st_set_crs(4326)

plot(grid)  

celldens <- overlap %>% 
  group_by(lat_bin, lon_bin) %>% 
  summarise(total_overlap = n())

library(ggplot2)
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

extent <- filter(states, long >= -94 & long <= -76, lat >= 36 & lat <= 42)

ggplot() + 
  geom_polygon(data = extent, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") +
  geom_sf(data = grid, alpha = 0.4) +
  geom_point(data = celldens, aes(x = lon_bin, y = lat_bin, 
                                  color = total_overlap, size = total_overlap)) +
  scale_size(guide = FALSE) + 
  scale_color_viridis_c()

# write csvs

write.csv(x = overlap_dens, file = "outputs/overlapping_cells.csv", row.names = FALSE)
write.csv(x = overlap_dens_allyears, file = "outputs/spp_most_overlap.csv")

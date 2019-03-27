# data visualization, ran estimat onset function and now visualising data

spatial_pt_df <- SpatialPointsDataFrame(total_buts[,c("longitude", "latitude")],
                                        total_buts, proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

grid_df <- fortify(grid_1deg)


total_buts_2010 <- total_buts %>% 
  filter(year >= 2010)

ggplot() +
  geom_polygon(data = grid_df,
               aes(x = long, y = lat, group = group),
               color = "black", fill = "grey70", size = 0.2) + 
  geom_point(data = total_buts_2010, aes(x = longitude, y = latitude, color = day)) +
  scale_color_gradientn (colours = rainbow(10)) +
  facet_wrap(~year) +
  theme_classic()

## let's try histogram now

#total buts
totalbuts_spdf <- SpatialPointsDataFrame(total_buts[,c("longitude", "latitude")],
                                                     total_buts, proj4string = 
                                                       CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

total_buts_polypts <- spatialEco::point.in.poly(totalbuts_spdf, grid_1deg)
total_buts_vis_df <- as.data.frame(total_buts_polypts@data)
total_buts_vis_df <- total_buts_vis_df %>%
  mutate(long_centroid = (left + right) / 2, lat_centroid = (top + bottom) / 2) 

totalbuts_2018 <- total_buts_vis_df %>% 
  filter(year == 2018) %>% 
  filter(lat_centroid == 41.5)

ggplot(totalbuts_2018, aes(day)) +
  geom_histogram() + 
  facet_wrap(~long_centroid + lat_centroid) +
  labs(title = "All observed butterflies 2018")


totalbuts_2017 <- total_buts_vis_df %>% 
  filter(year == 2017) %>% 
  filter(lat_centroid == 41.5)

ggplot(totalbuts_2017, aes(day)) +
  geom_histogram() + 
  facet_wrap(~long_centroid + lat_centroid) +
  labs(title = "All observed butterflies 2017")

#p tharos

p_tharos_vis <- total_buts %>% 
  filter(scientific_name == "Phyciodes tharos", year == 2018) 

p_tharos_spdf <- SpatialPointsDataFrame(p_tharos_vis[,c("longitude", "latitude")],
                                        p_tharos_vis, proj4string = 
                                          CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))


polypts <- spatialEco::point.in.poly(p_tharos_spdf, grid_1deg)
p_tharos_vis_df <- as.data.frame(polypts@data)
p_tharos_vis_df <- p_tharos_vis_df %>%
  mutate(long_centroid = (left + right) / 2, lat_centroid = (top + bottom) / 2) %>% 
  filter(lat_centroid == 38.5 | lat_centroid == 41.5) %>% 
  filter(long_centroid == -90.5 | long_centroid == -93.5 | long_centroid == -87.5|
           long_centroid == -88.5)
  

ggplot(p_tharos_vis_df, aes(day)) +
  geom_histogram() + 
  facet_wrap(~long_centroid + lat_centroid) +
  labs(title = "Phyciodes tharos 2018")
  

# repeat for other species

create_vis_df <- function(species, yearofinterest){
  sp_vis <- total_buts %>% 
    filter(scientific_name == species, yearofinterest == year)
  sp_vis_spdf <- SpatialPointsDataFrame(sp_vis[,c("longitude", "latitude")],
                                        sp_vis, proj4string = 
                                          CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
  polypts <- spatialEco::point.in.poly(sp_vis_spdf, grid_1deg)
  sp_vis_df <- as.data.frame(polypts@data)
  sp_vis_df <- sp_vis_df %>%
    mutate(long_centroid = (left + right) / 2, lat_centroid = (top + bottom) / 2)
  return(sp_vis_df)
}

e_clarus_vis_df <- create_vis_df("Epargyreus clarus", 2018)


e_clarus_vis_df <- e_clarus_vis_df %>%
  filter(lat_centroid == 41.5) %>% 
  filter(long_centroid == -87.5| long_centroid == -88.5)


ggplot(e_clarus_vis_df, aes(day)) +
  geom_histogram() + 
  facet_wrap(~long_centroid + lat_centroid) +
  labs(title = "Epargyreus clarus 2018")

# E comyntas

e_comyntas_vis_df <- create_vis_df("Cupido comyntas", 2017)

e_comyntas_vis_df <- e_comyntas_vis_df %>%
  filter(lat_centroid == 41.5 | lat_centroid == 38.5) %>% 
  filter(long_centroid == -87.5| long_centroid == -88.5 | long_centroid == -93.5 | long_centroid == -90.5)


ggplot(e_comyntas_vis_df, aes(day)) +
  geom_histogram() + 
  facet_wrap(~long_centroid + lat_centroid) +
  labs(title = "Everes comyntas 2017")

e_comyntas_vis_df <- create_vis_df("Cupido comyntas", 2018)

e_comyntas_vis_df <- e_comyntas_vis_df %>%
  filter(lat_centroid == 41.5 | lat_centroid == 38.5) %>% 
  filter(long_centroid == -87.5| long_centroid == -88.5 | long_centroid == -93.5 | long_centroid == -90.5)


ggplot(e_comyntas_vis_df, aes(day)) +
  geom_histogram() + 
  facet_wrap(~long_centroid + lat_centroid) +
  labs(title = "Everes comyntas 2018")

# Pieris rapae

p_rapae_vis_df <- create_vis_df("Pieris rapae", 2018)

p_rapae_vis_df <- p_rapae_vis_df %>%
  filter(lat_centroid == 41.5 ) %>% 
  filter(long_centroid == -87.5| long_centroid == -88.5 | long_centroid == -93.5)


ggplot(p_rapae_vis_df, aes(day)) +
  geom_histogram(bins = 10) + 
  facet_wrap(~long_centroid + lat_centroid) +
  labs(title = "Pieris rapae 2018")

# Polygonia interrogationis

p_interrogationis_vis_df <- create_vis_df("Polygonia interrogationis", 2017)

p_interrogationis_vis_df <- p_interrogationis_vis_df %>%
  filter(lat_centroid == 41.5 ) %>% 
  filter(long_centroid == -87.5)


ggplot(p_interrogationis_vis_df, aes(day)) +
  geom_histogram(bins = 30) + 
  facet_wrap(~long_centroid + lat_centroid) +
  labs(title = "Polygonia interrogationis 2017")

p_interrogationis_vis_df <- create_vis_df("Polygonia interrogationis", 2018)

p_interrogationis_vis_df <- p_interrogationis_vis_df %>%
  filter(lat_centroid == 41.5 ) %>% 
  filter(long_centroid == -87.5)


ggplot(p_interrogationis_vis_df, aes(day)) +
  geom_histogram(bins = 30) + 
  facet_wrap(~long_centroid + lat_centroid) +
  labs(title = "Polygonia interrogationis 2018")

# Colias eurytheme

c_eurytheme_vis_df <- create_vis_df("Colias eurytheme", 2018)

c_eurytheme_vis_df <- c_eurytheme_vis_df %>%
  filter(lat_centroid == 41.5 ) %>% 
  filter(long_centroid == -93.5)


ggplot(c_eurytheme_vis_df, aes(day)) +
  geom_histogram(bins = 30) + 
  facet_wrap(~long_centroid + lat_centroid) +
  labs(title = "Colias eurytheme 2018")

# Onset map

colnames(total_buts_vis_df)[colnames(total_buts_vis_df) == "scientific_name"] <- "SciName"

total_onset_vis <- left_join(total_buts_vis_df, total_est_centroid_8spp, by = c("SciName", "year", "polyids"))

total_onset_vis_clean <- total_onset_vis %>% 
  filter(year == 2017) %>% 
  filter(onset > 0) 

onset <- ggplot() +
  geom_polygon(data = grid_df,
               aes(x = long, y = lat, group = group),
               color = "black", fill = "grey70", size = 0.2) + 
  geom_point(data = total_onset_vis_clean, aes(x = longitude, y = latitude, color = onset)) +
  scale_color_gradientn (colours = rainbow(10)) +
  facet_wrap(~SciName) +
  labs(title = "2017 Onset Estimates") +
  theme_classic()

onset

ggplot() +
  geom_polygon(data = grid_df,
               aes(x = long, y = lat, group = group),
               color = "black", fill = "grey70", size = 0.2) + 
  geom_point(data = total_onset_vis_clean, aes(x = longitude, y = latitude, color = onset, shape = SciName)) +
  scale_color_gradientn (colours = rainbow(10)) +
  labs(title = "2017 Onset Estimates") +
  theme_classic()


# writing script to see how many cells have >10 obs in a year

e <- as(raster::extent(-100, -65, 33, 55), "SpatialPolygons") %>% 
  st_as_sf()

grid <- st_make_grid(e, cellsize = c(1,1)) %>% 
  st_set_crs(crs(gdd_2014))

total_buts_spdf <- SpatialPointsDataFrame(total_buts[,c("longitude", "latitude")], total_buts,
                                          proj4string = crs(gdd_2014))

tb_polypts <- spatialEco::point.in.poly(total_buts_spdf, grid_sp)

tb_polypts_df <- as.data.frame(tb_polypts@data)

most_oc <- tb_polypts_df %>% 
  group_by(year, scientific_name, poly.ids) %>% 
  summarise(count = n())

most_oc <- most_oc %>% 
  filter(count >= 10, year >= 2014)

most_oc <- most_oc %>% 
  group_by(scientific_name) %>% 
  summarise(total_cells = n())

library(sf)
library(raster)
library(dplyr)
library(readr)
library(rgeos)
library(phest)
# import gdd data

gdd_2014 <- raster("data/gdd/Mean_GDD_2014.tif")
gdd_2015 <- raster("data/gdd/Mean_GDD_2015.tif")
gdd_2016 <- raster("data/gdd/Mean_GDD_2016.tif")
gdd_2017 <- raster("data/gdd/Mean_GDD_2017.tif")
gdd_2018 <- raster("data/gdd/Mean_GDD_2018.tif")

# create spatial grid

e <- as(raster::extent(-100, -65, 33, 55), "SpatialPolygons") %>% 
  st_as_sf()

grid <- st_make_grid(e, cellsize = c(1,1)) %>% 
  st_set_crs(crs(gdd_2014))

grid_sp <- as_Spatial(grid)

# Crop gdd rasters
gdd_2014 <- crop(gdd_2014, e)
gdd_2015 <- crop(gdd_2015, e)
gdd_2016 <- crop(gdd_2016, e)
gdd_2017 <- crop(gdd_2017, e)
gdd_2018 <- crop(gdd_2018, e)

# import butterfly data

# import data sources
cats <- read.csv("data/caterpillars_eastern_na.csv", stringsAsFactors = F)
ebutterfly <- read.csv("data/allrecords_2018_09_20_08_16_15_semi.csv", stringsAsFactors = F)

hesperiidae <- read.csv("data/iNat_families/hesperiidae_na.csv", stringsAsFactors = F)
pieridae <- read.csv("data/iNat_families/pieridae_na.csv", stringsAsFactors = F)  
lycaenidae <- read.csv("data/iNat_families/lycaenidae_na.csv", stringsAsFactors = F)
papilionidae <- read.csv("data/iNat_families/papilionidae_na.csv", stringsAsFactors = F)
nymphalidae <- read.csv("data/iNat_families/nymphalidae_na.csv", stringsAsFactors = F)
riodinidae <- read.csv("data/iNat_families/riodinidae_na.csv", stringsAsFactors = F)

inaturalist <- rbind(hesperiidae, pieridae, lycaenidae, papilionidae, nymphalidae, riodinidae)

# tidy data

# tidy ebutterfly data 
ebutlong <- parse_number(ebutterfly$Longitude)
ebutlat <- parse_number(ebutterfly$Latitude)

ebutterfly$Longitude <- ebutlong
ebutterfly$Latitude <- ebutlat

ebutterfly_tidy <- ebutterfly %>% 
  dplyr::filter(!is.na(Longitude)) %>% 
  dplyr::filter(!is.na(Latitude))%>%  
  dplyr::filter(Latitude > 18 & Latitude < 77) %>% 
  dplyr::filter(Longitude < -50 & Longitude > -170) 

ebutterfly_tidy <- ebutterfly_tidy %>%  
  dplyr::mutate(Date.Observed = as.Date(ebutterfly_tidy$Date.Observed, "%m/%d/%Y")) %>% 
  dplyr::mutate(day = lubridate::yday(Date.Observed)) %>% 
  dplyr::mutate(year = lubridate::year(Date.Observed)) %>% 
  dplyr::mutate(data_source = "ebutterfly") %>% 
  dplyr::mutate(scientific_name = paste(Genus, Species)) %>% 
  dplyr::select(scientific_name, Latitude, Longitude, OccuranceID, Observer, 
                Date.Observed, day, year, data_source) %>% 
  dplyr::rename(latitude = Latitude, longitude = Longitude, id = OccuranceID, 
                user_login = Observer, observed_on = Date.Observed)
inat_tidy <- inaturalist %>% 
  dplyr::filter(coordinates_obscured == "false") %>% 
  dplyr::filter(captive_cultivated == "false") %>% 
  dplyr::filter(quality_grade == "research") 

inat_adults <- dplyr::anti_join(inat_tidy, cats, by = "id") %>% 
  dplyr::mutate(data_source = "iNaturalist") %>% 
  dplyr::mutate(day = lubridate::yday(observed_on)) %>% 
  dplyr::mutate(year = lubridate::year(observed_on)) %>% 
  dplyr::select(scientific_name, latitude, longitude, id, user_login, observed_on, day, year, data_source)

total_buts <- rbind(inat_adults, ebutterfly_tidy) %>% 
  dplyr::filter(longitude >= -100 & longitude <= -65) %>% 
  dplyr::filter(latitude >= 33 & latitude <= 55) %>% 
  dplyr::filter(id != 6653072) 

total_buts_spdf <- SpatialPointsDataFrame(total_buts[,c("longitude", "latitude")], total_buts,
                                          proj4string = crs(gdd_2014))


# estimate onset function
estimate_weib_onset <- function(binomial, spatial_polygon){
  
  sp_df <- total_buts %>% 
    filter(scientific_name == binomial)
  
  spatial_pt_df <- SpatialPointsDataFrame(sp_df[,c("longitude", "latitude")],
                                          sp_df, proj4string = crs(gdd_2014))
  
  sp_df_gdd <- sp_df %>% 
    mutate(gdd = ifelse(year == 2014, extract(gdd_2014, spatial_pt_df),
                        ifelse(year == 2015, extract(gdd_2015, spatial_pt_df),
                               ifelse(year == 2016, extract(gdd_2016, spatial_pt_df),
                                      ifelse(year == 2017, extract(gdd_2017, spatial_pt_df),
                                             ifelse(year == 2018, extract(gdd_2018, spatial_pt_df), NA))))))
  
  spatial_gdd_df <- SpatialPointsDataFrame(sp_df_gdd[,c("longitude", "latitude")],
                                           sp_df_gdd, proj4string = crs(gdd_2014))
  
  polypts <- spatialEco::point.in.poly(spatial_gdd_df, spatial_polygon)
  
  species_df <- as.data.frame(polypts@data)
  
  species_df_manip <- species_df%>% 
    dplyr::select(day, poly.ids, scientific_name, year, gdd) %>% 
    group_by(poly.ids, day, year) %>% 
    summarise(obs = n())
  
  sp_df2 <- species_df_manip %>% 
    filter(!is.na(poly.ids)) %>% 
    group_by(poly.ids, year) %>% 
    summarise(total_obs = n())
  
  sp_df2 <- sp_df2 %>% 
    filter(total_obs >= 10)
  
  sp_df3 <- left_join(species_df_manip, sp_df2) %>% 
    filter(!is.na(total_obs)) %>% 
    filter(!is.na(poly.ids))
  
  sp_df4 <- left_join(species_df, sp_df3) %>% 
    filter(total_obs >= 10)
  
  estimate_df <- sp_df4 %>% 
    group_by(poly.ids, year) %>% 
    mutate(onset = weib.limit(day, upper = FALSE)[["estimate"]], 
           high_ci = weib.limit(day,upper = FALSE)[["upper-ci"]], 
           low_ci = weib.limit(day,upper = FALSE)[["lower-ci"]])
  
  estimate_df2 <- estimate_df %>% 
    group_by(poly.ids, year) %>% 
    summarise(gdd = mean(gdd),
              onset = mean(onset),
              high_ci = mean(high_ci),
              low_ci = mean(low_ci))
  
  estimate_df3 <- estimate_df2 %>% 
    filter(!is.na(gdd)) %>% 
    filter(gdd > 0) %>% 
    mutate(scientific_name = binomial)
  
  centroids <- gCentroid(grid_sp, byid = TRUE)
  
  centroids_spdf <- SpatialPointsDataFrame(as.data.frame(centroids@coords)[,c("x", "y")],
                                           as.data.frame(centroids@coords), proj4string = crs(gdd_2014))
  
  centroids_pts <- spatialEco::point.in.poly(centroids_spdf, grid_sp)
  
  model_df <- left_join(estimate_df3, centroids_pts@data)
  
  return(model_df)
}

# Estimate for species
a_celtis_model <- estimate_weib_onset("Asterocampa celtis", grid_sp)
s_cybele_model <- estimate_weib_onset("Speyeria cybele", grid_sp)
e_clarus_model <- estimate_weib_onset("Epargyreus clarus", grid_sp)
c_eurytheme_model <- estimate_weib_onset("Colias eurytheme", grid_sp)
e_comyntas_model <- estimate_weib_onset("Cupido comyntas", grid_sp)
p_tharos_model <- estimate_weib_onset("Phyciodes tharos", grid_sp)
p_rapae_model <- estimate_weib_onset("Pieris rapae", grid_sp)
p_interrogationis_model <- estimate_weib_onset("Polygonia interrogationis", grid_sp)

total_model <- rbind(a_celtis_model, s_cybele_model, e_clarus_model, c_eurytheme_model,
      e_comyntas_model, p_tharos_model, p_rapae_model, p_interrogationis_model)

# data vis plots

library(ggplot2)

ggplot(total_model, aes(x = gdd, y = onset)) +
  geom_point(aes(color = scientific_name)) +
  geom_smooth(aes(color = scientific_name), method = "lm")


# LMM

a_celtis_null <- lme4::lmer(onset ~ (1|x), data = a_celtis_model, REML = FALSE)
a_celtis_gdd <- lme4::lmer(onset ~ gdd + (1|x), data = a_celtis_model, REML = FALSE)
anova(a_celtis_gdd, a_celtis_null)


s_cybele_null <- lme4::lmer(onset ~ (1|x), data = s_cybele_model, REML = FALSE)
s_cybele_gdd <- lme4::lmer(onset ~ gdd + (1|x), data = s_cybele_model, REML = FALSE)
anova(s_cybele_gdd, s_cybele_null)

e_clarus_null <- lme4::lmer(onset ~ (1|x), data = e_clarus_model, REML = FALSE)
e_clarus_gdd <- lme4::lmer(onset ~ gdd + (1|x), data = e_clarus_model, REML = FALSE)
anova(e_clarus_gdd, e_clarus_null)

c_eurytheme_null <- lme4::lmer(onset ~ (1|x), data = c_eurytheme_model, REML = FALSE)
c_eurytheme_gdd <- lme4::lmer(onset ~ gdd + (1|x), data = c_eurytheme_model, REML = FALSE)
anova(c_eurytheme_gdd, c_eurytheme_null)

e_comyntas_null <- lme4::lmer(onset ~ (1|x), data = e_comyntas_model, REML = FALSE)
e_comyntas_gdd <- lme4::lmer(onset ~ gdd + (1|x), data = e_comyntas_model, REML = FALSE)
anova(e_comyntas_gdd, e_comyntas_null)

p_tharos_null <- lme4::lmer(onset ~ (1|x), data = p_tharos_model, REML = FALSE)
p_tharos_gdd <- lme4::lmer(onset ~ gdd + (1|x), data = p_tharos_model, REML = FALSE)
anova(p_tharos_gdd, p_tharos_null)

p_rapae_null <- lme4::lmer(onset ~ (1|x), data = p_rapae_model, REML = FALSE)
p_rapae_gdd <- lme4::lmer(onset ~ gdd + (1|x), data = p_rapae_model, REML = FALSE)
anova(p_rapae_gdd, p_rapae_null)

p_interrogationis_null <- lme4::lmer(onset ~ (1|x), data = p_interrogationis_model, REML = FALSE)
p_interrogationis_gdd <- lme4::lmer(onset ~ gdd + (1|x), data = p_interrogationis_model, REML = FALSE)
anova(p_interrogationis_gdd, p_interrogationis_null)

# Data Vis
gdd_df <- as.data.frame(gdd_2018, xy = TRUE)

plot(gdd_2018)

total_model_2018 <- filter(total_model, year == 2018)

ggplot() + 
  geom_raster(data = gdd_df, aes(x = x, y = y, fill = Mean_GDD_2018)) +
  geom_point(data = total_model_2018, aes(x = x, y = y, color = onset, shape = scientific_name))

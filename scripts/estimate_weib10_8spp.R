# load packages
library(sf)
library(spatialEco)
library(raster)
library(lubridate)
library(dplyr)
library(rgdal)
library(readr)
library(purrr)
library(ggplot2)
library(boot)
library(fitdistrplus)

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

# Read in spatial data

grid_1deg <- readOGR("data/gis", "grid_1deg")
grid_1deg$polyids <- 1:nrow(grid_1deg)


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
  dplyr::filter(longitude >= -94 & longitude <= -82) %>% 
  dplyr::filter(latitude >= 36 & latitude <= 42) %>% 
  dplyr::filter(id != 6653072) 


#' fucntion that takes vector list of observations and builds a weibull distribution 
#' of the data and pulls the 10% cummulative probability of the distribution

estimate_onset <- function(observations, percentile = 0.1){
  obs <- sort(observations, decreasing = FALSE)
  weib <- fitdist(obs, "weibull")
  estimate <- (weib$estimate[['scale']]*10) * (-log(1-percentile))^(1/(weib$estimate[['shape']]))/10
  return(estimate)
}

#' function to estimate onset given different iterations of vector

weibfun <- function(data, i){
  d <- data[i]
  return(estimate_onset(d))
}

#' functions that returns estimate of ci and low and high values given the data

estimate_ci <- function(observations){
  bootstrap <- boot(observations, weibfun, R = 2000)
  boot_ci <- boot.ci(bootstrap, conf = 0.95, type = "bca")
  low_ci <- boot_ci$bca[4]
  high_ci <- boot_ci$bca[5]
  ci <- boot_ci$bc[5] - boot_ci$bca[4]
  return(ci)
}

estimate_low_ci <- function(observations){
  bootstrap <- boot(observations, weibfun, R = 2000)
  boot_ci <- boot.ci(bootstrap, conf = 0.95, type = "bca")
  low_ci <- boot_ci$bca[4]
  high_ci <- boot_ci$bca[5]
  ci <- boot_ci$bc[5] - boot_ci$bca[4]
  return(low_ci)
}

estimate_high_ci <- function(observations){
  bootstrap <- boot(observations, weibfun, R = 2000)
  boot_ci <- boot.ci(bootstrap, conf = 0.95, type = "bca")
  low_ci <- boot_ci$bca[4]
  high_ci <- boot_ci$bca[5]
  ci <- boot_ci$bc[5] - boot_ci$bca[4]
  return(high_ci)
}

# Estimate  phenology for a given species

estimate_sp_pheno_weib <- function(binomial, polydeg){
  
  sp_df <- total_buts %>% 
    filter(scientific_name == binomial)
  
  spatial_pt_df <- SpatialPointsDataFrame(sp_df[,c("longitude", "latitude")],
                                          sp_df, proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
  
  
  polypts <- spatialEco::point.in.poly(spatial_pt_df, polydeg)
  
  species_df <- as.data.frame(polypts@data)
  
  species_df_manip <- species_df%>% 
    dplyr::select(day, polyids, scientific_name, year) %>% 
    group_by(polyids, day, year) %>% 
    summarise(obs = n())
  
  sp_df2 <- species_df_manip %>% 
    filter(!is.na(polyids)) %>% 
    group_by(polyids, year) %>% 
    summarise(total_obs = n())
  
  sp_df2 <- sp_df2 %>% 
    filter(total_obs >= 10)
  
  sp_df3 <- left_join(species_df_manip, sp_df2) %>% 
    filter(!is.na(total_obs)) %>% 
    filter(!is.na(polyids))
  
  estimate10 <- sp_df3 %>% 
    group_by(polyids, year) %>% 
    summarise(weib10 = estimate_onset(day), high_ci = estimate_high_ci(day), low_ci = estimate_low_ci(day))
  
  return(estimate10)
}

## Species list

s_cybele_est1 <- estimate_sp_pheno_weib("Speyeria cybele", grid_1deg)
c_comyntas_est1 <- estimate_sp_pheno_weib("Cupido comyntas", grid_1deg)
p_tharos_est1 <- estimate_sp_pheno_weib("Phyciodes tharos", grid_1deg)
a_celtis_est1 <- estimate_sp_pheno_weib("Asterocampa celtis", grid_1deg)
p_rapae_est1 <- estimate_sp_pheno_weib("Pieris rapae", grid_1deg)
c_eurytheme_est1 <- estimate_sp_pheno_weib("Colias eurytheme", grid_1deg)
e_clarus_est1 <- estimate_sp_pheno_weib("Epargyreus clarus", grid_1deg)
p_interrogationis_est1<- estimate_sp_pheno_weib("Polygonia interrogationis", grid_1deg)

# get data merged

mutate_est_df <- function(df, binomial){
  df <- df %>% 
    mutate(SciName = binomial)
  
}

# merge 8 species

s_cybele_est10 <- mutate_est_df(s_cybele_est1,"Speyeria cybele")
c_comyntas_est10 <- mutate_est_df(c_comyntas_est1,"Everes comyntas")
p_tharos_est10 <- mutate_est_df(p_tharos_est1,"Phyciodes tharos")
e_clarus_est10 <- mutate_est_df(e_clarus_est1,"Epargyreus clarus")
p_interrogationis_est10 <- mutate_est_df(p_interrogationis_est1,"Polygonia interrogationis")
a_celtis_est10 <- mutate_est_df(a_celtis_est1,"Asterocampa celtis")
p_rapae_est10 <- mutate_est_df(p_rapae_est1,"Pieris rapae")
c_eurytheme_est10 <- mutate_est_df(c_eurytheme_est1,"Colias eurytheme")


weib10_est_8spp <- rbind(s_cybele_est10,
                         c_comyntas_est10, 
                         p_tharos_est10, 
                         e_clarus_est10,
                         p_interrogationis_est10,
                         a_celtis_est10,
                         p_rapae_est10,
                         c_eurytheme_est10)

grid_1deg_df <- grid_1deg@data %>% 
  mutate(long_centroid = (left + right) / 2, lat_centroid = (top + bottom) / 2) 

joined_weib10_8spp <- left_join(weib10_est_8spp, grid_1deg_df, by = "polyids")

total_weib10_centroid <- left_join(joined_weib10_8spp, grid_1deg_df, by = "polyids")


# get grid centroids

grid_1deg_df <- grid_1deg@data %>% 
  mutate(long_centroid = (left + right) / 2, lat_centroid = (top + bottom) / 2) 


total_est_centroid <- left_join(total_est, grid_1deg_df, by = "polyids")




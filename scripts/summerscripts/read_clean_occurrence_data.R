library(tidyverse)

# import inat data

nymph1 <- read.csv(file = "data/iNat_families/observations-55528.csv", stringsAsFactors = FALSE)
nymph2 <- read.csv(file = "data/iNat_families/observations-55529.csv", stringsAsFactors = FALSE)
nymph3 <- read.csv(file = "data/iNat_families/observations-55530.csv", stringsAsFactors = FALSE)
hes <- read.csv(file = "data/iNat_families/observations-55531.csv", stringsAsFactors = FALSE)
rio <- read.csv(file = "data/iNat_families/observations-55532.csv", stringsAsFactors = FALSE)
pap <- read.csv(file = "data/iNat_families/observations-55533.csv", stringsAsFactors = FALSE)
lyc <- read.csv(file = "data/iNat_families/observations-55534.csv", stringsAsFactors = FALSE)
pier <- read.csv(file = "data/iNat_families/observations-55535.csv", stringsAsFactors = FALSE)

inat <- rbind(nymph1,nymph2,nymph3,hes,rio,pap,lyc,pier)

cats <- read.csv(file = "data/iNat_cats/observations-55525.csv", stringsAsFactors = FALSE)

# clean iNat data

inat_tidy <- inat %>% 
  dplyr::filter(coordinates_obscured == "false") %>% 
  dplyr::filter(captive_cultivated == "false") %>% 
  dplyr::filter(quality_grade == "research") 

inat_adults <- dplyr::anti_join(inat_tidy, cats, by = "id") %>% 
  dplyr::mutate(data_source = "iNaturalist") %>% 
  dplyr::mutate(day = lubridate::yday(observed_on)) %>% 
  dplyr::mutate(year = lubridate::year(observed_on)) %>% 
  dplyr::select(scientific_name, latitude, longitude, id, user_login, observed_on, day, year, data_source)

inat_adults <- inat_adults %>% 
  dplyr::filter(!is.na(longitude), !is.na(latitude)) %>% 
  dplyr::filter(longitude >= -170 & longitude <= -65) %>% 
  dplyr::filter(latitude >= 0 & latitude <= 90)

# import eButterfly data
ebutterfly <- read.csv("data/allrecords_2018_09_20_08_16_15_semi.csv", stringsAsFactors = F)

ebutlong <- parse_number(ebutterfly$Longitude)
ebutlat <- parse_number(ebutterfly$Latitude)

ebutterfly$Longitude <- ebutlong
ebutterfly$Latitude <- ebutlat

ebutterfly_tidy <- ebutterfly %>% 
  dplyr::filter(!is.na(Longitude)) %>% 
  dplyr::filter(!is.na(Latitude))%>%  
  dplyr::filter(Latitude > 0 & Latitude < 90) %>% 
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

# combine data sets
total_buts <- rbind(inat_adults, ebutterfly_tidy)

# first run incidental phenology estimates onset script

# Then run this script

library(MuMIn)

pt_onset <- total_onset_centroid %>% 
  filter(SciName == "Phyciodes tharos") %>% 
  select(polyids, SciName, year, onset, high_ci, low_ci, long_centroid.x, lat_centroid.x)

pt_onset_2018 <- pt_onset %>% 
  filter(year == 2018)

# LMM for Phyciodes tharos

pt_lmm <- lm(onset ~ lat_centroid.x + factor(year), data = pt_onset)
summary(pt_lmm)

pt_lmm_2018 <- lm(onset ~ lat_centroid.x, data = pt_onset_2018)
summary(pt_lmm_2018)


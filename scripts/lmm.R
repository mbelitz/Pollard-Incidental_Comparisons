library(MuMIn)

pt_onset <- total_est_centroid_8spp %>% 
  filter(SciName == "Phyciodes tharos") %>% 
  select(polyids, SciName, year, onset, high_ci, low_ci, long_centroid.x, lat_centroid.x)

pt_onset_2018 <- pt_onset %>% 
  filter(year == 2018)

# LMM for Phyciodes tharos

pt_lmm <- lm(onset ~ lat_centroid.x + factor(year), data = pt_onset)
summary(pt_lmm)

pt_lmm_2018 <- lm(onset ~ lat_centroid.x, data = pt_onset_2018)
summary(pt_lmm_2018)




# LMM for Epargyreus clarus
ec_onset <- total_est_centroid_8spp %>% 
  filter(SciName == "Epargyreus clarus") %>% 
  select(polyids, SciName, year, onset, high_ci, low_ci, long_centroid.x, lat_centroid.x)

ec_lmm <- lm(onset ~ lat_centroid.x + factor(year), data = ec_onset)
summary(ec_lmm)



# LMM for Pieris rapae
pr_onset <- total_est_centroid_8spp %>% 
  filter(SciName == "Pieris rapae") %>% 
  select(polyids, SciName, year, onset, high_ci, low_ci, long_centroid.x, lat_centroid.x)

pr_onset_2018 <- pr_onset %>% 
  filter(year == 2018) %>% 
  filter(onset > 0)

pr_lmm <- lm(onset~ lat_centroid.x + factor(year), data = pr_onset)
summary(pr_lmm)

pr_lmm_2018 <- lm(onset~ lat_centroid.x, data = pr_onset_2018)
summary(pr_lmm)


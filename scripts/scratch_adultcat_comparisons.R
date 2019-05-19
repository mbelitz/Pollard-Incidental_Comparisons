library(dplyr)
library(phest)
library(ggplot2)


inat <- read.csv(file = "data/pheno_mismatch/inat_adults_cats_ENA.csv", header = TRUE, stringsAsFactors = FALSE)

h_tessellaris_cat <- inat %>% 
  filter(scientific_name == "Halysidota tessellaris", life_stage == "caterpillar")

h_tessellaris_cat_group <- h_tessellaris_cat %>% 
  group_by(lat_bin, lon_bin, year, jday) %>% 
  summarise(day_count = n())

year_count <- h_tessellaris_cat_group %>% 
  group_by(lat_bin, lon_bin, year) %>% 
  summarise(year_count = n())

h_tessellaris_cat_group2 <- left_join(h_tessellaris_cat_group, year_count)

h_tessellaris_cat_group2 <- h_tessellaris_cat_group2 %>% 
  filter(year_count >= 10)

h_tessellaris_2018 <- h_tessellaris_cat_group2 %>% 
  filter(year == 2018)

estimate_df_cat <- h_tessellaris_2018 %>% 
  group_by(lat_bin, lon_bin, year) %>% 
  mutate(cat_onset = weib.limit(jday, upper = FALSE)[["estimate"]], 
         cat_high_ci = weib.limit(jday,upper = FALSE)[["upper-ci"]], 
         cat_low_ci = weib.limit(jday,upper = FALSE)[["lower-ci"]])

# Now let's do adults!!


h_tessellaris_adult <- inat %>% 
  filter(scientific_name == "Halysidota tessellaris", life_stage == "adult")

h_tessellaris_adult_group <- h_tessellaris_adult %>% 
  group_by(lat_bin, lon_bin, year, jday) %>% 
  summarise(day_count = n())

year_count_adult <- h_tessellaris_adult_group %>% 
  group_by(lat_bin, lon_bin, year) %>% 
  summarise(year_count = n())

h_tessellaris_adult_group2 <- left_join(h_tessellaris_adult_group, year_count_adult)

h_tessellaris_adult_group2 <- h_tessellaris_adult_group2 %>% 
  filter(year_count >= 10)

h_tessellaris_2018_adult <- h_tessellaris_adult_group2 %>% 
  filter(year == 2018)

estimate_df_adult <- h_tessellaris_2018_adult %>% 
  group_by(lat_bin, lon_bin, year) %>% 
  mutate(adult_onset = weib.limit(jday, upper = FALSE)[["estimate"]], 
         adult_high_ci = weib.limit(jday,upper = FALSE)[["upper-ci"]], 
         adult_low_ci = weib.limit(jday,upper = FALSE)[["lower-ci"]])

combined_df <- inner_join(estimate_df_adult, estimate_df_cat, by = c("lat_bin", "lon_bin")) %>% 
  mutate(diff_onset = adult_onset - cat_onset)

ggplot(combined_df) + 
  geom_point(aes(x = lat_bin, y = diff_onset)) + 
  geom_smooth(aes(x = lat_bin, y = diff_onset), method = "lm")

## visulaize

ggplot() +
  geom_histogram(data = estimate_df_cat, aes(x = jday), fill = "pink", alpha = 1) +
  geom_histogram(data = estimate_df_adult, aes(x = jday), fill = "purple", alpha = 0.5) +
  geom_vline(data = estimate_df_cat, aes(xintercept = cat_onset), size = 1, color = "red") +
  geom_vline(data = estimate_df_adult, aes(xintercept = adult_onset), size = 1, color = "blue") +
  facet_grid(lat_bin ~ lon_bin)


ggplot(estimate_df) +
  geom_histogram(aes(x = jday)) +
                   facet_grid(lat_bin ~ lon_bin)


                 
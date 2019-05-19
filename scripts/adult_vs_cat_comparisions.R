inat <- read.csv(file = "data/pheno_mismatch/inat_adults_cats_ENA.csv", header = TRUE, stringsAsFactors = FALSE)

estimate_cat_pheno <- function(binomial, yearofinterest){

  lifes <- inat %>% 
    filter(scientific_name == binomial, life_stage == "caterpillar")

  group <- lifes %>% 
    group_by (lat_bin, lon_bin, year, jday) %>% 
    summarise(day_count = n())
  
  year_count <- group %>% 
    group_by(lat_bin, lon_bin, year) %>% 
    summarise(year_count = n())
  
  group_year <- left_join(group, year_count) 
  
  group_year <- group_year %>% 
    filter(year == yearofinterest, year_count >=10)
  
  estimate_df <- group_year %>% 
    group_by(lat_bin, lon_bin, year) %>% 
    mutate(cat_onset = weib.limit(jday, upper = FALSE)[["estimate"]], 
           cat_ci = weib.limit(jday,upper = FALSE)[["upper-ci"]], 
           cat_low_ci = weib.limit(jday,upper = FALSE)[["lower-ci"]],
           life_stage = "caterpillar")
  
  return(estimate_df)
}

estimate_adult_pheno <- function(binomial, yearofinterest){
  
  lifes <- inat %>% 
    filter(scientific_name == binomial, life_stage == "adult")
  
  group <- lifes %>% 
    group_by (lat_bin, lon_bin, year, jday) %>% 
    summarise(day_count = n())
  
  year_count <- group %>% 
    group_by(lat_bin, lon_bin, year) %>% 
    summarise(year_count = n())
  
  group_year <- left_join(group, year_count) 
  
  group_year <- group_year %>% 
    filter(year == yearofinterest, year_count >=10)
  
  estimate_df <- group_year %>% 
    group_by(lat_bin, lon_bin, year) %>% 
    mutate(adult_onset = weib.limit(jday, upper = FALSE)[["estimate"]], 
           adult_ci = weib.limit(jday,upper = FALSE)[["upper-ci"]], 
           adult_low_ci = weib.limit(jday,upper = FALSE)[["lower-ci"]],
           life_stage = "adult")
  
  return(estimate_df)
}


## ggplot functions

plot_obs <- function(binomial, year.of.interest){
 plot_out <-  ggplot() +
    geom_histogram(data = estimate_adult_pheno(binomial, year = year.of.interest), aes(x = jday, fill = life_stage), alpha = 1) +
    geom_histogram(data = estimate_cat_pheno(binomial, year = year.of.interest), aes(x = jday, fill = life_stage), alpha = 0.5) +
    geom_vline(data = estimate_adult_pheno(binomial, year = year.of.interest), aes(xintercept = adult_onset), size = 1, color = "red") +
    geom_vline(data = estimate_cat_pheno(binomial, year = year.of.interest), aes(xintercept = cat_onset), size = 1, color = "blue") +
    facet_grid(lat_bin ~ lon_bin) +
   ggtitle(binomial) 

return(plot_out)
  
}

## look at differences in estimates over space

examine_differences <- function(binomial, year.of.interest = 2018){
  
  combined_df <- inner_join(estimate_adult_pheno(binomial, year.of.interest), estimate_cat_pheno(binomial, year.of.interest), by = c("lat_bin", "lon_bin")) %>% 
    mutate(diff_onset = adult_onset - cat_onset)
  
  plotz <- ggplot(combined_df) + 
    geom_point(aes(x = lat_bin, y = diff_onset)) + 
    geom_smooth(aes(x = lat_bin, y = diff_onset), method = "lm")
  
  return(combined_df)
}



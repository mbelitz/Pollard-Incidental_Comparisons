library(dplyr)

pollard_est <- read.csv("outputs/PollardGAMs_10ofpeak.csv", header = TRUE, stringsAsFactors = FALSE)

incidental_est <- read.csv("outputs/weib10_estimates.csv", header = TRUE, stringsAsFactors = FALSE)

incidental_est <- incidental_est %>% 
  rename(Scientific.Name = ï..Scientific.Name)

total_est <- left_join(pollard_est, incidental_est, by = c("longcell", "latcell", "year", "Scientific.Name"))

total_est_filter <- total_est %>% 
  filter(!is.na(inc_estimate)) %>% 
  filter(inc_estimate != "error")

total_est_filter$inc_estimate <- as.numeric(total_est_filter$inc_estimate)
total_est_filter$inc_high_ci <- as.numeric(total_est_filter$inc_high_ci)
total_est_filter$inc_low_ci <- as.numeric(total_est_filter$inc_low_ci)

str(total_est_filter)


total_est_filter <- total_est_filter %>% 
  mutate(overlap = pollard_em10b > inc_low_ci & pollard_em10b < inc_high_ci) %>% 
  filter(pollard_counts != 0)

ggplot(total_est_filter) + 
  geom_point(aes(x = Scientific.Name, y = pollard_em10b))

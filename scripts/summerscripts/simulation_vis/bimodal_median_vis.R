library(dplyr)
library(ggplot2)
library(cowplot)

# set up sims

bm_median <- read.csv("simulation_outputs/bimodal_median_sims.csv", stringsAsFactors = FALSE, header = TRUE)

bm_median$sd <- as.factor(bm_median$sd)
bm_median$obs <- as.factor(bm_median$obs)

bm_median.summary <- bm_median %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meanmedian = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

bm_median.summary.pass <- bm_median %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meanmedian = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

bm_median.summary.10 <- bm_median.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(bm_median.summary.10, aes(x = uid, y = meanmedian)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.45) +
  scale_fill_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(213.21)), color = "cyan3") +
  geom_hline(aes(yintercept = c(206.48)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

bm_median.10 <- bm_median %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmmedian_10obs <- ggplot(bm_median.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(213.21)), color = "cyan3") +
  geom_hline(aes(yintercept = c(206.48)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True median, Points are Outliers") +
  labs(x = "Observations SD", y = "median Estimates of 30 Trials") +
  theme_classic() 

# 20 obs

bm_median.20 <- bm_median %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmmedian_20obs <- ggplot(bm_median.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(213.21)), color = "cyan3") +
  geom_hline(aes(yintercept = c(206.48)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True median, Points are Outliers") +
  labs(x = "Observations SD", y = "median % Estimates of 30 Trials") +
  theme_classic()

# 50 obs

bm_median.50 <- bm_median %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmmedian_50obs <- ggplot(bm_median.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(213.21)), color = "cyan3") +
  geom_hline(aes(yintercept = c(206.48)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True median, Points are Outliers") +
  labs(x = "Observations SD", y = "median % Estimates of 30 Trials") +
  theme_classic()

bm_50_cp <- cowplot::plot_grid(bmmedian_10obs, bmmedian_20obs, bmmedian_50obs)

# pass summary

bm_medianset_pass_sum <- bm_median %>% 
  mutate(pass_num = ifelse(pass == TRUE, 1,0)) %>% 
  group_by(estimator, sd, obs) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci), sd_dis = sd(distance),
            mean_low_ci = mean(lowci - estimate), mean_high_ci = mean(highci - estimate))

bm_median_pass_sum <- bm_medianset_pass_sum %>% 
  mutate(uid = paste(estimator, sd, obs)) %>% 
  mutate(percent_right = pass / 30) %>% 
  mutate(perc = "bimodal median")

bm_dis <- ggplot(bm_median_pass_sum, aes(x = uid, y = abs(mean_dis))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = abs(mean_dis) - sd_dis, ymax = abs(mean_dis) +sd_dis)) +
  labs(x = "SD - Observations", y = "Days from True median") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bm_corr <- ggplot(bm_median_pass_sum, aes(x = uid, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "SD - Observations", y = "Percent of Estimates") + 
  ggtitle("Does the true median fall within the CIs?") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bm <- plot_grid(bm_dis, bm_corr, rel_widths = 1, rel_heights = 1.4)

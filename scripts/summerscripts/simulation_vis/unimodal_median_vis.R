library(dplyr)
library(ggplot2)

# set up sims

um_median <- read.csv("simulation_outputs/onset_try2/unimodal_median_sims2.csv", stringsAsFactors = FALSE, header = TRUE)

um_median$sd <- as.factor(um_median$sd)
um_median$obs <- as.factor(um_median$obs)
um_median$estimator <- "belitz"

um_median.summary <- um_median %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meanmedian = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

um_median.summary.pass <- um_median %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meanmedian = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

um_median.summary.10 <- um_median.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(um_median.summary.10, aes(x = uid, y = meanmedian)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(199.89)), color = "cyan3") +
  geom_hline(aes(yintercept = c(200.19)), color = "red") +
  geom_hline(aes(yintercept = c(199.80)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

um_median.10 <- um_median %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ummedian_10obs <- ggplot(um_median.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(199.89)), color = "cyan3") +
  geom_hline(aes(yintercept = c(200.19)), color = "red") +
  geom_hline(aes(yintercept = c(199.80)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True median, Points are Outliers") +
  labs(x = "Observatimedians SD", y = "median Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 20 obs

um_median.20 <- um_median %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

ummedian_20obs <- ggplot(um_median.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(199.89)), color = "cyan3") +
  geom_hline(aes(yintercept = c(200.19)), color = "red") +
  geom_hline(aes(yintercept = c(199.80)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True median, Points are Outliers") +
  labs(x = "Observatimedians SD", y = "median % Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 50 obs

um_median.50 <- um_median %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

ummedian_50obs <- ggplot(um_median.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(199.89)), color = "cyan3") +
  geom_hline(aes(yintercept = c(200.19)), color = "red") +
  geom_hline(aes(yintercept = c(199.80)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True median, Points are Outliers") +
  labs(x = "Observatimedians SD", y = "median % Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

um_50_cp <- cowplot::plot_grid(ummedian_10obs, ummedian_20obs, ummedian_50obs)

# pass summary

um_median_pass_sum <- um_median %>% 
  mutate(pass_num = ifelse(pass == TRUE, 1,0)) %>% 
  group_by(estimator, sd, obs) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci), sd_dis = sd(distance))

um_median_pass_sum <- um_median_pass_sum %>% 
  mutate(uid = paste(estimator, sd, obs)) %>% 
  mutate(percent_right = pass / 30) %>% 
  mutate(perc = "unimodal median")

um_dis <- ggplot(um_median_pass_sum, aes(x = uid, y = mean_dis)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  geom_errorbar(aes(ymin = mean_dis - sd_dis, ymax = mean_dis +sd_dis)) +
  labs(x = "SD - Observations", y = "Days from True median") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

um_corr <- ggplot(um_median_pass_sum, aes(x = uid, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "SD - Observations", y = "Percent of Estimates") + 
  ggtitle("Does the true median fall within the CIs?") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

um <- plot_grid(um_dis, um_corr, rel_widths = 1, rel_heights = 1.5)

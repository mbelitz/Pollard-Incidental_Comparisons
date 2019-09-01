library(dplyr)
library(ggplot2)

# set up sims

um_on <- read.csv("simulation_outputs/onset_try2/unimodal_onset_sims2.csv", stringsAsFactors = FALSE, header = TRUE)

um_on$sd <- as.factor(um_on$sd)
um_on$obs <- as.factor(um_on$obs)

um_on.summary <- um_on %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meanonset = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

um_on.summary.pass <- um_on %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meanonset = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

um_on.summary.10 <- um_on.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(um_on.summary.10, aes(x = uid, y = meanonset)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(162.68)), color = "cyan3") +
  geom_hline(aes(yintercept = c(119.92)), color = "red") +
  geom_hline(aes(yintercept = c(35.88)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

um_on.10 <- um_on %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

umonset_10obs <- ggplot(um_on.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(162.68)), color = "cyan3") +
  geom_hline(aes(yintercept = c(119.92)), color = "red") +
  geom_hline(aes(yintercept = c(35.88)), color = "blue") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True Onset, Points are Outliers") +
  labs(x = "Observations SD", y = "Onset Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 20 obs

um_onset.20 <- um_on %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

umonset_20obs <- ggplot(um_onset.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(162.68)), color = "cyan3") +
  geom_hline(aes(yintercept = c(119.92)), color = "red") +
geom_hline(aes(yintercept = c(35.88)), color = "blue") +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True onset, Points are Outliers") +
  labs(x = "Observations SD", y = "Onset % Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 50 obs

um_onset.50 <- um_on %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

umonset_50obs <- ggplot(um_onset.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(162.68)), color = "cyan3") +
  geom_hline(aes(yintercept = c(119.92)), color = "red") +
geom_hline(aes(yintercept = c(35.88)), color = "blue") +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True onset, Points are Outliers") +
  labs(x = "Observations SD", y = "Onset % Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

um_onset_cp <- cowplot::plot_grid(umonset_10obs, umonset_20obs, umonset_50obs)

# pass summary

um_onset_pass_sum <- um_on %>% 
  mutate(pass_num = ifelse(pass == TRUE, 1,0)) %>% 
  group_by(estimator, sd, obs) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci), sd_dis = sd(distance, na.rm = TRUE),
            mean_low_ci = mean(lowci - estimate,na.rm = TRUE), mean_high_ci = mean(highci - estimate,na.rm = TRUE),
            RMSE = rmse(actual =true_onset, predicted = estimate))

um_onset_pass_sum <- um_onset_pass_sum %>% 
  mutate(uid = paste(estimator, sd, obs)) %>% 
  mutate(percent_right = pass / 30) %>% 
  mutate(perc = "unimodal onset")

uo_dis <- ggplot(um_onset_pass_sum, aes(x = uid, y = mean_dis)) +
  geom_bar(stat = "identity", aes(fill = estimator)) +
  geom_errorbar(aes(ymin = mean_dis - sd_dis, ymax = mean_dis +sd_dis)) +
  labs(x = "SD - Observations", y = "Days from True Onset") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

uo_cor <- ggplot(um_onset_pass_sum, aes(x = uid, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = estimator)) +
  labs(x = "SD - Observations", y = "Percent of Estimates") + 
  ggtitle("Does the true onset fall within the CIs?") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

uo <- cowplot::plot_grid(uo_dis, uo_cor, ncol = 1)

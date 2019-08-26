library(dplyr)
library(ggplot2)

# set up sims

bm_on <- read.csv("simulation_outputs/bimodal_onset_sims.csv", stringsAsFactors = FALSE, header = TRUE)

bm_on$sd <- as.factor(bm_on$sd)
bm_on$obs <- as.factor(bm_on$obs)

bm_on.summary <- bm_on %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meanonset = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

bm_on.summary.pass <- bm_on %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meanonset = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

bm_on.summary.10 <- bm_on.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(bm_on.summary.10, aes(x = uid, y = meanonset)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(113.29)), color = "cyan3") +
  geom_hline(aes(yintercept = c(76.58)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

bm_on.10 <- bm_on %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmonset_10obs <- ggplot(bm_on.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(113.29)), color = "cyan3") +
  geom_hline(aes(yintercept = c(76.58)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True Onset, Points are Outliers") +
  labs(x = "Observations SD", y = "Onset Estimates of 30 Trials") +
  theme_classic()

# 20 obs

bm_onset.20 <- bm_on %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmonset_20obs <- ggplot(bm_onset.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(113.29)), color = "cyan3") +
  geom_hline(aes(yintercept = c(76.58)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True onset, Points are Outliers") +
  labs(x = "Observations SD", y = "Onset % Estimates of 30 Trials") +
  theme_classic()

# 50 obs

bm_onset.50 <- bm_on %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmonset_50obs <- ggplot(bm_onset.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(113.29)), color = "cyan3") +
  geom_hline(aes(yintercept = c(76.58)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True onset, Points are Outliers") +
  labs(x = "Observations SD", y = "Onset % Estimates of 30 Trials") +
  theme_classic()

bm_onset_cp <- cowplot::plot_grid(bmonset_10obs, bmonset_20obs, bmonset_50obs)

# pass summary

bm_onset_pass_sum <- bm_on %>% 
  mutate(pass_num = ifelse(pass == TRUE, 1,0)) %>% 
  group_by(estimator, sd, obs) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci), sd_dis = sd(distance))

bm_onset_pass_sum <- bm_onset_pass_sum %>% 
  mutate(uid = paste(estimator, sd, obs)) %>% 
  mutate(percent_right = pass / 30) %>% 
  mutate(perc = "bimodal onset")

bo_dis <- ggplot(bm_onset_pass_sum, aes(x = uid, y = abs(mean_dis))) +
  geom_bar(stat = "identity", aes(fill = estimator)) +
  geom_errorbar(aes(ymin = abs(mean_dis) - sd_dis, ymax = abs(mean_dis) +sd_dis)) +
  labs(x = "SD - Observations", y = "Days from True Onset") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bo_corr <- ggplot(bm_onset_pass_sum, aes(x = uid, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = estimator)) +
  labs(x = "SD - Observations", y = "Percent of Estimates") + 
  ggtitle("Does the true onset fall within the CIs?") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bo <- plot_grid(bo_dis, bo_corr, rel_widths = 1, rel_heights = 1.4)

            
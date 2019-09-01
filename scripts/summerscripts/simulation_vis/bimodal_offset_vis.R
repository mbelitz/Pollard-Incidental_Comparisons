library(dplyr)
library(ggplot2)
library(cowplot)

# set up sims

bm_offset <- read.csv("simulation_outputs/bimodal_offset_sims.csv", stringsAsFactors = FALSE, header = TRUE)

bm_offset$sd <- as.factor(bm_offset$sd)
bm_offset$obs <- as.factor(bm_offset$obs)

bm_offset.summary <- bm_offset %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meanoffset = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

bm_offset.summary.pass <- bm_offset %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meanoffset = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

bm_offset.summary.10 <- bm_offset.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(bm_offset.summary.10, aes(x = uid, y = meanoffset)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.45) +
  scale_fill_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(257.28)), color = "cyan3") +
  geom_hline(aes(yintercept = c(294.56)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

bm_offset.10 <- bm_offset %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmoffset_10obs <- ggplot(bm_offset.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(257.28)), color = "cyan3") +
  geom_hline(aes(yintercept = c(294.56)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True offset, Points are Outliers") +
  labs(x = "Observations SD", y = "offset Estimates of 30 Trials") +
  theme_classic() 

# 20 obs

bm_offset.20 <- bm_offset %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmoffset_20obs <- ggplot(bm_offset.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(257.28)), color = "cyan3") +
  geom_hline(aes(yintercept = c(294.56)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True offset, Points are Outliers") +
  labs(x = "Observations SD", y = "offset % Estimates of 30 Trials") +
  theme_classic()

# 50 obs

bm_offset.50 <- bm_offset %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmoffset_50obs <- ggplot(bm_offset.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(257.28)), color = "cyan3") +
  geom_hline(aes(yintercept = c(294.56)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True offset, Points are Outliers") +
  labs(x = "Observations SD", y = "offset % Estimates of 30 Trials") +
  theme_classic()

bm_offset_cp <- cowplot::plot_grid(bmoffset_10obs, bmoffset_20obs, bmoffset_50obs)

# pass summary

bm_offsetset_pass_sum <- bm_offset %>% 
  mutate(pass_num = ifelse(pass == TRUE, 1,0)) %>% 
  group_by(estimator, sd, obs) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci), sd_dis = sd(distance, na.rm = TRUE),
            mean_low_ci = mean(lowci - estimate,na.rm = TRUE), mean_high_ci = mean(highci - estimate,na.rm = TRUE),
            RMSE = rmse(actual =true_offset, predicted = estimate))

bm_offset_pass_sum <- bm_offsetset_pass_sum %>% 
  mutate(uid = paste(estimator, sd, obs)) %>% 
  mutate(percent_right = pass / 30) %>% 
  mutate(perc = "bimodal offset")

bf_dis <- ggplot(bm_offset_pass_sum, aes(x = uid, y = abs(mean_dis))) +
  geom_bar(stat = "identity", aes(fill = estimator)) +
  geom_errorbar(aes(ymin = abs(mean_dis) - sd_dis, ymax = abs(mean_dis) +sd_dis)) +
  labs(x = "SD - Observations", y = "Days from True offset") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bf_corr <- ggplot(bm_offset_pass_sum, aes(x = uid, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = estimator)) +
  labs(x = "SD - Observations", y = "Percent of Estimates") + 
  ggtitle("Does the true offset fall within the CIs?") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bf <- plot_grid(bf_dis, bf_corr)

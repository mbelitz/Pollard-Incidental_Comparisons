library(dplyr)
library(ggplot2)
library(cowplot)

# set up sims

bm_ninty <- read.csv("simulation_outputs/bimodal_ninty_sims.csv", stringsAsFactors = FALSE, header = TRUE)

bm_ninty$sd <- as.factor(bm_ninty$sd)
bm_ninty$obs <- as.factor(bm_ninty$obs)

bm_ninty.summary <- bm_ninty %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meanninty = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

bm_ninty.summary.pass <- bm_ninty %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meanninty = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

bm_ninty.summary.10 <- bm_ninty.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(bm_ninty.summary.10, aes(x = uid, y = meanninty)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.45) +
  scale_fill_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(230.27)), color = "cyan3") +
  geom_hline(aes(yintercept = c(240.55)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

bm_ninty.10 <- bm_ninty %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmninty_10obs <- ggplot(bm_ninty.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(230.27)), color = "cyan3") +
  geom_hline(aes(yintercept = c(240.55)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True ninty, Points are Outliers") +
  labs(x = "Observations SD", y = "ninty Estimates of 30 Trials") +
  theme_classic() 

# 20 obs

bm_ninty.20 <- bm_ninty %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmninty_20obs <- ggplot(bm_ninty.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(230.27)), color = "cyan3") +
  geom_hline(aes(yintercept = c(240.55)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True ninty, Points are Outliers") +
  labs(x = "Observations SD", y = "ninty % Estimates of 30 Trials") +
  theme_classic()

# 50 obs

bm_ninty.50 <- bm_ninty %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmninty_50obs <- ggplot(bm_ninty.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(230.27)), color = "cyan3") +
  geom_hline(aes(yintercept = c(240.55)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True ninty, Points are Outliers") +
  labs(x = "Observations SD", y = "ninty % Estimates of 30 Trials") +
  theme_classic()

bm_90_cp <- cowplot::plot_grid(bmninty_10obs, bmninty_20obs, bmninty_50obs)

# pass summary

bm_nintyset_pass_sum <- bm_ninty %>% 
  mutate(pass_num = ifelse(pass == TRUE, 1,0)) %>% 
  group_by(estimator, sd, obs) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci), sd_dis = sd(distance, na.rm = TRUE),
            mean_low_ci = mean(lowci - estimate,na.rm = TRUE), mean_high_ci = mean(highci - estimate,na.rm = TRUE),
            RMSE = rmse(actual =true_ninty, predicted = estimate))

bm_ninty_pass_sum <- bm_nintyset_pass_sum %>% 
  mutate(uid = paste(estimator, sd, obs)) %>% 
  mutate(percent_right = pass / 30) %>% 
  mutate(perc = "bimodal ninty")

bn_dis <- ggplot(bm_ninty_pass_sum, aes(x = uid, y = abs(mean_dis))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = abs(mean_dis) - sd_dis, ymax = abs(mean_dis) +sd_dis)) +
  labs(x = "SD - Observations", y = "Days from True ninty") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bn_corr <- ggplot(bm_ninty_pass_sum, aes(x = uid, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = estimator)) +
  labs(x = "SD - Observations", y = "Percent of Estimates") + 
  ggtitle("Does the true ninty fall within the CIs?") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bn <- plot_grid(bn_dis, bn_corr, rel_widths = 1, rel_heights = 1.4)

library(dplyr)
library(ggplot2)
library(cowplot)

# set up sims

bm_tenth <- read.csv("simulation_outputs/bimodal_tenth_sims.csv", stringsAsFactors = FALSE, header = TRUE)

bm_tenth$sd <- as.factor(bm_tenth$sd)
bm_tenth$obs <- as.factor(bm_tenth$obs)

bm_tenth.summary <- bm_tenth %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meantenth = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

bm_tenth.summary.pass <- bm_tenth %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meantenth = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

bm_tenth.summary.10 <- bm_tenth.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(bm_tenth.summary.10, aes(x = uid, y = meantenth)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.45) +
  scale_fill_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(144.65)), color = "cyan3") +
  geom_hline(aes(yintercept = c(139.29)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

bm_tenth.10 <- bm_tenth %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmtenth_10obs <- ggplot(bm_tenth.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(144.65)), color = "cyan3") +
  geom_hline(aes(yintercept = c(139.29)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True tenth, Points are Outliers") +
  labs(x = "Observations SD", y = "tenth Estimates of 30 Trials") +
  theme_classic() 

# 20 obs

bm_tenth.20 <- bm_tenth %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmtenth_20obs <- ggplot(bm_tenth.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(144.65)), color = "cyan3") +
  geom_hline(aes(yintercept = c(139.29)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True tenth, Points are Outliers") +
  labs(x = "Observations SD", y = "Tenth % Estimates of 30 Trials") +
  theme_classic()

# 50 obs

bm_tenth.50 <- bm_tenth %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

bmtenth_50obs <- ggplot(bm_tenth.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "navy")) +
  geom_hline(aes(yintercept = c(144.65)), color = "cyan3") +
  geom_hline(aes(yintercept = c(139.29)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True tenth, Points are Outliers") +
  labs(x = "Observations SD", y = "Tenth % Estimates of 30 Trials") +
  theme_classic()

bm_10_cp <- cowplot::plot_grid(bmtenth_10obs, bmtenth_20obs, bmtenth_50obs)

# pass summary

bm_tenthset_pass_sum <- bm_tenth %>% 
  mutate(pass_num = ifelse(pass == TRUE, 1,0)) %>% 
  group_by(estimator, sd, obs) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci), sd_dis = sd(distance),
            mean_low_ci = mean(lowci - estimate), mean_high_ci = mean(highci - estimate))

bm_tenth_pass_sum <- bm_tenthset_pass_sum %>% 
  mutate(uid = paste(estimator, sd, obs)) %>% 
  mutate(percent_right = pass / 30) %>% 
  mutate(perc = "bimodal tenth")

bt_dis <- ggplot(bm_tenth_pass_sum, aes(x = uid, y = abs(mean_dis))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = abs(mean_dis) - sd_dis, ymax = abs(mean_dis) +sd_dis)) +
  labs(x = "SD - Observations", y = "Days from True tenth") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bt_corr <- ggplot(bm_tenth_pass_sum, aes(x = uid, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "SD - Observations", y = "Percent of Estimates") + 
  ggtitle("Does the true tenth fall within the CIs?") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

bt <- plot_grid(bt_dis, bt_corr, rel_widths = 1, rel_heights = 1.4)


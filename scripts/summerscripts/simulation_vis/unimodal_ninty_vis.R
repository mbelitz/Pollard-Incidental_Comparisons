library(dplyr)
library(ggplot2)

# set up sims

um_ninty <- read.csv("simulation_outputs/onset_try2/unimodal_ninty_sims2.csv", stringsAsFactors = FALSE, header = TRUE)

um_ninty$sd <- as.factor(um_ninty$sd)
um_ninty$obs <- as.factor(um_ninty$obs)
um_ninty$estimator <- "belitz"

um_ninty.summary <- um_ninty %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meanninty = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

um_ninty.summary.pass <- um_ninty %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meanninty = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

um_ninty.summary.10 <- um_ninty.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(um_ninty.summary.10, aes(x = uid, y = meanninty)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(212.76)), color = "cyan3") +
  geom_hline(aes(yintercept = c(225.60)), color = "red") +
  geom_hline(aes(yintercept = c(251.60)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

um_ninty.10 <- um_ninty %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

umninty_10obs <- ggplot(um_ninty.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(212.76)), color = "cyan3") +
  geom_hline(aes(yintercept = c(225.60)), color = "red") +
  geom_hline(aes(yintercept = c(251.60)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True ninty, Points are Outliers") +
  labs(x = "Observatinintys SD", y = "ninty Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 20 obs

um_ninty.20 <- um_ninty %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

umninty_20obs <- ggplot(um_ninty.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(212.76)), color = "cyan3") +
  geom_hline(aes(yintercept = c(225.60)), color = "red") +
  geom_hline(aes(yintercept = c(251.60)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True ninty, Points are Outliers") +
  labs(x = "Observatinintys SD", y = "ninty % Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 50 obs

um_ninty.50 <- um_ninty %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

umninty_50obs <- ggplot(um_ninty.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(212.76)), color = "cyan3") +
  geom_hline(aes(yintercept = c(225.60)), color = "red") +
  geom_hline(aes(yintercept = c(251.60)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True ninty, Points are Outliers") +
  labs(x = "Observatinintys SD", y = "ninty % Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

um_90_cp <- cowplot::plot_grid(umninty_10obs, umninty_20obs, umninty_50obs)

# pass summary

um_ninty_pass_sum <- um_ninty %>% 
  mutate(pass_num = ifelse(pass == TRUE, 1,0)) %>% 
  group_by(estimator, sd, obs) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci), sd_dis = sd(distance),
            mean_low_ci = mean(lowci - estimate), mean_high_ci = mean(highci - estimate))

um_ninty_pass_sum <- um_ninty_pass_sum %>% 
  mutate(uid = paste(estimator, sd, obs)) %>% 
  mutate(percent_right = pass / 30) %>% 
  mutate(perc = "unimodal ninty")

un_dis <- ggplot(um_ninty_pass_sum, aes(x = uid, y = mean_dis)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  geom_errorbar(aes(ymin = mean_dis - sd_dis, ymax = mean_dis +sd_dis)) +
  labs(x = "SD - Observations", y = "Days from True ninty") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

un_corr <- ggplot(um_ninty_pass_sum, aes(x = uid, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "SD - Observations", y = "Percent of Estimates") + 
  ggtitle("Does the true ninty fall within the CIs?") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

un <- plot_grid(un_dis, un_corr, rel_widths = 1, rel_heights = 1.5)


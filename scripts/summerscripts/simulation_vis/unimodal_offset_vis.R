library(dplyr)
library(ggplot2)

# set up sims

um_offset <- read.csv("simulation_outputs/unimodal_offset_sims.csv", stringsAsFactors = FALSE, header = TRUE)

um_offset$sd <- as.factor(um_offset$sd)
um_offset$obs <- as.factor(um_offset$obs)

um_offset.summary <- um_offset %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meanoffset = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

um_offset.summary.pass <- um_offset %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meanoffset = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

um_offset.summary.10 <- um_offset.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(um_offset.summary.10, aes(x = uid, y = meanoffset)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(238)), color = "cyan3") +
  geom_hline(aes(yintercept = c(279.35)), color = "red") +
  geom_hline(aes(yintercept = c(361.38)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

um_offset.10 <- um_offset %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

umoffset_10obs <- ggplot(um_offset.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(238)), color = "cyan3") +
  geom_hline(aes(yintercept = c(279.35)), color = "red") +
  geom_hline(aes(yintercept = c(361.38)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True offset, Points are Outliers") +
  labs(x = "Observations SD", y = "offset Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 20 obs

um_offset.20 <- um_offset %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

umoffset_20obs <- ggplot(um_offset.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(238)), color = "cyan3") +
  geom_hline(aes(yintercept = c(279.35)), color = "red") +
  geom_hline(aes(yintercept = c(361.38)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True offset, Points are Outliers") +
  labs(x = "Observations SD", y = "offset % Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 50 obs

um_offset.50 <- um_offset %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

umoffset_50obs <- ggplot(um_offset.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(238)), color = "cyan3") +
  geom_hline(aes(yintercept = c(279.35)), color = "red") +
  geom_hline(aes(yintercept = c(361.38)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True offset, Points are Outliers") +
  labs(x = "Observations SD", y = "offset % Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

um_offset_cp <- cowplot::plot_grid(umoffset_10obs, umoffset_20obs, umoffset_50obs)

# pass summary

um_offset_pass_sum <- um_offset %>% 
  mutate(pass_num = ifelse(pass == TRUE, 1,0)) %>% 
  group_by(estimator, sd, obs) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci))

um_offset_pass_sum <- um_offset_pass_sum %>% 
  mutate(uid = paste(estimator, sd, obs)) %>% 
  mutate(percent_right = pass / 30) %>% 
  mutate(perc = "unimodal offset")

uf_dis <- ggplot(um_offset_pass_sum, aes(x = uid, y = abs(mean_dis))) +
  geom_bar(stat = "identity", aes(fill = estimator)) +
  geom_errorbar(aes(ymin = abs(mean_dis) - mean_ci/2, ymax = abs(mean_dis) + mean_ci/2)) +
  labs(x = "SD - Observations", y = "Days from True offset") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

uf_corr <- ggplot(um_offset_pass_sum, aes(x = uid, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = estimator)) +
  labs(x = "SD - Observations", y = "Percent of Estimates") + 
  ggtitle("Does the true offset fall within the CIs?") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

uf <- plot_grid(uf_dis, uf_corr, nrow = 2)

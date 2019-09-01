library(dplyr)
library(ggplot2)

# set up sims

um_tenth <- read.csv("simulation_outputs/onset_try2/unimodal_tenth_sims2.csv", stringsAsFactors = FALSE, header = TRUE)

um_tenth$sd <- as.factor(um_tenth$sd)
um_tenth$obs <- as.factor(um_tenth$obs)
um_tenth$estimator <- "belitz"

um_tenth.summary <- um_tenth %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meantenth = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

um_tenth.summary.pass <- um_tenth %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meantenth = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

um_tenth.summary.10 <- um_tenth.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(um_tenth.summary.10, aes(x = uid, y = meantenth)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(137.17)), color = "cyan3") +
  geom_hline(aes(yintercept = c(124.8)), color = "red") +
  geom_hline(aes(yintercept = c(99.24)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

um_tenth.10 <- um_tenth %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

umtenth_10obs <- ggplot(um_tenth.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(137.17)), color = "cyan3") +
  geom_hline(aes(yintercept = c(124.8)), color = "red") +
  geom_hline(aes(yintercept = c(99.24)), color = "blue") + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True tenth, Points are Outliers") +
  labs(x = "Observations SD", y = "tenth Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 20 obs

um_tenth.20 <- um_tenth %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

umtenth_20obs <- ggplot(um_tenth.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(137.17)), color = "cyan3") +
  geom_hline(aes(yintercept = c(124.8)), color = "red") +
  geom_hline(aes(yintercept = c(99.24)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True tenth, Points are Outliers") +
  labs(x = "Observations SD", y = "tenth % Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 50 obs

um_tenth.50 <- um_tenth %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

umtenth_50obs <- ggplot(um_tenth.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd, fill = estimator)) + 
  scale_fill_manual(values = c("light grey", "white")) +
  scale_color_manual(values = c("cyan3", "red", "navy")) +
  geom_hline(aes(yintercept = c(137.17)), color = "cyan3") +
  geom_hline(aes(yintercept = c(124.8)), color = "red") +
  geom_hline(aes(yintercept = c(99.24)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True tenth, Points are Outliers") +
  labs(x = "Observations SD", y = "tenth % Estimates of 30 Trials") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

um_10_cp <- cowplot::plot_grid(umtenth_10obs, umtenth_20obs, umtenth_50obs)

# pass summary

um_tenth_pass_sum <- um_tenth %>% 
  mutate(pass_num = ifelse(pass == TRUE, 1,0)) %>% 
  group_by(estimator, sd, obs) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci), sd_dis = sd(distance, na.rm = TRUE),
            mean_low_ci = mean(lowci - estimate,na.rm = TRUE), mean_high_ci = mean(highci - estimate,na.rm = TRUE),
            RMSE = rmse(actual =true_tenth, predicted = estimate))

um_tenth_pass_sum <- um_tenth_pass_sum %>% 
  mutate(uid = paste(estimator, sd, obs)) %>% 
  mutate(percent_right = pass / 30) %>% 
  mutate(perc = "unimodal tenth")

ut_dis <- ggplot(um_tenth_pass_sum, aes(x = uid, y = mean_dis)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  geom_errorbar(aes(ymin = mean_dis - sd_dis, ymax = mean_dis +sd_dis)) +
  labs(x = "SD - Observations", y = "Days from True tenth") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ut_corr <- ggplot(um_tenth_pass_sum, aes(x = uid, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "SD - Observations", y = "Percent of Estimates") + 
  ggtitle("Does the true tenth fall within the CIs?") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ut <- cowplot::plot_grid(ut_dis, ut_corr, rel_widths = 1, rel_heights = 1.5)

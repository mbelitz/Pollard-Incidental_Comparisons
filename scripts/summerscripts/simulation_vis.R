library(ggplot2)

total_sims <- read.csv("outputs/total_sims.csv", stringsAsFactors = FALSE, header = TRUE)

total_sims$sd <- as.factor(total_sims$sd)
total_sims$obs <- as.factor(total_sims$obs)

str(total_sims)

# 10 obs

ten.sim.summary.10 <- total_sims %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(obs, sd))

bp10per_10obs <- ggplot(ten.sim.summary.10) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd)) + 
  scale_color_manual(values = c("red", "cyan3", "navy")) +
  geom_hline(aes(yintercept = c(137.17)), color = "red") +
  geom_hline(aes(yintercept = c(124.8)), color = "cyan3") +
  geom_hline(aes(yintercept = c(99.24)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True 10%, Points are Outliers") +
  labs(x = "Observations SD", y = "10% Estimates of 30 Trials") +
  theme_classic()

# 20 obs

ten.sim.summary.20 <- total_sims %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(obs, sd))

bp10per_20obs <- ggplot(ten.sim.summary.20) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd)) + 
  scale_color_manual(values = c("red", "cyan3", "navy")) +
  geom_hline(aes(yintercept = c(137.17)), color = "red") +
  geom_hline(aes(yintercept = c(124.8)), color = "cyan3") +
  geom_hline(aes(yintercept = c(99.24)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True 10%, Points are Outliers") +
  labs(x = "Observations SD", y = "10% Estimates of 30 Trials") +
  theme_classic()

# 50 obs

ten.sim.summary.50 <- total_sims %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(obs, sd))

bp10per_50obs <- ggplot(ten.sim.summary.50) +
  geom_boxplot(aes(x = uid, y = estimate, color = sd)) + 
  scale_color_manual(values = c("red", "cyan3", "navy")) +
  geom_hline(aes(yintercept = c(137.17)), color = "red") +
  geom_hline(aes(yintercept = c(124.8)), color = "cyan3") +
  geom_hline(aes(yintercept = c(99.24)), color = "blue") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True 10%, Points are Outliers") +
  labs(x = "Observations SD", y = "10% Estimates of 30 Trials") +
  theme_classic()


ggplot(total_sims) + 
  geom_boxplot(aes(x = ob_sd, y = estimate, fill = obs, color = sd)) +
  scale_color_manual(values = c("black", "grey", "navy")) +
  geom_hline(aes(yintercept = expected)) +
  theme_bw()



ggplot(total_sims) + 
  geom_boxplot(aes(x = ob_sd, y = distance, fill = obs, color = sd)) +
  scale_color_manual(values = c("black", "grey", "navy"))


summary_sims <- total_sims %>% 
  group_by(ob_sd) %>% 
  summarise(pass = sum(pass_num), mean_dis = mean(distance), mean_ci = mean(ci))

summary_sims$pass[9] <-  17

ggplot(summary_sims, aes(x = ob_sd, y = abs(mean_dis))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = abs(mean_dis) - mean_ci/2, ymax = abs(mean_dis) + mean_ci/2))

summary_sims <- summary_sims %>% 
  mutate(percent_right = pass/20)

ggplot(summary_sims, aes(x = ob_sd, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = ob_sd)) + 
  ylim(min = 0, max = 1)
  
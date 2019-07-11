library(ggplot2)
library(dplyr)

ten.sim <- read.csv(file = "outputs/total_sims.csv", stringsAsFactors = FALSE,
                      h = TRUE)

ten.sim$sd <- as.factor(ten.sim$sd)
ten.sim$obs <- as.factor(ten.sim$obs)


ten.sim.summary <- ten.sim %>% 
  group_by(obs, sd) %>% 
  summarize(count = n(), meanten = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))


ten.sim.summary.pass <- ten.sim %>% 
  group_by(obs, sd, pass) %>% 
  summarize(count = n(), meanten = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

ten.sim.summary.10 <- ten.sim.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(obs, sd))

ggplot(ten.sim.summary.10, aes(x = uid, y = meanten)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("red", "cyan3", "navy")) +
  geom_hline(aes(yintercept = c(137.17)), color = "red") +
  geom_hline(aes(yintercept = c(124.8)), color = "cyan3") +
  geom_hline(aes(yintercept = c(99.24)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "10 Observations", subtitle = "Lines are True 10%") +
  labs(x = "Observations SD", y = "Average 10% Estimate of 30 Trials") +
  theme_classic()

# 20 obs

ten.sim.summary.20 <- ten.sim.summary %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(obs, sd))

ggplot(ten.sim.summary.20, aes(x = uid, y = meanten)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("red", "cyan3", "navy")) +
  geom_hline(aes(yintercept = c(137.17)), color = "red") +
  geom_hline(aes(yintercept = c(124.8)), color = "cyan3") +
  geom_hline(aes(yintercept = c(99.24)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "20 Observations", subtitle = "Lines are True 10%") +
  labs(x = "Observations SD", y = "Average 10% Estimate of 30 Trials") +
  theme_classic()

# 50 obs

ten.sim.summary.50 <- ten.sim.summary %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(obs, sd))

ggplot(ten.sim.summary.50, aes(x = uid, y = meanten)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("red", "cyan3", "navy")) +
  geom_hline(aes(yintercept = c(137.17)), color = "red") +
  geom_hline(aes(yintercept = c(124.8)), color = "cyan3") +
  geom_hline(aes(yintercept = c(99.24)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(label = "50 Observations", subtitle = "Lines are True 10%") +
  labs(x = "Observations SD", y = "Average 10% Estimate of 30 Trials") +
  theme_classic()

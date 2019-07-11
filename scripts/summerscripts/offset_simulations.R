library(ggplot2)
library(dplyr)

offst.sim <- read.csv(file = "outputs/totaloffset_sims.csv", stringsAsFactors = FALSE,
                      h = TRUE)

offst.sim$sd <- as.factor(offst.sim$sd)
offst.sim$obs <- as.factor(offst.sim$obs)


offst.sim.summary <- offst.sim %>% 
  group_by(obs, sd, estimator) %>% 
  summarize(count = n(), meanoffst = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))


offst.sim.summary.pass <- offst.sim %>% 
  group_by(obs, sd, estimator, pass) %>% 
  summarize(count = n(), meanoffst = mean(estimate), meandis = mean(distance),
            meanci = mean(ci), meanhighci = mean(highci), meanlowci = mean(lowci))

# 10 obs

offst.sim.summary.10 <- offst.sim.summary %>% 
  filter(obs == "10") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(offst.sim.summary.10, aes(x = uid, y = meanoffst)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("red", "cyan3", "navy")) +
  geom_hline(aes(yintercept = c(238.1)), color = "red") +
  geom_hline(aes(yintercept = c(279.35)), color = "cyan3") +
  geom_hline(aes(yintercept = c(361.38)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

# 20 obs

offst.sim.summary.20 <- offst.sim.summary %>% 
  filter(obs == "20") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(offst.sim.summary.20, aes(x = uid, y = meanoffst)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("red", "cyan3", "navy")) +
  geom_hline(aes(yintercept = c(238.1)), color = "red") +
  geom_hline(aes(yintercept = c(279.35)), color = "cyan3") +
  geom_hline(aes(yintercept = c(361.38)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

# 50 obs

offst.sim.summary.50 <- offst.sim.summary %>% 
  filter(obs == "50") %>% 
  mutate(uid = paste(estimator, obs, sd))

ggplot(offst.sim.summary.50, aes(x = uid, y = meanoffst)) + 
  geom_bar(stat = "identity", aes(fill = sd), alpha = 0.65) +
  scale_fill_manual(values = c("red", "cyan3", "navy")) +
  geom_hline(aes(yintercept = c(238.1)), color = "red") +
  geom_hline(aes(yintercept = c(279.35)), color = "cyan3") +
  geom_hline(aes(yintercept = c(361.38)), color = "blue") + 
  geom_errorbar(aes(ymin = meanlowci, ymax = meanhighci)) +
  theme_classic()

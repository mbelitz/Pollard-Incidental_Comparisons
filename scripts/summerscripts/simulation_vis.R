library(ggplot2)

total_sims <- read.csv("outputs/total_sims.csv", stringsAsFactors = FALSE, header = TRUE)

total_sims$sd <- as.factor(total_sims$sd)
total_sims$obs <- as.factor(total_sims$obs)

str(total_sims)

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

ggplot(df2, aes(x=dose, y=len, fill=supp))

ggplot(summary_sims, aes(x = ob_sd, y = abs(mean_dis))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = abs(mean_dis) - mean_ci/2, ymax = abs(mean_dis) + mean_ci/2))

summary_sims <- summary_sims %>% 
  mutate(percent_right = pass/20)

ggplot(summary_sims, aes(x = ob_sd, y = percent_right)) +
  geom_bar(stat = "identity", aes(fill = ob_sd)) + 
  ylim(min = 0, max = 1)
  
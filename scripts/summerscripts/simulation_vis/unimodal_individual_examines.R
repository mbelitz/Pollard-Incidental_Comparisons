# Examine individual outputs of simulation

source("scripts/summerscripts/simulation_vis/simulation_setup.R")

bm_tenth <- read.csv("simulation_outputs/bimodal_tenth_sims.csv", stringsAsFactors = FALSE, header = TRUE)
bm_tenth_df <- as.data.frame(split(unlist(list_10obs_10sd),1:10))

for(i in 1:30){
  
plot <- ggplot() + 
  geom_histogram(data= sim_10sd_df, aes(x = sims_10sd), bins = 150) + 
  geom_vline(aes(xintercept = 144.65, color = "True Value"), size = 1.5) + 
  geom_vline(aes(xintercept = as.numeric(bm_tenth_df[i,]), color = "Observations")) +
  geom_vline(aes(xintercept = bm_tenth$estimate[i], color = "Estimate"), size = 1.5) +
  scale_color_manual(name ="", values = c("limegreen", "yellow", "red")) +
  scale_y_continuous(limits = c(0,550), expand = c(0, 0)) +
  theme_classic() +
  labs(x = 'Day of Year', y = 'Number of Individuals') + 
  ggtitle(label = "10 Observations - 10 SD", subtitle = paste("Simulation Number", i)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
  

print(plot)
  
}

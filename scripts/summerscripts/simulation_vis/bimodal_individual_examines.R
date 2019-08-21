# Examine individual outputs of simulation

#10 SD
set.seed(10)
obs_10sd <- rnorm(20000, mean = 200, sd = 10)
to_10sd <- as.data.frame(obs_10sd)
set.seed(1)
rep_10obs_10sd <- replicate(n = 30, expr = sample(obs_10sd, size = 10, replace = FALSE))
list_10obs_10sd <- split(rep_10obs_10sd, rep(1:ncol(rep_10obs_10sd), each = nrow(rep_10obs_10sd)))


um_tenth <- read.csv("simulation_outputs/onset_try2/unimodal_tenth_sims2.csv", stringsAsFactors = FALSE, header = TRUE)
um_tenth_df <- as.data.frame(split(unlist(list_10obs_10sd),1:10))

for(i in 1:30){
  
  plot <- ggplot() + 
    geom_histogram(data= to_10sd, aes(x = obs_10sd), bins = 150) + 
    geom_vline(aes(xintercept = 186.99, color = "True Value"), size = 1.5) + 
    geom_vline(aes(xintercept = as.numeric(um_tenth_df[i,]), color = "Observations")) +
    geom_vline(aes(xintercept = um_tenth$estimate[i], color = "Estimate"), size = 1.5) +
    scale_color_manual(name ="", values = c("limegreen", "yellow", "red")) +
    scale_y_continuous(limits = c(0,550), expand = c(0, 0)) +
    theme_classic() +
    labs(x = 'Day of Year', y = 'Number of Individuals') + 
    ggtitle(label = "10 Observations - 10 SD", subtitle = paste("Simulation Number", i)) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
  
  
  print(plot)
  
}

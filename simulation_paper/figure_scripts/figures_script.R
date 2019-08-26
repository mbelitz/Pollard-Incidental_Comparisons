library(ggplot2)
library(grid)
library(gridExtra)

source("scripts/summerscripts/simulation_vis/bimodal_median_vis.R")
source("scripts/summerscripts/simulation_vis/bimodal_onset_vis.R")
source("scripts/summerscripts/simulation_vis/bimodal_offset_vis.R")
source("scripts/summerscripts/simulation_vis/bimodal_tenth_vis.R")
source("scripts/summerscripts/simulation_vis/bimodal_ninty_vis.R")
source("scripts/summerscripts/simulation_vis/unimodal_median_vis.R")
source("scripts/summerscripts/simulation_vis/unimodal_onset_vis.R")
source("scripts/summerscripts/simulation_vis/unimodal_offset_vis.R")
source("scripts/summerscripts/simulation_vis/unimodal_tenth_vis.R")
source("scripts/summerscripts/simulation_vis/unimodal_ninty_vis.R")

unimodal_pass_sum <- rbind(um_tenth_pass_sum, um_onset_pass_sum, um_median_pass_sum,
                           um_ninty_pass_sum, um_offset_pass_sum)
bimodal_pass_sum <- rbind(bm_onset_pass_sum, bm_tenth_pass_sum, bm_median_pass_sum, bm_ninty_pass_sum,
                          bm_offset_pass_sum)


## FIGURE 1

um_pass_sum_nopearse <- unimodal_pass_sum %>% 
  dplyr::filter(estimator != "pearse") %>% 
  dplyr::mutate(perc = factor(perc, levels = c("unimodal onset", "unimodal tenth",
                                               "unimodal median", "unimodal ninty", "unimodal offset")))


um_dis <- ggplot(um_pass_sum_nopearse, aes(x = uid, y = mean_dis)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "Number of Observations", y = "Number of Days from True Value") + 
  geom_errorbar(aes(ymin = mean_dis - sd_dis, ymax = mean_dis +sd_dis)) +
  geom_hline(yintercept = 0) + 
  ggtitle("Unimodal Distribution") + 
  theme_bw() +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#C77CFF")) +
  scale_x_discrete(labels = c("belitz 10 10" = "10", "belitz 10 20" = "20", "belitz 10 50" = "50",
                              "belitz 20 10" = "10", "belitz 20 20" = "20", "belitz 20 50" = "50",
                              "belitz 40 10" = "10", "belitz 40 20" = "20", "belitz 40 50" = "50")) +
  facet_wrap(~perc, scales = "free_y") +
  theme(plot.title = element_text(hjust = 0.5)) 

um_dis

bm_pass_sum_nopearse <- bimodal_pass_sum %>% 
  dplyr::filter(estimator != "pearse") %>% 
  dplyr::mutate(perc = factor(perc, levels = c("bimodal onset", "bimodal tenth",
                                               "bimodal median", "bimodal ninty", "bimodal offset")))


bm_dis <- ggplot(bm_pass_sum_nopearse, aes(x = uid, y = mean_dis)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "Number of Observations", y = "Number of Days from True Value") + 
  geom_errorbar(aes(ymin = mean_dis - sd_dis, ymax = mean_dis +sd_dis)) +
  geom_hline(yintercept = 0) + 
  ggtitle("Bimodal Distribution") + 
  theme_bw() +
  scale_x_discrete(labels = c("belitz 10 10" = "10", "belitz 10 20" = "20", "belitz 10 50" = "50",
                              "belitz 20 10" = "10", "belitz 20 20" = "20", "belitz 20 50" = "50",
                              "belitz 40 10" = "10", "belitz 40 20" = "20", "belitz 40 50" = "50")) +
  facet_wrap(~perc, scales = "free_y") +
  theme(plot.title = element_text(hjust = 0.5)) 

bm_dis

total_dis <- gridExtra::grid.arrange(um_dis, bm_dis, nrow = 1,  top=textGrob("Accuracy of Estimators",
                                     gp = gpar(fontsize = 20, font = 2)))

total_dis

write.csv()


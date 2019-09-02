library(ggplot2)
library(grid)
library(gridExtra)
library(viridis)
library(Metrics)

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

um_pass_sum <- unimodal_pass_sum %>% 
  dplyr::mutate(perc = factor(perc, levels = c("unimodal onset", "unimodal tenth",
                                               "unimodal median", "unimodal ninty", "unimodal offset"))) 

um_pass_sum_onnoff <- um_pass_sum %>% 
  dplyr::filter(perc == "unimodal onset" | perc == "unimodal offset")

bm_pass_sum <- bimodal_pass_sum %>% 
  dplyr::mutate(perc = factor(perc, levels = c("bimodal onset", "bimodal tenth",
                                               "bimodal median", "bimodal ninty", "bimodal offset")))

bm_pass_sum_onoff <- bm_pass_sum %>% 
  dplyr::filter(perc == "bimodal onset" | perc == "bimodal offset")

um_pass_sum_nopearse <- unimodal_pass_sum %>% 
  dplyr::filter(estimator != "pearse") %>% 
  dplyr::mutate(perc = factor(perc, levels = c("unimodal onset", "unimodal tenth",
                                               "unimodal median", "unimodal ninty", "unimodal offset")))

bm_pass_sum_nopearse <- bimodal_pass_sum %>% 
  dplyr::filter(estimator != "pearse") %>% 
  dplyr::mutate(perc = factor(perc, levels = c("bimodal onset", "bimodal tenth",
                                               "bimodal median", "bimodal ninty", "bimodal offset")))

um_pass_sum_pearse <- unimodal_pass_sum %>% 
  dplyr::filter(estimator == "pearse") %>% 
  dplyr::mutate(perc = factor(perc, levels = c("unimodal onset", "unimodal tenth",
                                               "unimodal median", "unimodal ninty", "unimodal offset")))

bm_pass_sum_pearse <- bimodal_pass_sum %>% 
  dplyr::filter(estimator == "pearse") %>% 
  dplyr::mutate(perc = factor(perc, levels = c("bimodal onset", "bimodal tenth",
                                               "bimodal median", "bimodal ninty", "bimodal offset")))

# Distribution set up Figures

nn <- 20000

#10 SD
set.seed(1)
sims_10sd <- c(rtruncnorm(nn * (1/3), a=0, b=365, mean=150, sd=10),
               rtruncnorm(nn * (2/3), a=0, b=365, mean=220, sd=10))
sim_10sd_df <- as.data.frame(sims_10sd)
ggplot(data= sim_10sd_df, aes(x = sims_10sd)) + geom_histogram(bins = 150)
quantile(sims_10sd, probs = c(0,0.1,0.5,0.9,1)) 

#min = 113.29, 10% - 144.65, 50% - 213.21, #90% - 230.27, 100% - 257.28

#bm_dist_10sd <- ggplot(data= sim_10sd_df, aes(x = sims_10sd)) + 
 # geom_histogram(bins = 150) +
  #geom_vline(aes(xintercept = 113.29, color = "0th"), size = 1.25) +
#  geom_vline(aes(xintercept = 144.65, color = "10th"), size = 1.25) +
#  geom_vline(aes(xintercept = 213.21, color = "50th"), size = 1.25) +
#  geom_vline(aes(xintercept = 230.27, color = "90th"), size = 1.25) +
#  geom_vline(aes(xintercept = 257.28, color = "100th"), size = 1.25) +
#  scale_color_manual(name = "Pheno-Metric", values =c(0th = "#440154FF",
  #                                                    10th = "#404788FF",
 #                                                     50th = "#1F968BFF",
   #                                                   90th = "#3CBB75FF",
    #                                                  100th = "#FDE725FF"))+
  # scale_y_continuous(expand=c(0,0))

bm_dist_10sd

# For now, lets just get the distribution
bm_dist_10sd <- ggplot(data= sim_10sd_df, aes(x = sims_10sd)) + 
  geom_histogram(bins = 150) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Day of Year", y = "Number of Individuals") +
  ggtitle("10 SD")

bm_dist_10sd


#20 SD
set.seed(1)
sims_20sd <- c(rtruncnorm(nn * (1/3), a=0, b=365, mean=150, sd=20),
               rtruncnorm(nn * (2/3), a=0, b=365, mean=220, sd=20))
sim_20sd_df <- as.data.frame(sims_20sd)
bm_dist_20sd <- ggplot(data= sim_20sd_df, aes(x = sims_20sd)) + 
  geom_histogram(bins = 150) + 
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Day of Year", y = "Number of Individuals") +
  ggtitle("10 SD")
quantile(sims_20sd, probs = c(0,0.1,0.5,0.9,1)) 
#min = 76.58, 10% - 139.29, 50% - 206.48, #90% - 240.55, 100% - 294.56

bm_dists <- grid.arrange(bm_dist_10sd, bm_dist_20sd, nrow = 2)

ggsave(filename = "simulation_paper/figure_outputs/bm_dists.png", plot = bm_dists,
       width = 4, height = 6, dpi = 300)

## UM Distribution Figures

set.seed(10)
obs_10sd <- rnorm(20000, mean = 200, sd = 10)
to_10sd <- as.data.frame(obs_10sd)

set.seed(365)
obs_20sd <- rnorm(20000, mean = 200, sd = 20)
to_20sd <- as.data.frame(obs_20sd)

set.seed(40)
obs_40sd <- rnorm(20000, mean = 200, sd = 40)
to_40sd <- as.data.frame(obs_40sd)

tensd <- ggplot(data= to_10sd, aes(x = obs_10sd)) + 
  geom_histogram(bins = 150) +
  labs(x = "Day of Year", y = "Number of Individuals") +
  ggtitle("10 SD") +
  scale_y_continuous(expand = c(0,0))
twentysd <- ggplot(data= to_20sd, aes(x = obs_20sd)) + 
  geom_histogram(bins = 150) +
  labs(x = "Day of Year", y = "Number of Individuals") +
  ggtitle("20 SD") +
  scale_y_continuous(expand = c(0,0))
fortysd <- ggplot(data= to_40sd, aes(x = obs_40sd)) + 
  geom_histogram(bins = 150) +
  labs(x = "Day of Year", y = "Number of Individuals") +
  ggtitle("40 SD") +
  scale_y_continuous(expand = c(0,0))

um_dists <- grid.arrange(tensd, twentysd, fortysd, nrow = 2)

ggsave(filename = "simulation_paper/figure_outputs/um_dists.png", plot = um_dists,
       width = 6, height = 6, dpi = 300)

## FIGURE 1

um_dis <- ggplot(um_pass_sum_nopearse, aes(x = uid, y = mean_dis)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "Number of Observations", y = "Number of Days from True Value") + 
  geom_errorbar(aes(ymin = mean_dis - sd_dis, ymax = mean_dis +sd_dis)) +
  geom_hline(yintercept = 0) + 
  ggtitle("Unimodal Distribution") + 
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF", "#73D055FF")) +
  scale_x_discrete(labels = c("belitz 10 10" = "10", "belitz 10 20" = "20", "belitz 10 50" = "50",
                              "belitz 20 10" = "10", "belitz 20 20" = "20", "belitz 20 50" = "50",
                              "belitz 40 10" = "10", "belitz 40 20" = "20", "belitz 40 50" = "50")) +
  facet_wrap(~perc, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 

um_dis

bm_dis <- ggplot(bm_pass_sum_nopearse, aes(x = uid, y = mean_dis)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "Number of Observations", y = "Number of Days from True Value") + 
  geom_errorbar(aes(ymin = mean_dis - sd_dis, ymax = mean_dis +sd_dis)) +
  geom_hline(yintercept = 0) + 
  scale_fill_manual(values = c("#440154FF", "#287D8EFF")) +
  ggtitle("Bimodal Distribution") + 
  theme_bw() +
  scale_x_discrete(labels = c("belitz 10 10" = "10", "belitz 10 20" = "20", "belitz 10 50" = "50",
                              "belitz 20 10" = "10", "belitz 20 20" = "20", "belitz 20 50" = "50",
                              "belitz 40 10" = "10", "belitz 40 20" = "20", "belitz 40 50" = "50")) +
  facet_wrap(~perc, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 

bm_dis

total_dis <- gridExtra::grid.arrange(um_dis, bm_dis, nrow = 1,  top=textGrob("Accuracy of Estimators",
                                     gp = gpar(fontsize = 20, font = 2)))

total_dis

ggsave(filename = "simulation_paper/figure_outputs/Fig_1.png", plot = total_dis,
      width = 12, height = 4, dpi = 300)

### FIGURE 2

um_corr <- ggplot(um_pass_sum_nopearse, aes(x = uid, y = abs(percent_right))) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "Number of Observations", y = "Percent of Estimates") + 
  ggtitle("Unimodal Distribution") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF", "#73D055FF")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() +
  scale_x_discrete(labels = c("belitz 10 10" = "10", "belitz 10 20" = "20", "belitz 10 50" = "50",
                              "belitz 20 10" = "10", "belitz 20 20" = "20", "belitz 20 50" = "50",
                              "belitz 40 10" = "10", "belitz 40 20" = "20", "belitz 40 50" = "50")) +
  facet_wrap(~perc) +
  theme(plot.title = element_text(hjust = 0.5)) 

bm_corr <- ggplot(bm_pass_sum_nopearse, aes(x = uid, y = abs(percent_right))) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "Number of Observations", y = "Percent of Estimates") + 
  ggtitle("Bimodal Distribution") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() +
  scale_x_discrete(labels = c("belitz 10 10" = "10", "belitz 10 20" = "20", "belitz 10 50" = "50",
                              "belitz 20 10" = "10", "belitz 20 20" = "20", "belitz 20 50" = "50",
                              "belitz 40 10" = "10", "belitz 40 20" = "20", "belitz 40 50" = "50")) +
  facet_wrap(~perc) +
  theme(plot.title = element_text(hjust = 0.5)) 

total_corr <- gridExtra::grid.arrange(um_corr, bm_corr, nrow = 1,  top=textGrob("How often do the CIs Encompass the True Value",
                                                                gp = gpar(fontsize = 20, font = 2)))

ggsave(filename = "simulation_paper/figure_outputs/Fig_2.png", plot = total_corr,
       width = 12, height = 4, dpi = 300)


## Figure 3

um_cis <- ggplot(um_pass_sum_nopearse) +
  geom_crossbar(aes(x = uid, y = 0, ymin = mean_low_ci, ymax = mean_high_ci, fill = sd)) +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF", "#73D055FF")) +
  labs(x = "Number of Observations", y = "Length of Confidence Intervals") + 
  ggtitle("Unimodal Distribution") + 
  theme_bw() +
  scale_x_discrete(labels = c("belitz 10 10" = "10", "belitz 10 20" = "20", "belitz 10 50" = "50",
                              "belitz 20 10" = "10", "belitz 20 20" = "20", "belitz 20 50" = "50",
                              "belitz 40 10" = "10", "belitz 40 20" = "20", "belitz 40 50" = "50")) +
  facet_wrap(~perc) +
  theme(plot.title = element_text(hjust = 0.5)) 


bm_cis <- ggplot(bm_pass_sum_nopearse) +
  geom_crossbar(aes(x = uid, y = 0, ymin = mean_low_ci, ymax = mean_high_ci, fill = sd)) +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF")) +
  labs(x = "Number of Observations", y = "Length of Confidence Intervals") + 
  ggtitle("Bimodal Distribution") + 
  theme_bw() +
  scale_x_discrete(labels = c("belitz 10 10" = "10", "belitz 10 20" = "20", "belitz 10 50" = "50",
                              "belitz 20 10" = "10", "belitz 20 20" = "20", "belitz 20 50" = "50")) +
  facet_wrap(~perc) +
  theme(plot.title = element_text(hjust = 0.5)) 

total_cis <- gridExtra::grid.arrange(um_cis, bm_cis, nrow = 1)

ggsave(filename = "simulation_paper/figure_outputs/Fig_3.png", plot = total_cis,
       width = 12, height = 4, dpi = 300)


## FIGURE RMSE

um_rmse <- ggplot(um_pass_sum_nopearse, aes(x = uid, y = RMSE)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "Number of Observations", y = "RMSE") + 
  geom_hline(yintercept = 0) + 
  ggtitle("Unimodal Distribution") + 
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF", "#73D055FF")) +
  scale_x_discrete(labels = c("belitz 10 10" = "10", "belitz 10 20" = "20", "belitz 10 50" = "50",
                              "belitz 20 10" = "10", "belitz 20 20" = "20", "belitz 20 50" = "50",
                              "belitz 40 10" = "10", "belitz 40 20" = "20", "belitz 40 50" = "50")) +
  facet_wrap(~perc, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 

um_rmse

bm_rmse <- ggplot(bm_pass_sum_nopearse, aes(x = uid, y = RMSE)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "Number of Observations", y = "RMSE") + 
  geom_hline(yintercept = 0) + 
  scale_fill_manual(values = c("#440154FF", "#287D8EFF")) +
  ggtitle("Bimodal Distribution") + 
  theme_bw() +
  scale_x_discrete(labels = c("belitz 10 10" = "10", "belitz 10 20" = "20", "belitz 10 50" = "50",
                              "belitz 20 10" = "10", "belitz 20 20" = "20", "belitz 20 50" = "50",
                              "belitz 40 10" = "10", "belitz 40 20" = "20", "belitz 40 50" = "50")) +
  facet_wrap(~perc, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 

bm_rmse

total_rmse <- gridExtra::grid.arrange(um_rmse, bm_rmse, nrow = 1)
total_rmse

ggsave(filename = "simulation_paper/figure_outputs/RMSE.png", plot = total_rmse,
       width = 12, height = 4, dpi = 300)


# RMSE pearse vs belitz 

pb_unimodal_rmse <- ggplot(um_pass_sum_onnoff, aes(x = uid, y = RMSE)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "Estimator - Number of Observations", y = "RMSE") + 
  geom_hline(yintercept = 0) + 
  ggtitle("Unimodal Distribution") + 
  theme_bw() +
  scale_fill_manual(values = c("#440154FF", "#287D8EFF", "#73D055FF")) +
  scale_x_discrete(labels = c("belitz 10 10" = "Belitz 10", "belitz 10 20" = "Belitz 20", "belitz 10 50" = "Belitz 50",
                              "belitz 20 10" = "Belitz 10", "belitz 20 20" = "Belitz 20", "belitz 20 50" = "Belitz 50",
                              "belitz 40 10" = "Belitz 10", "belitz 40 20" = "Belitz 20", "belitz 40 50" = "Belitz 50",
                              "pearse 10 10" = "Pearse 10", "pearse 10 20" = "Pearse 20", "pearse 10 50" = "Pearse 50",
                              "pearse 20 10" = "Pearse 10", "pearse 20 20" = "Pearse 20", "pearse 20 50" = "Pearse 50",
                              "pearse 40 10" = "Pearse 10", "pearse 40 20" = "Pearse 20", "pearse 40 50" = "Pearse 50")) +
  facet_wrap(~perc, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 

pb_unimodal_rmse

pb_bimodal_rmse <- ggplot(bm_pass_sum, aes(x = uid, y = RMSE)) +
  geom_bar(stat = "identity", aes(fill = sd)) +
  labs(x = "Estimator - Number of Observations", y = "RMSE") + 
  geom_hline(yintercept = 0) + 
  scale_fill_manual(values = c("#440154FF", "#287D8EFF")) +
  ggtitle("Bimodal Distribution") + 
  theme_bw() +
  scale_x_discrete(labels = c("belitz 10 10" = "Belitz 10", "belitz 10 20" = "Belitz 20", "belitz 10 50" = "Belitz 50",
                              "belitz 20 10" = "Belitz 10", "belitz 20 20" = "Belitz 20", "belitz 20 50" = "Belitz 50",
                              "belitz 40 10" = "Belitz 10", "belitz 40 20" = "Belitz 20", "belitz 40 50" = "Belitz 50",
                              "pearse 10 10" = "Pearse 10", "pearse 10 20" = "Pearse 20", "pearse 10 50" = "Pearse 50",
                              "pearse 20 10" = "Pearse 10", "pearse 20 20" = "Pearse 20", "pearse 20 50" = "Pearse 50",
                              "pearse 40 10" = "Pearse 10", "pearse 40 20" = "Pearse 20", "pearse 40 50" = "Pearse 50")) +
  facet_wrap(~perc, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 

pb_bimodal_rmse

pearse_belitz <- gridExtra::grid.arrange(pb_unimodal_rmse,pb_bimodal_rmse, nrow = 2)
pearse_belitz

ggsave(filename = "simulation_paper/figure_outputs/pearse_belitz_compare.png", plot = pearse_belitz,
       width = 9, height = 6, dpi = 300)

# Try line graphs
um_pass_line <- um_pass_sum %>% 
  dplyr::mutate(Q = ifelse(perc == "unimodal onset",0,
                           ifelse(perc == "unimodal tenth", 10, 
                                  ifelse(perc == "unimodal median", 50, 
                                         ifelse(perc == "unimodal ninty", 90,
                                                ifelse(perc == "unimodal offset", 100, NA))))))


bm_pass_line <- bm_pass_sum %>% 
  dplyr::mutate(Q = ifelse(perc == "bimodal onset",0,
                           ifelse(perc == "bimodal tenth", 10, 
                                  ifelse(perc == "bimodal median", 50, 
                                         ifelse(perc == "bimodal ninty", 90,
                                                ifelse(perc == "bimodal offset", 100, NA))))))
um_pass_line_nopearse <- um_pass_line %>% 
  dplyr::filter(estimator != "pearse") %>% 
  dplyr::mutate(modality = "unimodal")

bm_pass_line_nopearse <- bm_pass_line %>% 
  dplyr::filter(estimator != 'pearse') %>% 
  dplyr::mutate(modality = "bimodal")

total_pass_line_nopearse <- rbind(um_pass_line_nopearse, bm_pass_line_nopearse)

ggplot(total_pass_line_nopearse, aes(x = Q, y = RMSE)) +
  geom_point(aes(color = sd)) +
  geom_line(aes(color = sd)) +
  scale_x_continuous(breaks = c(0,10,50,90,100)) +
  facet_wrap(modality~obs)


line_RMSE <- total_pass_line_nopearse %>%
  ungroup() %>% 
  mutate(
    modality = factor(modality, levels = c("unimodal", "bimodal")),
    sd = factor(sd, levels = c(10, 20, 40), labels = c("SD = 10", "SD = 20", "SD = 40") )
    ) %>% 
  ggplot(aes(x = Q, y = RMSE, color = obs)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(0,10,50,90,100)) +
  facet_grid(sd~modality, scales = "free_y") +
  theme(
    legend.position = "bottom"
  ) +
  labs(x = 'Percentile') +
  scale_color_manual(values = c("#440154FF", "#287D8EFF", "#73D055FF"))

ggsave(filename = "simulation_paper/figure_outputs/line_RMSE.png", plot = line_RMSE,
       width = 10, height = 8, dpi = 300)

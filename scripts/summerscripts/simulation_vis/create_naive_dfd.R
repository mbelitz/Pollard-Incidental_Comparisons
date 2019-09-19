library(matrixStats)

sims <- read.csv("simulation_outputs/lists_ninty_median_fixed.csv")
sims2 <- dplyr::select(sims, -uid)
sim_mat <- as.matrix(sims2)

naive <- rowQuantiles(sim_mat, probs = c(0,0.1,0.5,0.9,1), na.rm = TRUE)

naive_df <- as.data.frame(naive)

row_names <- dplyr::select(sims, uid)

naive_df <- cbind(naive_df, row_names)
naive_df$uid <- as.character(naive_df$uid)

write.csv(x = naive_df, file = "simulation_outputs/naive_estimates.csv", row.names = FALSE)

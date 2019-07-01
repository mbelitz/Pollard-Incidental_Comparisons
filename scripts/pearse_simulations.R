library(phest)
library(ggplot2)

source("C:/Users/Mike/Documents/UF1/statistical_principles/Percentile-Bound-Estimator/R_Scripts/percentile_bound_estimator.R")
source("C:/Users/Mike/Documents/UF1/statistical_principles/Percentile-Bound-Estimator/R_Scripts/percentile_bound_ci.R")

set.seed(10)
obs_10sd <- rnorm(20000, mean = 200, sd = 10)
to_10sd <- as.data.frame(obs_10sd)
ggplot(data= to_10sd, aes(x = obs_10sd)) + geom_histogram(bins = 150)
min(to_10sd) ## 162.6818
max(to_10sd) # 238.1258

set.seed(365)
obs_20sd <- rnorm(20000, mean = 200, sd = 20)
to_20sd <- as.data.frame(obs_20sd)
ggplot(data= to_20sd, aes(x = obs_20sd)) + geom_histogram(bins = 150)
min(to_20sd) # 119.9182
max(to_20sd) # 279.3462


set.seed(40)
obs_40sd <- rnorm(20000, mean = 200, sd = 40)
to_40sd <- as.data.frame(obs_40sd)
ggplot(data= to_40sd, aes(x = obs_40sd)) + geom_histogram(bins = 150)
min(to_40sd) # 35.88112
max(to_40sd) #361.3789

# lapply function

onsetestimator <- function(x){
  
  onset <- phest::weib.limit(x, upper = FALSE )
  return(onset)
}

# 10 obs - 10 sd ONSET

subsam_rep <- replicate(n = 30, expr = sample(total_observations, size = 10, replace = FALSE))
subsam_list <- split(subsam_rep, rep(1:ncol(subsam_rep), each = nrow(subsam_rep)))

unlist(lapply(subsam_list, FUN = onsetestimator))

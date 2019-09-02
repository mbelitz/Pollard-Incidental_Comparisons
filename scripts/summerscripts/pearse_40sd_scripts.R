library(ggplot2)
library(parallel)
library(phest)
library(dplyr)

nn <- 20000

#10 SD
set.seed(10)
obs_10sd <- rnorm(20000, mean = 200, sd = 10)
to_10sd <- as.data.frame(obs_10sd)
ggplot(data= to_10sd, aes(x = obs_10sd)) + geom_histogram(bins = 150)
min(to_10sd) ## 162.6818
max(to_10sd) # 199.89258
quantile(obs_10sd, probs = c(0,0.1,0.5,0.9,1)) #10% -186.99 50% - 199.89

set.seed(1)
rep_10obs_10sd <- replicate(n = 30, expr = sample(obs_10sd, size = 10, replace = FALSE))
list_10obs_10sd <- split(rep_10obs_10sd, rep(1:ncol(rep_10obs_10sd), each = nrow(rep_10obs_10sd)))

set.seed(2)
rep_20obs_10sd <- replicate(n = 30, expr = sample(obs_10sd, size = 20, replace = FALSE))
list_20obs_10sd <- split(rep_20obs_10sd, rep(1:ncol(rep_20obs_10sd), each = nrow(rep_20obs_10sd)))

set.seed(5)
rep_50obs_10sd <- replicate(n = 30, expr = sample(obs_10sd, size = 50, replace = FALSE))
list_50obs_10sd <- split(rep_50obs_10sd, rep(1:ncol(rep_50obs_10sd), each = nrow(rep_50obs_10sd)))


#20 SD
set.seed(365)
obs_20sd <- rnorm(20000, mean = 200, sd = 20)
to_20sd <- as.data.frame(obs_20sd)
ggplot(data= to_20sd, aes(x = obs_20sd)) + geom_histogram(bins = 150)
min(to_20sd) # 119.9182
max(to_20sd) # 279.3462
quantile(obs_20sd, probs = c(0,0.1,0.5,0.9,1)) #10% -174.82 50% - 200.18

set.seed(1)
rep_10obs_20sd <- replicate(n = 30, expr = sample(obs_20sd, size = 10, replace = FALSE))
list_10obs_20sd <- split(rep_10obs_20sd, rep(1:ncol(rep_10obs_20sd), each = nrow(rep_10obs_20sd)))

set.seed(2)
rep_20obs_20sd <- replicate(n = 30, expr = sample(obs_20sd, size = 20, replace = FALSE))
list_20obs_20sd <- split(rep_20obs_20sd, rep(1:ncol(rep_20obs_20sd), each = nrow(rep_20obs_20sd)))

set.seed(5)
rep_50obs_20sd <- replicate(n = 30, expr = sample(obs_20sd, size = 50, replace = FALSE))
list_50obs_20sd <- split(rep_50obs_20sd, rep(1:ncol(rep_50obs_20sd), each = nrow(rep_50obs_20sd)))


#40 SD
set.seed(40)
obs_40sd <- rnorm(20000, mean = 200, sd = 40)
to_40sd <- as.data.frame(obs_40sd)
ggplot(data= to_40sd, aes(x = obs_40sd)) + geom_histogram(bins = 150)
min(to_40sd) # 35.88112
max(to_40sd) #361.3789
quantile(obs_40sd, probs = c(0,0.1,0.5,0.9,1)) #10% -149.30459 50% - 199.80307

set.seed(1)
rep_10obs_40sd <- replicate(n = 30, expr = sample(obs_40sd, size = 10, replace = FALSE))
list_10obs_40sd <- split(rep_10obs_40sd, rep(1:ncol(rep_10obs_40sd), each = nrow(rep_10obs_40sd)))

set.seed(2)
rep_20obs_40sd <- replicate(n = 30, expr = sample(obs_40sd, size = 20, replace = FALSE))
list_20obs_40sd <- split(rep_20obs_40sd, rep(1:ncol(rep_20obs_40sd), each = nrow(rep_20obs_40sd)))

set.seed(5)
rep_50obs_40sd <- replicate(n = 30, expr = sample(obs_40sd, size = 50, replace = FALSE))
list_50obs_40sd <- split(rep_50obs_40sd, rep(1:ncol(rep_50obs_40sd), each = nrow(rep_50obs_40sd)))



# lapply functions 

pearse_onsetestimator <- function(x){
  
  pearseonset <- weib.limit(x = x,upper = FALSE)
  return(pearseonset)
}

# lapply functions 

pearse_offsetestimator <- function(x){
  
  pearseoffset <- weib.limit(x = x,upper = TRUE)
  return(pearseoffset)
}


# 20 obs 40 sd
pearse_onset20obs_40sd <- unlist(lapply(list_20obs_40sd, FUN = pearse_onsetestimator))
pearse_onset20obs_40sd_df <- as.data.frame(split(pearse_onset20obs_40sd, 1:3))

# 50 obs 40 sd
pearse_onset50obs_40sd <- unlist(lapply(list_50obs_40sd, FUN = pearse_onsetestimator))
pearse_onset50obs_40sd_df <- as.data.frame(split(pearse_onset50obs_40sd, 1:3))

pearse_onset50obs_40sd_df <- pearse_onset50obs_40sd_df %>% 
  rename(estimate = X1, lowci = X2, highci = X3) %>% 
  mutate(true_onset = 35.88) %>% 
  mutate(pass = true_onset > lowci & true_onset < highci) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "pearse") %>% 
  mutate(ci = highci - lowci)

pearse_onset20obs_40sd_df <- pearse_onset20obs_40sd_df %>% 
  rename(estimate = X1, lowci = X2, highci = X3) %>% 
  mutate(true_onset = 35.88) %>% 
  mutate(pass = true_onset > lowci & true_onset < highci) %>% 
  mutate(distance = estimate - true_onset) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "pearse") %>% 
  mutate(ci = highci - lowci)


## offset fixes

pearse_offset20obs_40sd <- unlist(lapply(list_20obs_40sd, FUN = pearse_offsetestimator))
pearse_offset20obs_40sd_df <- as.data.frame(split(pearse_offset20obs_40sd, 1:3))

# 50 obs 40 sd
pearse_offset50obs_40sd <- unlist(lapply(list_50obs_40sd, FUN = pearse_offsetestimator))
pearse_offset50obs_40sd_df <- as.data.frame(split(pearse_offset50obs_40sd, 1:3))

pearse_offset50obs_40sd_df <- pearse_offset50obs_40sd_df %>% 
  rename(estimate = X1, lowci = X2, highci = X3) %>% 
  mutate(true_offset = 361.38) %>% 
  mutate(pass = true_offset > lowci & true_offset < highci) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 50, sd = 40) %>% 
  mutate(estimator = "pearse") %>% 
  mutate(ci = highci - lowci)

pearse_offset20obs_40sd_df <- pearse_offset20obs_40sd_df %>% 
  rename(estimate = X1, lowci = X2, highci = X3) %>% 
  mutate(true_offset = 361.38) %>% 
  mutate(pass = true_offset > lowci & true_offset < highci) %>% 
  mutate(distance = estimate - true_offset) %>% 
  mutate(obs = 20, sd = 40) %>% 
  mutate(estimator = "pearse") %>% 
  mutate(ci = highci - lowci)


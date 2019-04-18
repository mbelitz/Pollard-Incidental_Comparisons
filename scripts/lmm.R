library(MuMIn)
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)

#read in phenology estimator data

pheno_est <- read.csv(file = "data/gam+weibull_2014-18_t2.csv")

# visualize all estimators of all butterflies

ggplot(pheno_est, aes(x = lat, y = gam.em.10)) + 
  geom_point(aes(color = species)) 

ggplot(pheno_est, aes(x = lat, y = weibull.onset)) + 
  geom_point(aes(color = species)) 

pt_onset <- pheno_est %>% 
  filter(species == "Phyciodes tharos") %>% 
  select(cell, species, survey.year, weibull.onset, gam.em.10, lon, lat)

pt_onset_2017 <- pt_onset %>% 
  filter(survey.year == 2017)

ggplot(pt_onset, aes(x = lat, y = weibull.onset)) + 
  geom_point() +
  geom_smooth()

ggplot(pt_onset, aes(x = lat, y = gam.em.10)) + 
  geom_point() +
  geom_smooth()

# LMM for Phyciodes tharos

pt_lmm_weib <- lmer(weibull.onset ~ lat + factor(survey.year) + (1|cell), data = pt_onset)
summary(pt_lmm_weib)

pt_lmm_2017_weib <- lm(weibull.onset ~ lat  , data = pt_onset_2017)
summary(pt_lmm_2017_weib)

pt_lmm_gam <- lm(gam.em.10 ~ lat + factor(survey.year) , data = pt_onset)
summary(pt_lmm_gam)

pt_lmm_gam <- lm(gam.em.10 ~ lat + factor(survey.year) + (1|cell) , data = pt_onset)
summary(pt_lmm_gam)

pt_lmm_gam_2018 <- lmer (gam.em.10 ~ lat, data = pt_onset_2018)
summary(pt_lmm_gam_2018)


# LMM for Epargyreus clarus
ec_onset <- pheno_est %>% 
  filter(species == "Epargyreus clarus") %>% 
  select(cell, species, survey.year, weibull.onset, gam.em.10, lon, lat)

ec_lmm_weib <- lmer(weibull.onset ~ lat + factor(survey.year) + (1|cell), data = ec_onset)
summary(ec_lmm_weib)

ec_lmm_gam <- lmer(gam.em.10 ~ lat + factor(survey.year) + (1|cell) , data = ec_onset)
summary(ec_lmm_gam)

ggplot(ec_onset, aes(x = lat, y = weibull.onset)) + 
  geom_point() +
  geom_smooth()

ggplot(ec_onset, aes(x = lat, y = gam.em.10)) + 
  geom_point() +
  geom_smooth()

# LMM for Everes comyntas
evco_onset <- pheno_est %>% 
  filter(species == "Everes comyntas") %>% 
  select(cell, species, survey.year, weibull.onset, gam.em.10, lon, lat)

evco_lmm_weib <- lmer(weibull.onset ~ lat + factor(survey.year) + (1|cell), data = evco_onset)
summary(evco_lmm_weib)

evco_lmm_weib <- lm(weibull.onset ~ lat + factor(survey.year), data = evco_onset)
summary(evco_lmm_weib)


evco_lmm_gam <- lmer(gam.em.10 ~ lat + factor(survey.year) + (1|cell) , data = evco_onset)
summary(ec_lmm_gam)

ggplot(evco_onset, aes(x = lat, y = gam.em.10)) + 
  geom_point() +
  geom_smooth()

ggplot(evco_onset, aes(x = lat, y = weibull.onset)) + 
  geom_point() +
  geom_smooth()


# LMM for Pieris rapae
pr_onset <- pheno_est %>% 
  filter(species == "Pieris rapae") %>% 
  select(cell, species, survey.year, weibull.onset, gam.em.10, lon, lat)

pira_lmm_weib <- lmer(weibull.onset ~ lat + factor(survey.year) + (1|cell), data = pr_onset)
summary(pira_lmm_weib)

pira_lmm_gam <- lmer(gam.em.10 ~ lat + factor(survey.year) + (1|cell) , data = pr_onset)
summary(pira_lmm_gam)

ggplot(pr_onset, aes(x = lat, y = gam.em.10)) + 
  geom_point() +
  geom_smooth()

ggplot(pr_onset, aes(x = lat, y = weibull.onset)) + 
  geom_point() +
  geom_smooth()
 
# Colias eurytheme

coeu_onset <- pheno_est %>% 
  filter(species == "Colias eurytheme") %>% 
  select(cell, species, survey.year, weibull.onset, gam.em.10, lon, lat)

coeu_lmm_weib <- lmer(weibull.onset ~ lat + factor(survey.year) + (1|cell), data = coeu_onset)
summary(coeu_lmm_weib)

coeu_lmm_gam <- lm(gam.em.10 ~ lat + factor(survey.year) , data = coeu_onset)
summary(coeu_lmm_gam)

ggplot(coeu_onset, aes(x = lat, y = gam.em.10)) + 
  geom_point() +
  geom_smooth()

ggplot(coeu_onset, aes(x = lat, y = weibull.onset)) + 
  geom_point() +
  geom_smooth()

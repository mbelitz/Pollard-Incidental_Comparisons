library(phest)
library(ggplot2)
library(dplyr)

# Read in grace's inat data

inat <- read.csv(file = "data/pheno_mismatch/inat_adults_cats_ENA.csv", header = TRUE, stringsAsFactors = FALSE)


## Visualize obs for diff species

h_tessellaris <- plot_obs("Halysidota tessellaris", year.of.interest = 2018)
h_tessellaris
h_tess_dif <- examine_differences("Halysidota tessellaris")

Orgyia_leucostigma <- plot_obs("Orgyia leucostigma", year.of.interest = 2018)
Orgyia_leucostigma
o_luec_diff <- examine_differences("Orgyia leucostigma")

Lophocampa_caryae <- plot_obs("Lophocampa caryae", year.of.interest = 2018)
Lophocampa_caryae
l_cary_diff <- examine_differences("Lophocampa caryae")

Euchaetes_egle <- plot_obs("Euchaetes egle", year.of.interest = 2018)
Euchaetes_egle
e_egle_diff <- examine_differences("Euchaetes egle")

Hyphantria_cunea <- plot_obs("Hyphantria cunea", year.of.interest = 2018)
Hyphantria_cunea
h_cunea_diff <- examine_differences("Hyphantria cunea")

Acronicta_americana <- plot_obs("Acronicta americana", year.of.interest = 2018)
Acronicta_americana
a_amer_diff <- examine_differences("Acronicta americana")

Estigmene_acrea <- plot_obs("Estigmene acrea", year.of.interest = 2018)
Estigmene_acrea
e_acrea_diff <- examine_differences("Estigmene acrea")

Lymantria_dispar <- plot_obs("Lymantria dispar", year.of.interest = 2018)
Lymantria_dispar
l_dispar_diff <- examine_differences("Lymantria dispar")

Hypercompe_scribonia <- plot_obs("Hypercompe scribonia", year.of.interest = 2018)
Hypercompe_scribonia
h_scrib_diff <- examine_differences("Hypercompe scribonia")

combined_df <- rbind(h_scrib_diff, l_dispar_diff, e_acrea_diff, a_amer_diff, h_cunea_diff,
                     e_egle_diff, l_cary_diff, o_luec_diff, h_tess_dif)

ggplot(combined_df) + 
  geom_point(aes(x = year_count.y, y = diff_onset)) + 
  geom_smooth(aes(x = year_count.y, y = diff_onset), method = "lm")

#broken......
Pyrrharctia_isabella <- plot_obs("Pyrrhactia isabella", year.of.interest = 2018)
Pyrrharctia_isabella

Spilosoma_virginica <- plot_obs("Spilosoma_virginica", year.of.interest = 2018)
Spilosoma_virginica
library(readr)
#library(lava)
library(missMethods)
library(simputation)
library(ggplot2)
set.seed(1) 


penguins <- readr::read_csv('penguins.csv')

summary(penguins)

penguins_missing <- delete_MCAR(penguins,0.5,"flipper_length_mm")

ggplot(penguins, aes(body_mass_g, flipper_length_mm)) + 
  geom_count() + 
  geom_smooth(method='lm')

regg = lm(body_mass_g~ flipper_length_mm, data = penguins, na.action=na.omit) 
summary(regg)

penguins_imputed <- impute_lm(penguins_missing, flipper_length_mm ~ species + bill_length_mm + bill_depth_mm)

regg = lm(body_mass_g~ flipper_length_mm, data = penguins_imputed)
summary(regg)

regg = lm(body_mass_g~ flipper_length_mm, data = penguins_missing, na.action=na.omit) 
summary(regg)


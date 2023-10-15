library(readr)
library(lava)
library(missMethods)
library(simputation)

library(ggplot2)

penguins <- readr::read_csv('penguins.csv')

summary(penguins)

penguins_missing <- delete_MCAR(penguins,0.2,"flipper_length_mm")

ggplot(penguins, aes(body_mass_g, flipper_length_mm)) + 
  geom_count() + 
  geom_smooth(method='lm')

regg = lm(body_mass_g~ flipper_length_mm, data = penguins) 
summary(regg)

penguins_imputed <- impute_median(penguins_missing, flipper_length_mm ~ species + bill_length_mm + bill_depth_mm)

regg = lm(body_mass_g~ flipper_length_mm, data = penguins_imputed)
summary(regg)

regg = lm(body_mass_g~ flipper_length_mm, data = penguins_missing) 
summary(regg)


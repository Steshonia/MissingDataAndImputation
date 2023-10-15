library(readr)
library(lava)
library(simputation)

library(ggplot2)

cats_uk <- readr::read_csv('cats_uk.csv')
cats_uk_reference <- readr::read_csv('cats_uk_reference.csv')

cats <- merge(cats_uk, cats_uk_reference, by='tag_id')
summary(cats_uk)

cats_uk_missing <- makemissing(cats_uk)


ggplot(cats_uk_reference, aes(hrs_indoors, prey_p_month, colour=animal_sex)) + 
  geom_count() + 
  geom_smooth(method='lm')

regg = lm(prey_p_month~hrs_indoors + n_cats + age_years, data = cats_uk_reference) #Create the linear regression
summary(regg) #Review the results

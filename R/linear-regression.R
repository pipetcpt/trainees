# iris ----

library(ggplot2)
ggplot(lm(Sepal.Length~Sepal.Width, data=iris)) + 
  geom_point(aes(x=.fitted, y=.resid))

fit <- lm(Sepal.Length~Sepal.Width, data=iris)
par(mfrow=c(2,2))
plot(fit)

library(tidyverse)
PKPDdatasets::aht_trial1 %>% as_tibble()

PKPDdatasets::list_datasets()

library(PKPDdatasets)
devtools::install_github()




PKPDdatasets::sd_oral_richpk %>% 
  as_tibble() %>% 
  ggplot(aes(Time, Conc, group = ID, color = Dose)) +
  geom_line() +
  geom_point() +
  facet_grid(rows = vars(Gender), cols = vars(Race))
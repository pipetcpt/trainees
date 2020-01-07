library(pkr)
library(NonCompart)
library(ggplot2)
library(readxl)
library(dplyr)
library(ncar)
PK_data <- read_excel("Drug_X_PK.xlsx") %>% as.data.frame() %>% mutate(ID = as.factor(ID))

ggplot(PK_data, aes(x = TIME, y = DV, group = ID, color = ID)) + 
  geom_line() + 
  labs(x = "Time", y = "concentration", title = "PK")

ggplot(PK_data, aes(x = TIME, y = DV)) + 
  geom_line() + 
  facet_wrap(.~ ID) +
  labs(x = "Time", y = "concentration", title = "PK_ID")

ggplot(PK_data, aes(x = TIME, y = DV)) + 
  geom_line() + 
  facet_wrap(.~ GENE) +
  labs(x = "Time", y = "concentration", title = "PK_ID")

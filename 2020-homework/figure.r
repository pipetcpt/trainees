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

PK_GENE_1 <- PK_data %>% filter(GENE == 1)
PK_GENE_2 <- PK_data %>% filter(GENE == 2)
PK_GENE_3 <- PK_data %>% filter(GENE == 3)

ggplot(PK_GENE_1, aes(x = TIME, y = DV, group = ID, color = ID)) +
  geom_line() +
  labs(x = "Time", y = "concentration", title = "PK_GENE1")

ggplot(PK_GENE_2, aes(x = TIME, y = DV, group = ID, color = ID)) +
  geom_line() +
  labs(x = "Time", y = "concentration", title = "PK_GENE2")
ggplot(PK_GENE_3, aes(x = TIME, y = DV, group = ID, color = ID)) +
  geom_line() +
  labs(x = "Time", y = "concentration", title = "PK_GENE3")

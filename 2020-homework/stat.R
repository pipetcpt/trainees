library(pkr)
library(NonCompart)
library(ggplot2)
library(readxl)
library(dplyr)
library(ncar)

PK_data <- read_excel("Drug_X_PK.xlsx") %>% as.data.frame()

PK <- PK_data %>% select(ID, Drug) %>% distinct()
Dose <- PK$Drug

NCA <- NonCompart::tblNCA(PK_data, "ID", "TIME", "DV", dose = Dose, doseUnit = "mg", timeUnit = "h", concUnit = "ug/L", R2ADJ = 0.01)
View(NCA)


GENE_data <- PK_data %>% select(ID, GENE) %>% distinct()
stat <- left_join(NCA, GENE_data)
stat

oneway.test(CMAX ~ GENE, stat, var.equal = T)
oneway.test(AUCLST ~ GENE, stat, var.equal = T)

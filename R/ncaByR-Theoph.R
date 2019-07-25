# install.packages('NonCompart')
library(NonCompart) # NonCompartmental Analysis package.
library(tidyverse)

# ?NonCompart

head(Theoph)

# Theoph and Indometh data: dose in mg, conc in mg/L, time in h
ncaTheoph <- tblNCA(Theoph, dose=320, concUnit="mg/L")

write.csv(ncaTheoph, 'data/ncaTheoph.csv', row.names = FALSE)

# Objective 1. Sort by ID
# OBjective 2. Wide format table -> Long format table

ncaTheophLong <- ncaTheoph %>%
  as.tibble() %>% 
  arrange(Subject) %>% 
  gather(key = 'PKparameter', value = 'PKparameterValue', b0:CLFP)

write.csv(ncaTheophLong, 'data/ncaTheophLong.csv', row.names = FALSE)

# Objective 3. Do ranking of PKparameterValue by PKparameter
# Objective 4. Mark Ranking == 1 with star

ncaTheophLong %>% 
  group_by(PKparameter) %>% 
  mutate(Ranking = min_rank(desc(PKparameterValue))) %>% 
  mutate(Star = ifelse(Ranking == 1, 
                       yes = '*', 
                       no = ifelse(Ranking == 2, 
                                   yes = '**', 
                                   no = ' ')))

# Assignment to the subintern
# Objective: Do the similar job with Indometh dataset

head(Indometh)

?NonCompart # This command lets users know how to do NCA with indometh

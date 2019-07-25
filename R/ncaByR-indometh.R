library(NonCompart)
library(dplyr)
library(tidyr)
library(tibble)

head(Indometh)

ncaIndometh <- tabNCA(Indometh, colSubj="Subject", 
                      colTime="time", colConc="conc", 
                      dose=25, adm="Infusion", dur=0.5, concUnit="mg/L")
write.csv(ncaIndometh, 'data/ncaIndometh.csv', row.names = FALSE)

ncaIndometh %>% colnames()

ncaIndomethLong <- ncaIndometh %>%
  as.tibble() %>%
  arrange(ID) %>%
  gather(key = 'PKparameter', value = 'PKparameterValue', b0:VSSP)

write.csv(ncaIndomethLong, 'data/ncaIndomethLong.csv', row.names = FALSE)

ncaIndomethStar <- ncaIndomethLong %>%
  group_by(PKparameter) %>%
  mutate(Ranking = min_rank(desc(PKparameterValue))) %>%
  mutate(Star = ifelse(Ranking == 1,
                       yes = '*',
                       no = ifelse(Ranking == 6,
                                   yes = '**', 
                                   no = ' ')))

write.csv(ncaIndomethStar, 'data/ncaIndomethStar.csv', row.names = FALSE)

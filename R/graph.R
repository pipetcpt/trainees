library(tidyverse)

# start - COPD ----

read_csv('data-raw/COPD-FEV1-smoking.csv') %>% 
  gather(status, FEV1, No_smoking:Smoking) %>% 
  ggplot(aes(status, FEV1)) +
  geom_point()

# end - COPD ----

# Loading
PKRaw <- read.csv('data-raw/assay/Drug_X_PK.csv', stringsAsFactors = FALSE)

# There's missing ID so we implemented this code.
UniID <- unique(PKRaw$ID)

# Base plot
for (i in 1:length(UniID)){
  PKRaw01 = PKRaw[PKRaw$ID == UniID[i], ]
  plot(PKRaw01$TIME, PKRaw01$DV, main = paste0("SUBJID ", UniID[i], sep=" ", PKRaw01$Drug[1], "mg"), ylim=c(0,200), xlab="Time (hour)", ylab="Concentration (ug/L)")
  lines(PKRaw01$TIME, PKRaw01$DV, type="l")
}

# ggplot2 - saves png files in "assets/pk" folder.
library(ggplot2)

for (i in 1:length(UniID)){
  PKRaw01 = PKRaw[PKRaw$ID == UniID[i], ]
  FINAL = ggplot(PKRaw01, aes(x = TIME, y = DV)) + geom_point() + geom_line() + ggtitle(paste0("SUBJID ", UniID[i], sep=" ", PKRaw01$Drug[1], "mg")) + xlab("Time (hour)") + ylab("Concentration (ug/L)") + expand_limits(y=c(0,200))
  plot(FINAL)
  ggsave(filename = paste0("assets/pk/SUBJID", UniID[i], ".png"), FINAL)
}

# data analysis
mean(unique(PKRaw$HT))
# [1] 173.4746 cm
mean(unique(PKRaw$WT))
# [1] 69.15598 kg
mean(unique(PKRaw$AGE))
# [1] 27.7 year

# mean weight according to gene 
mean(unique(PKRaw$WT[PKRaw$GENE == 1]))
mean(unique(PKRaw$WT[PKRaw$GENE == 2]))
mean(unique(PKRaw$WT[PKRaw$GENE == 3]))

# mean weight according to gene using dplyr
library(dplyr)
PKRaw %>% 
  group_by(GENE) %>% 
  summarise(MEAN.AGE = mean(AGE), MEAN.WT = mean(WT), MEAN.HT = mean(HT))

PKRaw %>% 
  group_by(ID) %>% 
  summarise(Cmax = max(DV))

head(PKRaw)
tail(PKRaw)

PKRaw %>% 
  ggplot(aes(x=TIME, y=DV, group=ID, color=as.factor(Drug))) +
  geom_line() +
  geom_point()

# Not a good idea. GENE may contain various Drug amount.
PKRaw %>% 
  group_by(ID) %>% 
  mutate(RankDV = min_rank(desc(DV))) %>% 
  filter(RankDV == 1) %>% 
  select(ID, Tmax = TIME, Cmax = DV, GENE) %>% 
  group_by(GENE) %>% 
  summarise(Tmax.Mean = mean(Tmax), Cmax.Mean = mean(Cmax))

PKRaw %>% 
  group_by(ID) %>% 
  mutate(RankDV = min_rank(desc(DV))) %>% 
  filter(RankDV == 1) %>% 
  select(ID, Tmax = TIME, Cmax = DV, GENE, Drug) %>% 
  group_by(Drug, GENE) %>% 
  summarise(Tmax.Mean = mean(Tmax), Cmax.Mean = mean(Cmax))
# -> this result(effect of gene on Cmax) is not impressive.

DosePK = PKRaw %>% 
  group_by(ID) %>% 
  mutate(RankDV = min_rank(desc(DV))) %>% 
  filter(RankDV == 1) %>% 
  select(ID, Tmax = TIME, Cmax = DV, GENE, Drug) %>% 
  group_by(Drug) %>% 
  summarise(Tmax.Mean = mean(Tmax), Cmax.Mean = mean(Cmax))
# -> this result will be used to draw plot.
colnames(DosePK)

library(tidyr)
TidyDosePK = gather(DosePK, key = PARAMETER, VALUE, Tmax.Mean:Cmax.Mean)
ggplot(TidyDosePK) +
  geom_point(mapping = aes(x = Drug, y = VALUE, color = PARAMETER)) +
  geom_line(mapping = aes(x = Drug, y = VALUE, color = PARAMETER))

# for fun (PK)

PK = read.csv("Xproject_Final Parameters Pivoted.csv") # WNL
TidyPK = gather(PK, WNLPARAMTER, WNLVALUE, 2:dim(PK)[2])
ggplot(TidyPK) +
  geom_point(mapping = aes(x = ID, y = WNLVALUE, color = WNLPARAMTER)) +
  geom_line(mapping = aes(x = ID, y = WNLVALUE, color = WNLPARAMTER))

# ggplot2 - put all data in a single plot

PKRaw01 <- PKRaw[PKRaw$ID == UniID[i], ]
FINAL <- ggplot(PKRaw01, aes(x = TIME, y = DV)) + 
  geom_point() + 
  geom_line() + 
  ggtitle(paste0("SUBJID ", UniID[i], sep=" ", PKRaw01$Drug[1], "mg")) + 
  xlab("Time (hour)") + 
  ylab("Concentration (ug/L)") + 
  expand_limits(y=c(0,200))
FINAL

ggsave(filename = paste0("assets/pk/SUBJID", UniID[i], ".png"), FINAL)

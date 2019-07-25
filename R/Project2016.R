
#setwd("D:/AMC")
setwd("~/GIT/WNL") # OSX

# Required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Loading
PKRaw = read.csv("resources/assay/Drug_X_PK.csv")

# There's missing ID so we implemented this code.
UniID = unique(PKRaw$ID)

# Base plot
for (i in 1:length(UniID)){
  PKRaw01 = PKRaw[PKRaw$ID == UniID[i], ]
  plot(PKRaw01$TIME, PKRaw01$DV, main = paste0("SUBJID ", UniID[i], sep=" ", PKRaw01$Drug[1], "mg"), ylim=c(0,200), xlab="Time (hour)", ylab="Concentration (ug/L)")
  lines(PKRaw01$TIME, PKRaw01$DV, type="l")
}

# ggplot2 - saves png files in "ggplotfigs" folder.
for (i in 1:length(UniID)){
  PKRaw01 = PKRaw[PKRaw$ID == UniID[i], ]
  FINAL = ggplot(PKRaw01, aes(x = TIME, y = DV)) + geom_point() + geom_line() + ggtitle(paste0("SUBJID ", UniID[i], sep=" ", PKRaw01$Drug[1], "mg")) + xlab("Time (hour)") + ylab("Concentration (ug/L)") + expand_limits(y=c(0,200))
  plot(FINAL)
  ggsave(filename = paste0("result/ggplotfigs/SUBJID", UniID[i], ".png"), FINAL)
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
  summarise(AGE.WT = mean(AGE), MEAN.WT = mean(WT), MEAN.HT = mean(HT))

PKRaw %>% 
  group_by(ID) %>% 
  summarise(Cmax = max(DV))

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
PKRaw01 = PKRaw[PKRaw$ID == UniID[i], ]
FINAL = ggplot(PKRaw01, aes(x = TIME, y = DV)) + geom_point() + geom_line() + ggtitle(paste0("SUBJID ", UniID[i], sep=" ", PKRaw01$Drug[1], "mg")) + xlab("Time (hour)") + ylab("Concentration (ug/L)") + expand_limits(y=c(0,200))
plot(FINAL)
ggsave(filename = paste0("ggplotfigs/SUBJID", UniID[i], ".png"), FINAL)

# WNL PK column subsetting
PK <- read.csv("resources/WNL/Xproject_Final Parameters Pivoted.csv") # WNL
PKWhatWeNeed <- PK %>% select(ID, AUClast, AUCINF_obs, Cmax, Tmax, Lambda_z, Cl_F_obs, Vz_F_obs, MRTlast)
write.csv(PKWhatWeNeed, "Xproject_Final_for_Word.csv", row.names = FALSE)


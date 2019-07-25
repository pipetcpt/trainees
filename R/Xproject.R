PK = read.csv("Xproject_Final Parameters Pivoted.csv") # WNL
PKWhatWeNeed = data.frame(PK$ID, PK$AUClast, PK$AUCINF_obs, PK$Cmax, PK$Tmax, PK$Lambda_z, PK$Cl_F_obs, PK$Vz_F_obs, PK$MRTlast)
write.csv(PKWhatWeNeed, "Xproject_Final_for_Word.csv", row.names = FALSE)
plot(PKWhatWeNeed$PK.Cmax)

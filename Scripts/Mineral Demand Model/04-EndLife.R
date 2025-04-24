# Mineral Demand Module
# PBH January 2025
# End of Life (Lifetime) Scenarios 

source("Scripts/Mineral Demand Model/00-LoadInputs.R")

mineral_perLIB <- mineral_perLIB_all %>% 
  filter(capacity_scenario=="Baseline",chem_scenario=="Baseline") %>% 
  mutate(capacity_scenario=NULL,chem_scenario=NULL) %>% 
  complete(c, Year, Mineral, fill = list(kg_per_LIB = 0))

source("Scripts/Mineral Demand Model/RunMineralDemand.R")

# Lifetime scenarios ------

## Longer lifetime ----
mean_ev=20;sd_ev=4;mean_lib=20;sd_lib=4

# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling)
nrow(df)/1e6
write.csv(df,"Results/Mineral Demand/longerLifetime.csv",row.names = F)

if(no_Trade){ # No Trade Ratios consideration
  df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling,
                        considerMONET=F,consider2HandTrade=F)
  write.csv(df,"Results/Mineral Demand/No Trade/longerLifetime.csv",row.names = F)
}

if(circular){ # 100% recycling assumption
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling)
  write.csv(df,"Results/Mineral Demand/Circular/longerLifetime.csv",row.names = F)
}


## Shorter LIB lifetime ----
mean_ev=17;sd_ev=4;mean_lib=10;sd_lib=4

# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling)
nrow(df)/1e6
write.csv(df,"Results/Mineral Demand/shorterLifetime.csv",row.names = F)

if(no_Trade){ # No Trade Ratios consideration
  df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling,
                        considerMONET=F,consider2HandTrade=F)
  write.csv(df,"Results/Mineral Demand/No Trade/shorterLifetime.csv",row.names = F)
}

if(circular){ # 100% recycling assumption
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling)
  write.csv(df,"Results/Mineral Demand/Circular/shorterLifetime.csv",row.names = F)
}


# EoF
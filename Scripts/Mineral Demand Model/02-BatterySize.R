# Mineral Demand Module
# PBH January 2025
# Battery Size Scenarios


source("Scripts/Mineral Demand Model/00-LoadInputs.R")

source("Scripts/Mineral Demand Model/RunMineralDemand.R")

# Battery Size scenarios - Input from Supply paper scenarios ------


# Small LIB -------

## Minerals per EV by Year
mineral_perLIB <- mineral_perLIB_all %>% 
  filter(capacity_scenario=="Low Capacity",chem_scenario=="Baseline") %>% 
  mutate(capacity_scenario=NULL,chem_scenario=NULL) %>% 
  complete(c, Year, Mineral, fill = list(kg_per_LIB = 0))

# Run Mineral Demand 
# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling)
nrow(df)/1e6
write.csv(df,"Results/Mineral Demand/smallLIB.csv",row.names = F)

if(no_Trade){ # No Trade Ratios consideration
  df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling,
                        considerMONET=F,consider2HandTrade=F)
  write.csv(df,"Results/Mineral Demand/No Trade/smallLIB.csv",row.names = F)
}

if(circular){ # 100% recycling assumption
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling)
  write.csv(df,"Results/Mineral Demand/Circular/smallLIB.csv",row.names = F)
}

# Large LIB -------

## Minerals per EV by Year
mineral_perLIB <- mineral_perLIB_all %>% 
  filter(capacity_scenario=="High Capacity",chem_scenario=="Baseline") %>% 
  mutate(capacity_scenario=NULL,chem_scenario=NULL) %>% 
  complete(c, Year, Mineral, fill = list(kg_per_LIB = 0))

# Run Mineral Demand 
# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling)
nrow(df)/1e6
write.csv(df,"Results/Mineral Demand/largeLIB.csv",row.names = F)

if(no_Trade){ # No Trade Ratios consideration
  df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling,
                        considerMONET=F,consider2HandTrade=F)
  write.csv(df,"Results/Mineral Demand/No Trade/largeLIB.csv",row.names = F)
}

if(circular){ # 100% recycling assumption
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling)
  write.csv(df,"Results/Mineral Demand/Circular/largeLIB.csv",row.names = F)
}


# EoF
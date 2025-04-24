# Mineral Demand Module
# PBH January 2025
# Recycling Scenarios 

source("Scripts/Mineral Demand Model/00-LoadInputs.R")

mineral_perLIB <- mineral_perLIB_all %>% 
  filter(capacity_scenario=="Baseline",chem_scenario=="Baseline") %>% 
  mutate(capacity_scenario=NULL,chem_scenario=NULL) %>% 
  complete(c, Year, Mineral, fill = list(kg_per_LIB = 0))

source("Scripts/Mineral Demand Model/RunMineralDemand.R")

# Recycling scenarios ------

## 30% world ----

# 30% by 2032, linearly from 2022
recyc_scen <- tibble(Year=2022:2050) %>% 
  mutate(mat_recov_recyc=if_else(Year<2033,
                                      (Year-2022)*0.3/10,
                                      0.3))

mat_recovery_recycling_scen <- expand.grid(c=countries,Year=2022:2050,
                                           Mineral=c("Lithium","Nickel","Cobalt","Graphite","kWh")) %>% 
  left_join(recyc_scen)
  

# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling_scen)
nrow(df)/1e6
write.csv(df,"Results/Mineral Demand/recyclingLow.csv",row.names = F)

if(no_Trade){ # No Trade Ratios consideration
  df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling_scen,
                        considerMONET=F,consider2HandTrade=F)
  write.csv(df,"Results/Mineral Demand/No Trade/recyclingLow.csv",row.names = F)
}

## 50% world ----

# 50% by 2032, linearly from 2022
recyc_scen <- tibble(Year=2022:2050) %>% 
  mutate(mat_recov_recyc=if_else(Year<2033,
                                 (Year-2022)*0.5/10,
                                 0.5))

mat_recovery_recycling_scen <- expand.grid(c=countries,Year=2022:2050,
                                           Mineral=c("Lithium","Nickel","Cobalt","Graphite","kWh")) %>% 
  left_join(recyc_scen)


# 2-3 minutes each run
# df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling_scen)
nrow(df)/1e6
# write.csv(df,"Results/Mineral Demand/recyclingMedium.csv",row.names = F)

if(no_Trade){ # No Trade Ratios consideration
  df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling_scen,
                        considerMONET=F,consider2HandTrade=F)
  write.csv(df,"Results/Mineral Demand/No Trade/recyclingMedium.csv",row.names = F)
}


## 80% world ----

# 80% by 2032, linearly from 2022
recyc_scen <- tibble(Year=2022:2050) %>% 
  mutate(mat_recov_recyc=if_else(Year<2033,
                                      (Year-2022)*0.8/10,
                                      0.8))

mat_recovery_recycling_scen <- expand.grid(c=countries,Year=2022:2050,
                                           Mineral=c("Lithium","Nickel","Cobalt","Graphite","kWh")) %>% 
  left_join(recyc_scen)


# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling_scen)
nrow(df)/1e6
write.csv(df,"Results/Mineral Demand/recyclingHigh.csv",row.names = F)

if(no_Trade){ # No Trade Ratios consideration
  df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling_scen,
                        considerMONET=F,consider2HandTrade=F)
  write.csv(df,"Results/Mineral Demand/No Trade/recyclingHigh.csv",row.names = F)
}

## High recycling 80% in EV Manufacturer countries ------

# 80% by 2032, linearly from 2022
recyc_scen <- tibble(Year=2022:2050) %>% 
  mutate(mat_recov_recyc=if_else(Year<2033,
                                      (Year-2022)*0.8/10,
                                      0.8))

# China and EU already have recycling mandates
countries_ev <- c("Japan","Mexico","United States","South Korea",
                  "India","Brazil","Thailand","Canada","United Kingdom")

mat_recovery_recycling_scen <- expand.grid(c=countries,Year=2022:2050,
                                           Mineral=c("Lithium","Nickel","Cobalt","Graphite","kWh")) %>% 
  left_join(recyc_scen) %>% 
  mutate(mat_recov_recyc=if_else(c %in% countries_ev,mat_recov_recyc,
                                 world_baseline_recycling))


# 2-3 minutes each run
# df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling_scen)
nrow(df)/1e6
# write.csv(df,"Results/Mineral Demand/recyclingManufacturing.csv",row.names = F)

if(no_Trade){ # No Trade Ratios consideration
  # df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling_scen,
  #                       considerMONET=F,consider2HandTrade=F)
  # write.csv(df,"Results/Mineral Demand/No Trade/recyclingManufacturing.csv",row.names = F)
}

# EoF
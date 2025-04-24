# Mineral Demand Module
# PBH January 2025
# Battery Chemistry Scenarios

source("Scripts/Mineral Demand Model/00-LoadInputs.R")


# Loop for chemistry scenarios
source("Scripts/Mineral Demand Model/RunMineralDemand.R")
for (ch in unique(mineral_perLIB_all$chem_scenario)){
  
  if(ch !="Baseline"){
    
    cat("",ch,"\n\n")
    
    mineral_perLIB <- mineral_perLIB_all %>% 
      filter(capacity_scenario=="Baseline",chem_scenario==ch) %>% 
      mutate(capacity_scenario=NULL,chem_scenario=NULL) %>% 
      complete(c, Year, Mineral, fill = list(kg_per_LIB = 0))
    
    # 2-3 minutes each run
    df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling)
    nrow(df)/1e6
    
    ## Save results -----
    write.csv(df,paste0("Results/Mineral Demand/",ch,".csv"),row.names = F)
    
    
    if(no_Trade){ # No Trade Ratios consideration
      df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling,
                            considerMONET=F,consider2HandTrade=F)
      write.csv(df,paste0("Results/Mineral Demand/No Trade/",ch,".csv"),row.names = F)
    }
    
    if(circular){ # 100% recycling assumption
      df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling)
      write.csv(df,paste0("Results/Mineral Demand/Circular/",ch,".csv"),row.names = F)
    }
  }
}

# EoF
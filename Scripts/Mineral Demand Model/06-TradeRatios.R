# Mineral Demand Module
# PBH January 2025
# Trade Ratios Scenarios

source("Scripts/Mineral Demand Model/00-LoadInputs.R")

mineral_perLIB <- mineral_perLIB_all %>% 
  filter(capacity_scenario=="Baseline",chem_scenario=="Baseline") %>% 
  mutate(capacity_scenario=NULL,chem_scenario=NULL) %>% 
  complete(c, Year, Mineral, fill = list(kg_per_LIB = 0))

source("Scripts/Mineral Demand Model/RunMineralDemand.R")


# New Vehicle Trade Ratios scenarios -----

sales_ratio # MONET

## Higher Domestic Supply -----

sales_ratio <- read.csv("Inputs/Trade Ratios/salesShare_HigherDS.csv")
names_aux <- sales_ratio$X
sales_ratio$X <- NULL
sales_ratio <- as.matrix(sales_ratio)
rownames(sales_ratio) <- colnames(sales_ratio) <- names_aux
# same order of countries
sales_ratio <- sales_ratio[countries,countries]
colSums(sales_ratio)

# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling)
nrow(df)/1e6
write.csv(df,"Results/Mineral Demand/MONET_HighDS.csv",row.names = F)


if(circular){ # 100% recycling assumption
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling)
  write.csv(df,"Results/Mineral Demand/Circular/MONET_HighDS.csv",row.names = F)
}


## Global Free Trade -----

sales_ratio <- read.csv("Inputs/Trade Ratios/salesShare_GlobalTrade.csv")
names_aux <- sales_ratio$X
sales_ratio$X <- NULL
sales_ratio <- as.matrix(sales_ratio)
rownames(sales_ratio) <- colnames(sales_ratio) <- names_aux
# same order of countries
sales_ratio <- sales_ratio[countries,countries]
colSums(sales_ratio)


# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling)
nrow(df)/1e6
write.csv(df,"Results/Mineral Demand/MONET_FreeTrade.csv",row.names = F)


if(circular){ # 100% recycling assumption
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling)
  write.csv(df,"Results/Mineral Demand/Circular/MONET_FreeTrade.csv",row.names = F)
}


# USED Vehicle Trade Ratios scenarios -----

tradeVeh # 2-Hand trade ratios


## China  -----

tradeVeh <- readxl::read_excel("Inputs/Trade Ratios/2023.CHN.scn.xlsx",
                               sheet="exports.share")
tradeVeh <- as.matrix(tradeVeh)
colnames(tradeVeh) <- colnames(tradeVeh) %>% str_replace_all("\\."," ") %>% 
  str_replace("Asia/oceania","Asia/Oceania")
rownames(tradeVeh) <- colnames(tradeVeh)
aux <- which(rownames(tradeVeh)=="Taiwan") # Remove Taiwan - Fix at trade ratios level
tradeVeh <- tradeVeh[-aux,-aux]
tradeVeh <- tradeVeh[countries,countries]
tradeVeh <- sweep(tradeVeh,1,rowSums(tradeVeh),"/") # fix Taiwan removal by re-balancing
rm(aux)
rowSums(tradeVeh)

# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling)
nrow(df)/1e6
write.csv(df,"Results/Mineral Demand/usedVeh_CHN.csv",row.names = F)

if(circular){ # 100% recycling assumption
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling)
  write.csv(df,"Results/Mineral Demand/Circular/usedVeh_CHN.csv",row.names = F)
}


## US Mexico -----

tradeVeh <- readxl::read_excel("Inputs/Trade Ratios/2023.useu.scn.xlsx",
                               sheet="export.share")
tradeVeh <- as.matrix(tradeVeh)
colnames(tradeVeh) <- colnames(tradeVeh) %>% str_replace_all("\\."," ") %>% 
  str_replace("Asia/oceania","Asia/Oceania")
rownames(tradeVeh) <- colnames(tradeVeh)
aux <- which(rownames(tradeVeh)=="Taiwan") # Remove Taiwan - Fix at trade ratios level
tradeVeh <- tradeVeh[-aux,-aux]
tradeVeh <- tradeVeh[countries,countries]
tradeVeh <- sweep(tradeVeh,1,rowSums(tradeVeh),"/") # fix Taiwan removal by re-balancing
rm(aux)
rowSums(tradeVeh)


# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling)
nrow(df)/1e6
write.csv(df,"Results/Mineral Demand/usedVeh_Mex.csv",row.names = F)

if(circular){ # 100% recycling assumption
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling)
  write.csv(df,"Results/Mineral Demand/Circular/usedVeh_Mex.csv",row.names = F)
}

# EoF
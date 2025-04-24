# Mineral Demand Module
# PBH July 2024
# Reference Scenario
# Updated: January 2025 for inputs that change over time (indexed by year)

source("Scripts/Mineral Demand Model/00-LoadInputs.R")

mineral_perLIB <- mineral_perLIB_all %>% 
  filter(capacity_scenario=="Baseline",chem_scenario=="Baseline") %>% 
  mutate(capacity_scenario=NULL,chem_scenario=NULL) %>% 
  complete(c, Year, Mineral, fill = list(kg_per_LIB = 0))
# unique(mineral_perLIB$c)
mineral_perLIB %>% filter(c=="United States",Year==2024)


# Run Mineral Demand -----
source("Scripts/Mineral Demand Model/RunMineralDemand.R")

# 2-3 minutes each run
df <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling)
nrow(df)/1e6

## Save results -----
write.csv(df,"Results/Mineral Demand/Reference.csv",row.names = F)

# Exploratory figure
df %>%  
  left_join(dict_region,by="c") %>%
  # mutate(Sector=Region_EV) %>%
  filter(Mineral=="Lithium") %>%
  # mutate(c=Region_EV) %>% 
  group_by(Year,Sector,Mineral,c) %>% 
  reframe(ktons=sum(tons)/1e3) %>% 
  ggplot(aes(Year,ktons,fill=Sector))+
  geom_area()+
  # facet_wrap(~Mineral,scales="free_y")+
  facet_wrap(~c,scales="free_y")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  labs(x="",y="",title="Mineral EV Demand [ktons]",fill="")
df %>% filter(Mineral=="Lithium") %>% pull(tons) %>% sum()/1e6

# NO Monet ------
# Run without MONET allocation to EV production
df_monet <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling,
                            considerMONET = F)
write.csv(df_monet,"Results/Mineral Demand/NoMonet.csv",row.names = F)


# No 2-hand TR ----
# Run with no second hand flow trade
df_2hand <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling,
                            consider2HandTrade = F)
write.csv(df_2hand,"Results/Mineral Demand/No2Hand.csv",row.names = F)


df_monet2hand <- f.MineralDemand(icct,mineral_perLIB,mat_recovery_recycling,
                                 considerMONET = F,
                                 consider2HandTrade = F)
write.csv(df_monet2hand,"Results/Mineral Demand/No Trade/Reference.csv",row.names = F)


if(circular){ # 100% recycling assumption
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling)
  write.csv(df,"Results/Mineral Demand/Circular/Reference.csv",row.names = F)
}

# Circular and No Trade combination
if(circular & no_Trade){ # 100% recycling assumption
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling,
                        considerMONET = F)
  write.csv(df,"Results/Mineral Demand/Circular/NoMonet.csv",row.names = F)
  
  df <- f.MineralDemand(icct,mineral_perLIB,circular_recycling,
                        considerMONET = F,
                        consider2HandTrade = F)
  write.csv(df,"Results/Mineral Demand/Circular/NoTrade.csv",row.names = F)
}

# EoF
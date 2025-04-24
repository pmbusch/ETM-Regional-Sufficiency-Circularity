# Ratio of reserves over cumulative demand
# PBH August 2024


source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/Load Data/05-Load_USGS.R", encoding = "UTF-8")
dict_scen <- read_excel("Inputs/Dict/Dict_Scenario.xlsx")


# Load demand results - Reference scenario ------
# Load from folder results

df <- read.csv("Results/Mineral Demand/Reference.csv")
head(df)
# remove battery needs
df <- df %>% filter(Mineral!="kWh")


# table demand against reserves -----------
demand <- df %>% 
  left_join(dict_region,by="c")
demand_world <- demand %>% 
  group_by(Mineral) %>% 
  reframe(mtons=sum(tons)/1e6) %>% ungroup() %>% 
  mutate(Region_EV="World")
demand_region <- demand %>% 
  group_by(Mineral,Region_EV) %>% 
  reframe(mtons=sum(tons)/1e6) %>% ungroup() %>% 
  rbind(demand_world)

usgs_res <- usgs_world %>% 
  dplyr::select(c,mtons,Mineral) %>% rename(Region_EV=c) %>%  
  rbind(usgs_region) %>% 
  rename(reserve=mtons)

region_colors
# table ------
demand_region %>% 
  left_join(usgs_res) %>% 
  mutate(ratio_res=reserve/mtons) %>% 
  mutate(Region_EV=factor(Region_EV,levels=names(region_colors))) %>% 
  dplyr::select(-mtons,-reserve) %>% 
  pivot_wider(names_from = Mineral, values_from = ratio_res) %>% 
  arrange(Region_EV) %>% dplyr::select(Region_EV,Lithium,Nickel,Cobalt,Graphite)

.Last.value %>% write.table("clipboard", sep="\t",row.names = F)


# EoF
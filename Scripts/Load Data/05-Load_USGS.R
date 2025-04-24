# Load USGS Reserves 2024 Data
# PBH July 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# in metric tons
usgs <- readxl::read_excel("Inputs/USGS2025_Reserves.xlsx",sheet="Reserves_tons")

# to million tons
usgs <- usgs %>% dplyr::select(-MONET_Region) %>% 
  pivot_longer(c(-Country,-MONET_Country,-Region_EV), 
               names_to = "Mineral", values_to = "tons") %>% 
  mutate(mtons=tons/1e6) %>% 
  mutate(min_label=paste0(Mineral," 2025 Reserves")) %>% 
  rename(c=MONET_Country)

(usgs_world <- usgs %>% filter(Country=="WORLD"))
usgs <- usgs %>% filter(Country!="WORLD") 

# by region
usgs_region <- usgs %>% 
  filter(!is.na(Region_EV)) %>% 
  group_by(Region_EV,Mineral) %>% 
  reframe(mtons=sum(mtons,na.rm=T))


# EoF
# Battery Capacity by County
# Source of Data: EV Volumes
# PBH August 2023 - Updated March 2025


# LOAD DATA -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# 2024 EV Volumes data - Not possible to Share
bat <- read.csv("Inputs/Original/BatteryInstallation-Tracker-January_2025_Data.csv")
(names(bat) <- names(bat) %>% str_remove("&") %>% str_replace_all(" |-|\\.","_") %>% 
    str_replace_all("__","_") %>% str_remove("Delivered_"))

# to long format
bat <- bat %>% 
  dplyr::select(-OEM_Group,-Brand,-Make_Model,-Architecture,
                -LCV_Details,-Fast_Charging,-Battery_kWh,-ED_Wh_kg_Cell,
                -Cell_Type,-Cell_Supplier,-Cathode_Supplier) %>% 
  pivot_longer(-c(Sales_Region,Sales_Sub_Region,Sales_Country, Global_Segment,
                 Vehicle_Production_Region,Vehicle_Production_Country,
                 Propulsion,Cathode_Chemistry,Cathode_Mix), names_to = "key", values_to = "value")
unique(bat$key)  

# get total EV registrations and MWh
bat <- bat %>% 
  mutate(type=str_extract(key,"Mwh|Reg"),
         year=str_extract(key,paste0(2013:2025,collapse="|"))) %>% 
  filter(year<2025,value>0)
unique(bat$year)  
unique(bat$type)
range(bat$value)

# only BEV
bat <- bat %>% filter(Propulsion=="BEV")

# summarise by year and region
bat <- bat %>% 
  group_by(Sales_Region,Sales_Sub_Region,Sales_Country, Global_Segment,
           Vehicle_Production_Region,Vehicle_Production_Country,
           Propulsion,Cathode_Chemistry,Cathode_Mix,type,year) %>% 
  reframe(value=sum(value))
bat <- bat %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  rename(MWh=Mwh,unit=Reg)

# check
bat %>% filter(year==2018) %>% pull(MWh) %>% sum() # 66928

# DATA WRANGLING -----

## Aggregate Chemistry 2024-----
bat %>% filter(year==2024) %>% 
  group_by(Cathode_Chemistry) %>% summarise(x=sum(MWh,na.rm=T)) %>%
  arrange(desc(x)) %>% ungroup() %>% mutate(perc=x/sum(x)*100)
bat %>% filter(year==2024) %>% 
  group_by(Cathode_Mix) %>% summarise(x=sum(MWh,na.rm=T)) %>%
  arrange(desc(x)) %>% ungroup() %>% mutate(perc=x/sum(x)*100)
# NMC ratios: 721, 622, 811, 532, 111
# Others NMCA 89-4-4-3 (used by Tesla only) https://evreporter.com/nmca-cathode-for-lithium-ion-batteries/

# Mix aggregation
bat %>% group_by(Cathode_Chemistry,Cathode_Mix) %>% tally() %>% arrange(desc(n))
bat <- bat %>% 
  mutate(mix=case_when(
    str_detect(Cathode_Mix,"NMC 111") & Cathode_Chemistry!="LMO" ~ " 111",
    str_detect(Cathode_Mix,"NMC 721") ~ " 721",
    str_detect(Cathode_Mix,"NMC 532|NMC532|NMC 523") ~ " 532", # I assume 523 is a typo error
    str_detect(Cathode_Mix,"NMC 622|NMC622") ~ " 622",
    str_detect(Cathode_Mix,"NMC 442") ~ " 442",
    str_detect(Cathode_Mix,"NMC 811") ~ " 811",
    str_detect(Cathode_Mix,"NMCA 89:04:04:03") ~ "A 89:4:4:3",
    T ~ ""))
bat %>% group_by(Cathode_Chemistry,Cathode_Mix,mix) %>% tally() %>% arrange(desc(n))

bat <- bat %>% mutate(chemistry=paste0(Cathode_Chemistry,mix))

# share of ratios of NMC
unique(bat$Propulsion)
bat %>% 
  filter(chemistry %in% c("NMC","NMC 111","NMC 811","NMC 622","NMC 532")) %>% 
                          # "NMC 721","NMCA 89:4:4:3","NMC 442")) %>%
  filter(Propulsion!="FCEV") %>% 
  filter(year==2024) %>% 
  # mutate(Sales_Region="World") %>%
  group_by(Propulsion,Sales_Region,chemistry) %>% 
  summarise(MWh=sum(MWh)) %>% ungroup() %>% 
  group_by(Propulsion,Sales_Region) %>% 
  mutate(perc=MWh/sum(MWh)) %>% 
  ggplot(aes(Sales_Region,perc,fill=chemistry))+
  geom_col()+
  coord_flip()+
  facet_wrap(~Propulsion,nrow=1)

# Do it by world shares of NMC, easier to explain. Only major difference will be Japan
nmc_share <- bat %>% 
  filter(chemistry %in% c("NMC 111","NMC 811","NMC 622","NMC 532")) %>%
  #   "NMC 721","NMCA 89:4:4:3" not considered to go towards NMC with no detail
  filter(Propulsion=="BEV") %>% 
  filter(year==2024) %>% 
  group_by(chemistry) %>% 
  summarise(MWh=sum(MWh)) %>% ungroup() %>% 
  mutate(perc=MWh/sum(MWh)) 

# aggregate chemistry - above 2% (for figure)
chem_selected <- bat %>% group_by(chemistry) %>% filter(year==2024) %>% 
  summarise(x=sum(MWh,na.rm=T)) %>%
  arrange(desc(x)) %>% ungroup() %>% mutate(perc=x/sum(x)*100) %>% 
  filter(perc>2) %>% pull(chemistry)
chem_selected <- c(chem_selected,"NMC 111","LMO")
bat <- bat %>% mutate(chemistry=if_else(chemistry %in% chem_selected,chemistry,"Other"))
rm(chem_selected)

# other share
other_share <- bat %>% 
  filter(!(chemistry %in% c("NMC","Other"))) %>% 
  filter(year==2024) %>% 
  group_by(chemistry) %>% 
  summarise(MWh=sum(MWh)) %>% ungroup() %>% 
  mutate(perc=MWh/sum(MWh)) 


## Flat years -----
bat <- bat %>% mutate(unit = if_else(is.na(unit), 0, unit))

# Checks
bat %>% group_by(year) %>% summarise(MWh=sum(MWh),unit=sum(unit,na.rm = T)) %>% 
  mutate(kWh_veh=MWh*1e3/unit)

## orders -----

bat$Sales_Region %>% table()
region_order <- c("Americas","Europe","Asia-Pacific","Africa & ME")
bat <- bat %>% mutate(Sales_Region=factor(Sales_Region,levels=region_order))

chem_order <- bat %>% group_by(chemistry) %>% 
  summarise(MWh=sum(MWh),unit=sum(unit,na.rm = T)) %>% 
  mutate(kWh_veh=MWh*1e3/unit) %>% arrange(kWh_veh) %>% pull(chemistry)
bat <- bat %>% mutate(chemistry=factor(chemistry,levels=chem_order))

## Dimensions -----
bat$Sales_Region %>% unique()
bat$Sales_Sub_Region %>% unique() # 13
bat$Sales_Country %>% unique() # 121
bat$year %>% unique()
bat$Propulsion %>% unique() 
bat$chemistry %>% unique()


# Distribute NMC and Other as averages
nmc_share
bat_nmc <- bat %>% filter(chemistry=="NMC") %>% rename(x=chemistry)

bat_nmc <- bat_nmc %>% 
  filter(MWh>0, unit>0) %>% 
  left_join(mutate(nmc_share,x="NMC",MWh=NULL),relationship = "many-to-many") %>% 
  mutate(MWh=MWh*perc,unit=unit*perc,
         perc=NULL,x=NULL)

# add back to database
bat <- bat %>% filter(chemistry!="NMC") %>% rbind(bat_nmc)
rm(bat_nmc)

# others share - same method
other_share
bat_other <- bat %>% filter(chemistry=="Other") %>% rename(x=chemistry)
bat_other <- bat_other %>% 
  filter(MWh>0, unit>0) %>% 
  left_join(mutate(nmc_share,x="Other",MWh=NULL),relationship = "many-to-many") %>% 
  mutate(MWh=MWh*perc,unit=unit*perc,
         perc=NULL,x=NULL)
bat <- bat %>% filter(chemistry!="Other") %>% rbind(bat_other)
rm(bat_other)

bat$chemistry %>% unique()

# Aggregate data -----

## World level ----------
unique(bat$Propulsion)
bat_world <- bat %>% 
  filter(year>2014) %>% 
  group_by(year,chemistry) %>% 
  summarise(MWh=sum(MWh,na.rm=T),
            unit=sum(unit,na.rm=T)) %>% ungroup()

# kWh per vehicle
# Note: chemistry is taking as weighted average by units
bat_world <- bat_world %>% 
  filter(MWh>0&unit>0) %>% 
  group_by(year) %>% 
  mutate(share_units=unit/sum(unit)) %>% # share of units by chemistry
  mutate(kwh_veh=sum(MWh)*1e3/sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh=kwh_veh*share_units) %>% 
  group_by(year) %>% 
  mutate(kwh_veh_total=sum(kwh_veh)) %>% ungroup()

bat_world <- bat_world %>% rename(Year=year)


# Country level ----------

# MONET 65 regions
eq <- read_excel("Inputs/Dict/Dict_Countries_ICCT_EVV_MONET.xlsx",sheet="EVV_to_ICCT") %>% 
  dplyr::select(EVV_Country,c)

bat_country <- bat %>% left_join(eq,by=c("Sales_Country"="EVV_Country")) 
sum(is.na(bat_country$c))

bat_country <- bat_country %>% 
  filter(year>2014) %>% 
  group_by(year,c,chemistry) %>% 
  summarise(MWh=sum(MWh,na.rm=T),
            unit=sum(unit,na.rm=T)) %>% ungroup()

# kWh per vehicle
# Note: chemistry is taking as weighted average by units
bat_country <- bat_country %>% 
  filter(MWh>0&unit>0) %>% 
  group_by(year,c) %>% 
  mutate(share_units=unit/sum(unit)) %>% # share of units by chemistry
  mutate(kwh_veh=sum(MWh)*1e3/sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh=kwh_veh*share_units) %>% 
  group_by(year,c) %>% 
  mutate(kwh_veh_total=sum(kwh_veh)) %>% ungroup()

bat_country <- bat_country %>% rename(Year=year)

# SAVE DATA -----------

write.csv(bat_world,"Inputs/Processed/battery_world.csv",row.names = F)
write.csv(bat_country,"Inputs/Processed/battery_country.csv",row.names = F)

## Save battery size -----
bat_world_size <- bat_world %>% group_by(Year) %>% 
  reframe(MWh=sum(MWh),unit=sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh_total=MWh/unit*1e3)
write.csv(bat_world_size,"Inputs/Processed/battery_size_world.csv",row.names = F)

bat_country_size <- bat_country %>% group_by(Year,c) %>% 
  reframe(MWh=sum(MWh),unit=sum(unit)) %>% ungroup() %>% 
  mutate(kwh_veh_total=MWh/unit*1e3)
write.csv(bat_country_size,"Inputs/Processed/battery_size_country.csv",row.names = F)

## Save Chem Share ----------
bat_world_chem <- bat_world %>% group_by(Year,chemistry) %>% 
  reframe(MWh=sum(MWh)) %>% ungroup() %>% group_by(Year) %>% 
  mutate(chem_share_mwh=MWh/sum(MWh))
write.csv(bat_world_chem,"Inputs/Processed/battery_chem_world.csv",row.names = F)

bat_country_chem <- bat_country %>% group_by(Year,c,chemistry) %>% 
  reframe(MWh=sum(MWh)) %>% ungroup() %>%  group_by(Year,c) %>% 
  mutate(chem_share_mwh=MWh/sum(MWh))
write.csv(bat_country_chem,"Inputs/Processed/battery_chem_country.csv",row.names = F)


# EoF
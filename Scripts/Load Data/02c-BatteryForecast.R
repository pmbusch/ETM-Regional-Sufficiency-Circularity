# Merge and Consolidate battery size data at country level
# Useful to have clear data on battery size used
# Adds battery chemistry scenarios as well
# PBH February 2024
# Updated March 2025
# Add Historical data as well
# Output: Battery size kwh/veh by chem, country, year

url_save <- "Inputs/%s.csv"

# LOAD DATA ---------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# for LDV by country 
bat_ldv <- read.csv("Inputs/Processed/battery_size_country.csv")

# 2015 to 2024
range(bat_ldv$Year)

# Battery Size  ---------

# missing countries
countries <- read_excel("Inputs/Dict/Dict_Countries_ICCT_EVV_MONET.xlsx",
                        sheet="Monet_to_Bench") %>% 
  filter(c!="Taiwan") %>% pull(c)

# expand list of countries to have data on battery size
countries[!(countries %in% unique(bat_ldv$c))]
# Venezuela and Iran

# use bat size of regions
bat_vene <- bat_ldv %>% filter(c=="Rest of South America") %>% mutate(c="Venezuela")
bat_iran <- bat_ldv %>% filter(c=="Rest of Middle East/Africa") %>% mutate(c="Iran")

bat_ldv <- rbind(bat_ldv,bat_vene,bat_iran); rm(bat_vene,bat_iran);


# Check
length(unique(bat_ldv$c)) # 64
bat_ldv$MWh <- bat_ldv$unit <- NULL  
bat_ldv <- bat_ldv %>% rename(kwh_veh=kwh_veh_total)

# save results for 2024
batsize <- bat_ldv %>% filter(Year==2024)
write.csv(batsize,sprintf(url_save,"bat_size"),row.names = F)

# Bat Size Over time ------------

# Scenarios to 45 and 90 kwh for cars
# Scenarios for all
years <- tibble(Year=2024:2050,dummy=1)
batSize_scen <- tibble(dummy=1,capacity_scenario=c("Baseline","Low Capacity","High Capacity"))
# scenario constant
bat_2050 <- batsize %>% mutate(dummy=1,Year=NULL) %>% 
  left_join(years) %>% 
  left_join(batSize_scen) %>% 
  mutate(dummy=NULL)

# Do Scenarios for BEV  based on desired kWh
goal_year <- 2035 # year that range/cap is achieved
cap_goals <- tibble(dummy=1,
                      capacity_scenario=c("Low Capacity","High Capacity"),
                      cap_goal=c(45,90))
                      
# slope
slope_bat <- batsize %>% mutate(dummy=1) %>% 
  left_join(cap_goals) %>% 
  mutate(slope=(cap_goal-kwh_veh)/(goal_year-2024)) %>% 
  dplyr::select(c,capacity_scenario,slope,cap_goal)

# linearly until 2035
bat_2050 <- bat_2050 %>% 
  left_join(slope_bat) %>% 
  rename(kwh_veh_2024=kwh_veh) %>% 
  mutate(kwh_veh=if_else(Year>goal_year,
                       cap_goal,
                       kwh_veh_2024+(Year-2024)*slope)) %>%
  mutate(kwh_veh=if_else(is.na(kwh_veh),kwh_veh_2024,kwh_veh)) %>% 
  dplyr::select(-slope,-kwh_veh_2024,-cap_goal)

# Figure at region level
bat_2050 %>% 
  group_by(c,Year,capacity_scenario) %>% 
  reframe(kwh_veh=mean(kwh_veh)) %>% ungroup() %>% # note that unit are valid for 2024
  filter(Year<2036) %>% 
  ggplot(aes(Year,kwh_veh,col=c,group = c))+
  geom_line()+
  facet_wrap(~capacity_scenario,ncol=1)+
  # scale_color_manual(values = region_colors) +
  labs(x="",y="Battery \n Capacity \n [kWh]")+
  coord_cartesian(expand = F)+
  theme(legend.text = element_text(size=6),
        legend.position = "none",
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))+
  scale_x_continuous(breaks = seq(2023,2035,2))+
  scale_y_continuous(breaks=c(20,40,60,80),limits = c(0,95))

ggsave("Figures/Inputs/batSize_scenarios.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

# Add historical bat size
range(bat_2050$Year)
bat_past <- bat_ldv %>% filter(Year<2024)
bat_2050 <- rbind(bat_2050,
                  mutate(bat_past,capacity_scenario="Baseline"),
                  mutate(bat_past,capacity_scenario="Low Capacity"),
                  mutate(bat_past,capacity_scenario="High Capacity"))
bat_2050 <- bat_2050 %>% arrange(Year,c)

write.csv(bat_2050,sprintf(url_save,"bat_size_forecast"),row.names = F)

# Chemistry ------------

# Use regional chemistry share forecasts, BY  scenarios
chem <- read.csv("Inputs/Processed/Chemistry_Scenarios/bat_share_2050_region.csv")
chem$chem_scenario <- "Baseline"
chem_LFP <- read.csv("Inputs/Processed/Chemistry_Scenarios/LFP_scen_region.csv")
chem_LFP$chem_scenario <- "Double LFP"
chem_NMC <- read.csv("Inputs/Processed/Chemistry_Scenarios/NMC811_scen_region.csv")
chem_NMC$chem_scenario <- "Double NMC 811"
chem_SS <- read.csv("Inputs/Processed/Chemistry_Scenarios/SolidState_scen_region.csv")
chem_SS$chem_scenario <- "Solid State adoption"
chem_SIB <- read.csv("Inputs/Processed/Chemistry_Scenarios/Sodium_scen_region.csv")
chem_SIB$chem_scenario <- "Sodium Battery adoption"

chem <- rbind(chem,chem_LFP,chem_NMC,chem_SS,chem_SIB);rm(chem_LFP,chem_NMC,chem_SS,chem_SIB)
chem <- chem %>% rename(Year=year) %>% mutate(bench_Region=NULL)

# Add historical chem share from EV Volumes
range(chem$Year)
historical_chem <- read.csv("Inputs/Processed/battery_country.csv")
range(historical_chem$Year)
unique(historical_chem$c)
historical_chem <- historical_chem %>% filter(Year<2024)
historical_chem <- historical_chem %>% dplyr::select(c,Year,chemistry,share_units)

unique(chem$chem_scenario)
chem <- rbind(chem,
              mutate(historical_chem,chem_scenario="Baseline"),
              mutate(historical_chem,chem_scenario="Double LFP"),
              mutate(historical_chem,chem_scenario="Double NMC 811"),
              mutate(historical_chem,chem_scenario="Solid State adoption"),
              mutate(historical_chem,chem_scenario="Sodium Battery adoption")) %>% 
  arrange(Year,c)

# expand list of countries to have data on battery size
countries[!(countries %in% unique(chem$c))]
# Venezuela and Iran

# use bat size of regions
chem_vene <- chem %>% filter(c=="Rest of South America") %>% mutate(c="Venezuela")
chem_iran <- chem %>% filter(c=="Rest of Middle East/Africa") %>% mutate(c="Iran")
chem <- rbind(chem,chem_vene,chem_iran); rm(chem_vene,chem_iran);

write.csv(chem,sprintf(url_save,"chem"),row.names = F)

# Add chemistry scenarios for LDV
bat_all <- bat_2050 %>%
  left_join(chem) %>% 
  mutate(kwh_veh=kwh_veh*share_units,
         share_units=NULL)


# Save data
unique(bat_all$capacity_scenario)
unique(bat_all$chem_scenario)
range(bat_all$Year)
bat_all %>% group_by(capacity_scenario,chem_scenario) %>% tally()

write.csv(bat_all,sprintf(url_save,"bat_size_chem_ldv"),row.names = F)

# EoF
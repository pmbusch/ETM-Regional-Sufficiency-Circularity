# Load and pre-process EV Chemistry demand forecast
# Data comes from Benchmark Mineral Intelligence
# PBH September 2023 
# Updated March 2024


# Libraries
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load data --------------
# Benchmark data - Not possible to share
df <- read_excel("Inputs/Cathode_Benchmark_Summary.xlsx",
                  sheet="Cathode_Forecast")
names(df) <- c("Region","chemistry_bmi","Unit",2024:2040)
head(df)

# Note: Share is for EV, ESS and portables aggregated, but EV are 88% of demand, so shares should be right

# Data Manipulation ----

# flat
df <- df %>% dplyr::select(-Unit) %>% 
  pivot_longer(c(-Region,-chemistry_bmi), names_to = "year", values_to = "MWh") %>% 
  mutate(year=as.numeric(year))

# equivalency of chemistries
dict_chem <- tibble(
  chemistry_bmi=c("LFP","NCM low nickel","NCM mid nickel","NCM high nickel",
              "NCA","LCO","4V Ni or Mn based","5V Mn based","Other"),
  chemistry=c("LFP","NMC 111","NMC 622","NMC 811","NCA","LCO",
              "LMO","LMNO","Other"))
dict_chem
df <- df %>% left_join(dict_chem)

# get share
df <- df %>% group_by(Region,year) %>% mutate(share=MWh/sum(MWh))

# filter chemistry
chems <- df %>% group_by(chemistry) %>% 
  reframe(share=sum(share)) %>% 
  filter(share>0.1) %>% pull(chemistry)


# figure
df %>% 
  filter(Region!="Global") %>%
  filter(chemistry!="Other") %>% 
  ggplot(aes(year,share,fill=chemistry))+
  geom_area()+
  facet_wrap(~Region)+
  scale_fill_viridis_d(option = "turbo")+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  labs(x="",y="LIB \n Market \n Share",fill="Chemistry",
       caption="Source: Benchmark Mineral Intelligence")

ggsave("Figures/Inputs/EV_Bench_Battery.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)


# Compare to EV Volumes 2024
evVol <- read.csv("Inputs/Processed/battery_world.csv")

evVol %>%  
  mutate(x=1) %>% 
  filter(Year==2024) %>% 
  ggplot(aes(x,y=share_units,fill=chemistry))+
  geom_col()+
  scale_fill_viridis_d(option = "turbo")+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x="EV Volumes 2024",y="")+
  theme(axis.text.x = element_blank())

# Forecasts -----
# Based on principle of proportional growth among countries

# equivalency of chemistry categories
(x=unique(df$chemistry))
(y=unique(evVol$chemistry))

match(x,y) # missing: LCO, LMNO, Other
match(y,x) # missing: NMC 721, NMCA 89:4:4:3 - add to NMC 811 growth; and 532 (add to 622 growth)

## extend to Benchmark to 2050 ------ 

# avg growth of last 5 years
avg5 <- df %>% 
  group_by(Region,chemistry) %>% 
  filter(year >= 2035 & year <= 2040) %>%
  summarise(x = mean((share / lag(share) - 1) * 100,na.rm=T)) 
avg5[avg5$chemistry=="LMNO",3] <- 0 # Do growth of LMNO as zero
avg5

# Fill in the dataframe with the projected values
df_aux <- df %>% filter(year==2040) # keeps last values
df_ext <- c()
for (y in 2041:2050) {
  df_aux <- df_aux %>% mutate(year=y)
  df_ext <- rbind(df_ext,df_aux)
}
df_ext <- df_ext %>%  
  left_join(avg5) %>% 
  mutate(share=share*(1 + x / 100)^(year-2040)) %>% 
  mutate(x=NULL)
# re-normalize to 1
df_ext <- df_ext %>% group_by(Region,year) %>% mutate(share=share/sum(share)) %>% ungroup()
df_ext %>% group_by(year) %>% reframe(sum(share)) # check

df_ext <- rbind(df,df_ext)

#figure again
df_fig <- df_ext %>% 
  filter(chemistry %in% chems) %>%
  filter(Region!="Global") %>% 
  mutate(proj=year>2040)

ggplot(df_fig,aes(year,share,fill=chemistry))+
  geom_area(aes(alpha=proj))+
  geom_area(data=filter(df_fig,year>2039,year<2042),alpha=0.7,linewidth=0.01)+ # fill missing gap behind
  facet_wrap(~Region)+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  scale_alpha_manual(values=c(1,0.7))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="",title="LIB Market Share",fill="Chemistry",
       caption = "Source: Benchmark Mineral Intelligence. \n 2040-2050 projection based on 2035-2040 avg. growth")

ggsave("Figures/Inputs/EV_Bench_Battery_ext.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)


## Project EV volumes 2024 using benchmark forecast by Region -----
# get proportional increase relative to 2024 for Benchmark 

bench2024 <- df_ext %>% ungroup %>% filter(year==2024) %>% 
  mutate(year=NULL,MWh=NULL,chemistry_bmi=NULL) %>% rename(b2024=share)

bench <- df_ext %>% 
  mutate(MWh=NULL,chemistry_bmi=NULL) %>% 
  left_join(bench2024) %>% 
  mutate(ratio2024=if_else(b2024==0,0,share/b2024)) %>% 
  rename(chem_eq=chemistry) %>% 
  rename(bench_Region=Region) %>% 
  mutate(share=NULL,b2024=NULL)

# PROJECT EV volumes share
evVol <- read.csv("Inputs/Processed/battery_world.csv")

range(bench$year)
evVol <- evVol %>% 
  filter(Year==2024) %>% 
  mutate(chem_eq=case_when(
    chemistry %in% c("NMC 721","NMCA 89:4:4:3")~"NMC 811",
    chemistry %in% c("NMC 532") ~ "NMC 622",
    T ~ chemistry)) %>% 
  left_join(filter(bench,bench_Region=="Global"),relationship = "many-to-many") %>% 
  mutate(share_units=share_units*ratio2024,Year=NULL,chem_eq=NULL,ratio2022=NULL,
         MWh=NULL,unit=NULL,kwh_veh=NULL,kwh_veh_total=NULL)

# normalize to 1
evVol <- evVol %>% group_by(year) %>% mutate(share_units=share_units/sum(share_units))

write.csv(evVol,"Inputs/Processed/Chemistry_Scenarios/bat_share_2050_world.csv",row.names = F)

evVol %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="EV \n Market \n Share",fill="Chemistry")

## Region wise - 65 countries/regions

evVol_region <- read.csv("Inputs/Processed/battery_country.csv")
dict_regs <-read_excel("Inputs/Dict/Dict_Countries_ICCT_EVV_MONET.xlsx",sheet="Monet_to_Bench")

evVol_region <- evVol_region %>% 
  filter(Year==2024) %>% 
  mutate(chem_eq=case_when(
    chemistry %in% c("NMC 721","NMCA 89:4:4:3")~"NMC 811",
    chemistry %in% c("NMC 532") ~ "NMC 622",
    T ~ chemistry)) %>% 
  left_join(dict_regs) %>% 
  left_join(bench,relationship = "many-to-many") %>% 
  mutate(share_units=share_units*ratio2024,Year=NULL,chem_eq=NULL,ratio2022=NULL,
         MWh=NULL,unit=NULL,kwh_veh=NULL,kwh_veh_total=NULL,ratio2024=NULL) %>%
  group_by(year,c) %>% mutate(share_units=share_units/sum(share_units))

evVol_region %>% group_by(year,c) %>% reframe(x=sum(share_units)) %>% arrange(desc(x))
write.csv(evVol_region,"Inputs/Processed/Chemistry_Scenarios/bat_share_2050_region.csv",row.names = F)

# BEV LIB chemistry forecast by major world region
key_regions <- c("United States","China","Brazil","Germany",
                 "India","Japan")
evVol_region %>% 
  filter(c %in% key_regions) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  facet_wrap(~c)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="",title="EV Market Share",fill="Chemistry")+
  theme(panel.spacing.x = unit(0.75, "cm"))

ggsave("Figures/Inputs/LIB_Chemistry_Region.png",ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

# Battery Chemistry Scenarios -----------
# Key idea: set target goals and achieve them proportionally through time

## LFP doubles ------------

# Make LFP twice as big by 2050, then maintain
exp_fact <- evVol_region %>% filter(year==2050,chemistry=="LFP") %>% ungroup() %>% 
  mutate(exp_factor=2*(1-share_units)/(1-share_units*2)) %>%  # expansion factor formula to scale x 2 once re-normalizing
  mutate(exp_factor=if_else(exp_factor<0|exp_factor>10,10,exp_factor)) %>%  # if negative, make really big to get 100 share
  mutate(chemistry=NULL,year=NULL,share_units=NULL)

# Expand to future years
range(evVol_region$year)
expansion_df <- expand.grid(c=unique(evVol_region$c),year=2024:2050) %>% 
  left_join(exp_fact) %>% 
  mutate(exp_factor=1+(exp_factor-1)/26*(year-2024)) %>%  # 26 years between 2024 to 2050
  ungroup() 

# add to original
evVol_LFP <- evVol_region %>% left_join(expansion_df) %>% 
  mutate(exp_factor=if_else(chemistry=="LFP",exp_factor,1)) %>% # expand only LFP
  mutate(share_units=share_units*exp_factor) %>% mutate(exp_factor=NULL) %>% 
  group_by(c,year) %>% mutate(share_units=share_units/sum(share_units))

# evVol_LFP %>% group_by(year) %>% reframe(sum(share_units))

write.csv(evVol_LFP,"Inputs/Processed/Chemistry_Scenarios/LFP_scen_region.csv",row.names = F)

evVol_LFP %>% 
  filter(c %in% key_regions) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  facet_wrap(~c)+
  coord_cartesian(expand = F)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="",title="EV Market Share",fill="Chemistry")

ggsave("Figures/Inputs/EV_Bench_Battery_AdjustedLFP.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

## NMC 811 Doubles --------------
# Make NMC twice as big by 2050

# Regional
exp_fact <- evVol_region %>% filter(year==2050,chemistry=="NMC 811") %>% ungroup() %>% 
  mutate(exp_factor=2*(1-share_units)/(1-share_units*2)) %>%  # expansion factor formula to scale x 2 once re-normalizing
  mutate(exp_factor=if_else(exp_factor<0|exp_factor>10,10,exp_factor)) %>%  # if negative, make really big to get 100 share
  mutate(chemistry=NULL,year=NULL,share_units=NULL)

# Expand to future years
expansion_df <- expand.grid(year=2024:2050,
                            c=unique(exp_fact$c)) %>% 
  left_join(exp_fact) %>% group_by(c) %>% 
  mutate(exp_factor=1+(exp_factor-1)/26*(year-2024)) %>% # 26 years between 2024 to 2050
  ungroup()

# add to original
evVol_NMC <- evVol_region %>% left_join(expansion_df) %>% 
  mutate(exp_factor=if_else(chemistry=="NMC 811",exp_factor,1)) %>% # expand only NMC
  mutate(share_units=share_units*exp_factor) %>% mutate(exp_factor=NULL) %>% 
  mutate(chemistry=if_else(chemistry=="NMC 811","NMC 811-Si",chemistry)) %>% 
  group_by(c,year) %>% mutate(share_units=share_units/sum(share_units))

# evVol_NMC %>% group_by(year) %>% reframe(sum(share_units))

write.csv(evVol_NMC,"Inputs/Processed/Chemistry_Scenarios/NMC811_scen_region.csv",row.names = F)

evVol_NMC %>% 
  filter(c %in% key_regions) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  facet_wrap(~c)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="",title="EV Market Share",fill="Chemistry")

ggsave("Figures/Inputs/EV_Bench_Battery_AdjustedNMC811.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)


## Solid State Adoption --------------
# Half of NMC goes towards solid state, same cathodes
# Electrolyte LPS: Li3PS4
# Anode: Li metal

# Start at 2030, reach half of share by 2040, then half for rest of period
share_difussion <- tibble(year=2024:2050,
       share_multiplier=c(rep(0,5),
                          seq(0,1/2,length.out=12),
                          rep(0.5,10)))

## regional
# Create new rows duplicate based on NMC - with only half of share
evVol_NMC <- evVol_region %>% filter(str_detect(chemistry,"NMC|NCA")) %>% 
  mutate(chemistry=paste0("SS ",chemistry)) %>% 
  left_join(share_difussion) %>% 
  mutate(share_units=share_units*share_multiplier, share_multiplier=NULL)

# add to original, but reduce to half
evVol_NMC <- evVol_region %>% 
  left_join(share_difussion) %>%
  mutate(share_units=if_else(str_detect(chemistry,"NMC|NCA"),
                             share_units*(1-share_multiplier),
                             share_units),share_multiplier=NULL) %>% 
  rbind(evVol_NMC) %>% 
  group_by(c,year) %>% mutate(share_units=share_units/sum(share_units))

write.csv(evVol_NMC,"Inputs/Processed/Chemistry_Scenarios/SolidState_scen_region.csv",row.names = F)

evVol_NMC %>% 
  filter(c %in% key_regions) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  facet_wrap(~c)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="",title="EV Market Share",fill="Chemistry")

ggsave("Figures/Inputs/EV_Bench_Battery_SolidState.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

## Sodium Batteries Adoption --------------
# Half of LFP goes towards Sodium based battery
# Electrolyte based on sodium: NaPF6
# Cathode active material: NaCu(1/3)Fe(1/3)Mn(1/3)O2
# Anode:	Hard Carbon

# Start at 2030, reach half of share by 2040, then half for rest of period
share_difussion <- tibble(year=2024:2050,
                          share_multiplier=c(rep(0,5),
                                             seq(0,1/2,length.out=12),
                                             rep(0.5,10)))
## regional
# Create new rows duplicate based on NMC - with only half of share
evVol_NMC <- evVol_region %>% filter(str_detect(chemistry,"LFP")) %>% 
  mutate(chemistry="SIB") %>% 
  left_join(share_difussion) %>% 
  mutate(share_units=share_units*share_multiplier, share_multiplier=NULL)

# add to original, but reduce to half
evVol_NMC <- evVol_region %>% 
  left_join(share_difussion) %>%
  mutate(share_units=if_else(str_detect(chemistry,"LFP"),
                             share_units*(1-share_multiplier),
                             share_units),share_multiplier=NULL) %>% 
  rbind(evVol_NMC) %>% 
  group_by(c,year) %>% mutate(share_units=share_units/sum(share_units))

write.csv(evVol_NMC,"Inputs/Processed/Chemistry_Scenarios/Sodium_scen_region.csv",row.names = F)

evVol_NMC %>% 
  filter(c %in% key_regions) %>% 
  ggplot(aes(year,share_units,fill=chemistry))+
  geom_area()+
  coord_cartesian(expand = F)+
  facet_wrap(~c)+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  guides(alpha=F)+
  labs(x="",y="",title="EV Market Share",fill="Chemistry")

ggsave("Figures/Inputs/EV_Bench_Battery_Sodium.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)


# EoF 
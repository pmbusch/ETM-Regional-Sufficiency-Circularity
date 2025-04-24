# Battery Size Figures
# Data: EV Volumes

library(tidyverse)
source("Scripts/00-Libraries.R", encoding = "UTF-8")
bat_region <- read.csv("Inputs/Processed/battery_country.csv")
bat_region_size <- read.csv("Inputs/Processed/battery_size_country.csv")

# FIGURES ----
theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))


# Bat size
bat_region_size %>% 
  filter(Year==2024) %>%
  mutate(lab_kwh=paste0(round(kwh_veh_total,0),"")) %>% 
  ggplot(aes(reorder(c,kwh_veh_total),kwh_veh_total))+
  geom_col(fill="brown")+
  geom_text(aes(label=lab_kwh),nudge_y = 3,size=20*5/14 * 0.8)+
  coord_flip(expand = T)+
  labs(x="",y="Average Battery Size per BEV [kWh]")

## Chemistry stacked -----
chem_level <- c("LFP","LMO","NCA","NMC 111","NMC 532",
                "NMC 622","NMC 721","NMC 811","NMCA 89:4:4:3")
eq_region <- readxl::read_excel("Inputs/Eq_Countries_ICCT_EVV.xlsx",sheet="Eq_Region")

bat_region %>% 
  filter(Year=="2024") %>% 
  left_join(dict_region,by="c") %>% 
  group_by(Region_EV,chemistry) %>% 
  reframe(MWh=sum(MWh),unit=sum(unit)) %>% ungroup() %>% 
  group_by(Region_EV) %>% 
  mutate(kwh_veh_total=sum(MWh)*1e3/sum(unit),
         kwh_veh=MWh*1e3/sum(unit)) %>% ungroup() %>% 
  mutate(chemistry=factor(chemistry,levels=chem_level)) %>%
  ggplot(aes(reorder(Region_EV,kwh_veh_total),kwh_veh,fill=fct_rev(chemistry)))+
  geom_col(position = "stack",col="black",linewidth=0.1)+
  coord_flip(expand = F)+
  labs(x="",y="kWh Battery Capacity per BEV Vehicle",fill="Battery \nChemistry \nShare")+
  scale_fill_viridis_d(option="turbo",direction = -1)+
  theme_bw(10)+ 
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=8))+
  guides(fill = guide_legend(reverse = T,byrow = T))

ggsave("Figures/Inputs/2024_BEV_LIB_Size.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

# EoF
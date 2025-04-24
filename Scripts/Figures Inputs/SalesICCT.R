# Figure ICCT Sales
# PBH July  2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

icct <- read.csv("Inputs/EV_Sales.csv")


theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))

order_region <- names(region_colors)

icct %>% 
  left_join(dict_region,by="c") %>% 
  group_by(Region_EV,Year) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% 
  mutate(Region_EV=factor(Region_EV,levels=rev(order_region))) %>% 
  ggplot(aes(Year,Sales,fill=Region_EV))+
  geom_area()+
  coord_cartesian(expand = F)+
  scale_fill_manual(values = region_colors)+
  scale_x_continuous(breaks = c(2015,2024, 2030, 2040, 2050))+
  labs(x="",y="",title="Car EV Sales [million units]",fill="")+
  theme(legend.position = c(0.1,0.6),
        legend.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(hjust = 1))

ggsave("Figures/Inputs/ICCTSales.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=8.7)

# Weird shape by country, original ICCT Data
ggplot(icct,aes(Year,Sales))+
  geom_line()+
  facet_wrap(~c,scales="free_y")

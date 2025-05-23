# Figure for Mineral Intensity
# Source of data: Default values of Battery 1. BatPac 5.1
# PBH September 2023

source("Scripts/00-Libraries.R", encoding = "UTF-8")
mineral <- read_excel("Inputs/Mineral_Intensity_2025.xlsx",sheet = "Study_Scope")

# Choose only selected chemistries
mineral <- mineral %>% 
  filter(!str_detect(chemistry,"SS")|str_detect(chemistry,"622")) %>% 
  filter(!str_detect(chemistry,"Si")|str_detect(chemistry,"811"))
# filter(!str_detect(chemistry,"SIB"))

min_interest <- min_interest[-5] # remove kWh

# Factor
mineral <- mineral %>% 
  mutate(chemistry=factor(chemistry,levels=rev(unique(mineral$chemistry)))) %>% 
  filter(Mineral %in% min_interest) %>% 
  mutate(Mineral=factor(Mineral,rev(min_interest)))


# Figure
mineral %>% 
  filter(chemistry!="LMO-LTO") %>%
  filter(chemistry!="NMC 95") %>% 
  filter(chemistry!="LMNO") %>% 
  ggplot(aes(chemistry,kg_per_kwh,fill=Mineral))+
  geom_col(position = "dodge")+
  # ggforce::facet_col(facets = vars(type), 
  #                    scales = "free_y", 
  #                    space = "free") +
  # 
  
  # facet_grid(type~.,scales = "free",space="free")+
  coord_flip(expand = F,ylim = c(0,0.8))+
  scale_fill_manual(values=c("Lithium" = "darkblue", "Nickel" = "darkgreen", "Cobalt" = "darkred",
                             "Graphite" = "darkgrey","Phosphorus"="#B8860B"))+
  labs(x="",y="Mineral Intensity [kg per kWh]",fill="Mineral")+
  scale_y_continuous(breaks=seq(0,0.8,0.1))+
  geom_vline(xintercept = seq(0.5,nrow(mineral)+0.5,1),col="lightgray",
             linetype="dashed",linewidth=0.2)+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(
    # legend.position = c(0.9,0.8),
    # strip.background = element_rect(margin = margin(0, 0, 0.1, 0)),
    strip.text = element_text(angle=0,size=10,margin = margin()))


ggsave("Figures/Inputs/MinIntensity.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=20,height=16)

# EoF
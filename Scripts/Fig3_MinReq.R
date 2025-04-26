# Analysis of Mineral Demand Results -----
# Mineral Budget under different scenarios and regional aggregation
# PBH January 2025

# Libraries ----
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/Load Data/05-Load_USGS.R", encoding = "UTF-8")
dict_scen <- read_excel("Inputs/Dict/Dict_Scenario.xlsx")
scen_order <- dict_scen %>% filter(demand_scen==1) %>% pull(name)
theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))

# Load Results -----------

# Load from folder results
url_results <- "Results/Mineral Demand/"
runs <- list.files(url_results,recursive = T)
(runs <- runs[!str_detect(runs,"No Trade|Circular")])

# Read all
df <- do.call(rbind, lapply(runs, function(path) 
  transform(read.csv(file.path(url_results, path)) %>% mutate(X=NULL), 
            file_name = str_remove(path,"\\.csv"))))
unique(df$file_name)

df <- df %>% filter(Year>2023) # 2024 to 2050

sectors_order <- c("EV Production","LIB Replacement","Recycling")
# Add scenario names
df <- df %>% 
  left_join(dict_scen) %>% 
  mutate(Sector=case_when(
    Sector=="EV_Production" ~ sectors_order[1],
    Sector=="add_LIB" ~ sectors_order[2],
    Sector=="LIB_recycling" ~ sectors_order[3],
    T ~ "NA"))

# Remove unwanted scenarios
df %>% group_by(demand_scen,name) %>% tally()
df <- df %>% filter(demand_scen==1) 

# remove battery needs
df <- df %>% filter(Mineral!="kWh")

# Mineral Budget Estimation ----------

## Country Level ----
unique(df$name)
unique(df$Sector)

iso_order <- unique(dict_region$ISO)
# ISO codes to aggregate to REST OF ... - why? no much demand or reserves
agg_iso <- c("AUT","BEL","DNK","FIN","GRC","IRL","NLD","SWE","CHE","WEU",
             "BOL","COL","PER","ERY","NZL","SGP","EGY","ISR","SAU","ARE",
             "ETH","KEN","NGA","TWN",
             "VEN","ITA","HUN","POL","PRT","NOR","URY","DZA","MAR","ZAF","ROU")

data_fig_country <- df %>% 
  left_join(dict_region) %>% mutate(c=ISO) %>%
  mutate(c=if_else(c %in% agg_iso,ISO_Region,c)) %>% # aggregate
  group_by(category,name,c,Mineral) %>%
  reframe(mtons=sum(tons)/1e6) %>% ungroup() %>% 
  mutate(region_agg="Country")

                       
## Region ----
order_region <- dict_region %>% mutate(ISO=factor(ISO,levels=iso_order)) %>% 
  pull(Region_EV) %>% unique()
# order_region <- order_region %>% str_replace(" ","\n")

data_fig_region <- df %>% 
  left_join(dict_region) %>% mutate(c=Region_EV) %>%
  group_by(category,name,c,Mineral) %>%
  reframe(mtons=sum(tons)/1e6) %>% ungroup() %>% 
  mutate(region_agg="Region")
  # mutate(c=str_replace(c," ","\n"))

## World and aggregate -----
data_fig_world <- df %>% 
  group_by(category,name,Mineral) %>%
  reframe(mtons=sum(tons)/1e6) %>% ungroup() %>% 
  mutate(region_agg="W",c="World")

c_order <- c("World",order_region,iso_order)

#bind
data_fig <- rbind(data_fig_country,data_fig_region,data_fig_world)

data_fig <- data_fig %>% 
  mutate(name=factor(name,levels=scen_order)) %>% 
  mutate(c=factor(c,levels=rev(c_order))) %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  mutate(region_agg=factor(region_agg,levels = c("W","Region","Country")))

# get ranges by Demand scenario
unique(data_fig$category)
data_fig <- data_fig %>% 
  group_by(region_agg,category,Mineral,c) %>% 
  reframe(minTons=min(mtons),maxTons=max(mtons),mtons=mean(mtons),n=n())


## USGS -----

# USGS data
head(usgs)
usgs_country <- usgs %>% group_by(c,Mineral) %>% 
  reframe(mtons=sum(mtons,na.rm=T)) %>% ungroup() %>% 
  filter(Mineral %in% min_interest)
usgs_country <- dict_region %>% 
  left_join(usgs_country,by="c")
usgs_country <- usgs_country %>%
  mutate(c=ISO) %>%
  mutate(c=if_else(c %in% agg_iso,ISO_Region,c)) %>% # aggregate
  dplyr::select(c,Mineral,mtons) %>% 
  # complete(c,Mineral, fill = list(mtons=0)) %>% 
  filter(mtons>0) %>% 
  filter(!is.na(Mineral)) %>% 
  mutate(region_agg="Country")


# bind region and world
usgs_world <- usgs_world %>% dplyr::select(c,Mineral,mtons) %>% 
  mutate(region_agg="W")
usgs_region <- usgs %>% filter(!is.na(Region_EV)) %>% 
  group_by(Region_EV,Mineral) %>% 
  reframe(mtons=sum(mtons,na.rm=T)) %>% 
  rename(c=Region_EV) %>% mutate(region_agg="Region") 
  # mutate(c=str_replace(c," ","\n"))

usgs_country <- rbind(usgs_country,usgs_region,usgs_world)

usgs_country <- usgs_country %>% 
  filter(mtons>0) %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  mutate(c=factor(c,levels=rev(c_order))) %>% 
  mutate(region_agg=factor(region_agg,levels = c("W","Region","Country")))


## Other Plots annotations ----

# annotations
pos_rel <- 7
ann_text <- data.frame(Mineral = factor("Lithium",levels=min_interest),
                       region_agg=factor("Region",levels=c("W","Region","Country")),
                       mtons=rep(5.2,4),
                       x=pos_rel+c(0.9,0.3,-0.3,-0.9),
                       label=c("LIB Capacity","LIB Chemistry",
                               "LIB & EV Lifetime","Recycling"))
ann_rese <- data.frame(Mineral = factor("Lithium",levels=min_interest),
                       region_agg=factor("Region",levels=c("W","Region","Country")),
                       mtons=7.5,x=pos_rel-6,label="Reserves")

# mark countries/regions with enough reserves (just ref case)
data_fig <- data_fig %>% left_join(rename(usgs_country,reserve=mtons)) %>% 
  mutate(reserve=if_else(is.na(reserve),0,reserve)) %>% 
  mutate(sufficiency=case_when(
    reserve==0 ~ "No Reserves",
    reserve>mtons ~ "Sufficient",
    T ~ "Insufficient") %>% factor(levels=c("Sufficient","Insufficient","No Reserves"))) 

# order by mineral demand - Lithium
c_order2 <- data_fig %>% filter(Mineral=="Lithium") %>% 
  filter(category=="Reference") %>% 
  # mutate(deficit=mtons-reserve) %>% 
  # arrange(desc(deficit)) %>% 
  arrange((mtons)) %>% # sort by requirement
  pull(c)
data_fig <- data_fig %>% mutate(c=factor(c,levels=c_order2))


# Figure -----

a=data_fig;b=usgs_country # Save 
# data_fig=a;usgs_country=b # restore

# just plot lithium
# data_fig=data_fig %>% filter(Mineral %in% c("Lithium"))
# usgs_country=usgs_country %>% filter(Mineral %in% c("Lithium"))
# ann_text$Mineral="Lithium"

ggplot(data_fig,aes(c,mtons))+
  geom_col(data=filter(data_fig,category=="Reference"),aes(fill=sufficiency))+
  # Segments by scenarios
  geom_segment(data=filter(data_fig,category=="LIB Capacity"),
               aes(y = minTons,yend=maxTons),col="darkred",
               linewidth = 0.5,position = position_nudge(x=0.4))+
  geom_segment(data=filter(data_fig,category=="LIB Chemistry"),
               aes(y = minTons,yend=maxTons),col="darkblue",
               linewidth = 0.5,position = position_nudge(x=0.2))+
  geom_segment(data=filter(data_fig,category=="LIB & EV Lifetime"),
               aes(y = minTons,yend=maxTons),col="darkorange",
               linewidth = 0.5,position = position_nudge(x=-0.2))+
  geom_segment(data=filter(data_fig,category=="Recycling"),
               aes(y = minTons,yend=maxTons),col="#4CAF50",
               linewidth = 0.5,position = position_nudge(x=-0.4))+
  geom_text(data = ann_text,aes(label = label,col=label,x=x),angle=0,
            size=9*5/14 * 0.8,hjust=0)+
  geom_text(data = ann_rese,aes(label = label,col=label,x=x),angle=0,
            size=9*5/14 * 0.8,hjust=0)+
  geom_point(data=usgs_country,size=0.5)+
  scale_color_manual(values=c("LIB Capacity"="darkred","LIB Chemistry"="darkblue",
                              "LIB & EV Lifetime"="darkorange","Recycling"="#4CAF50",
                              "Reserves"="black"))+
  scale_fill_manual(values=c("Sufficient"="#505050","Insufficient"="#888888",
                             "No Reserves"="#B0B0B0"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  coord_flip()+
  ggh4x::facet_grid2(region_agg~ Mineral,scales = "free", axes="margins",
                     independent = "x",space = "free_y")+
  labs(x="",y="",title="Mineral Requirements 2024-2050 [million tons]",col="Demand \nScenario",
       fill="Reserve\nsufficiency")+
  guides(color = "none")+
  theme_bw(9)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=8,hjust=1,vjust=1,lineheight = 0.7),
        strip.background = element_blank(),
        strip.text=element_text(size=9),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.92,0.56))

ggsave("Figures/MineralBudget.png", 
       ggplot2::last_plot(),units="cm",dpi=1200,width=17.8,height=8.7*2)

pdf("Figures/pdf/Fig3.pdf",width=17.8/2.54,height=8.7*2/2.54)
ggplot2::last_plot()
dev.off()
# Only Lithium - uncomment
# ggsave("Figures/MineralBudgetLi.png", 
#        ggplot2::last_plot(),units="cm",dpi=600,width=8.7*2,height=8.7)

## Graphical Abstract Figure -----------

data_fig2 <- data_fig %>% 
  filter(Mineral=="Lithium",category=="Reference") %>% 
  filter(c=="World")

data_fig2 %>% 
  pivot_longer(c(mtons,reserve), names_to = "key", values_to = "value") %>% 
  mutate(key=factor(key,levels=c("reserve","mtons"))) %>% 
  ggplot(aes(key,value,fill=key))+
  geom_col(width = 0.95,col="black",linewidth=0.1)+
  scale_fill_manual(values = c("mtons" = "darkgrey", "reserve" = "#6a3d9a"))+
  labs(x="",y="")+
  coord_flip(expand = F)+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        legend.position = "none")

ggsave("Figures/Graphical/World_li.png",
       units="cm",dpi=600,
       height = 2,width = 2)

# Same but for countries
data_fig2 <- data_fig %>% 
  filter(Mineral=="Lithium",category=="Reference") %>% 
  filter(c %in% c("CHN","USA","JPN","AUS"))

data_fig2 %>% 
  pivot_longer(c(mtons,reserve), names_to = "key", values_to = "value") %>% 
  mutate(key=if_else(key=="mtons",c,key)) %>% 
  mutate(key=factor(key,levels=c("reserve","CHN","USA","JPN","AUS"))) %>%
  ggplot(aes(c,value,fill=key))+
  geom_col(width = 0.7,position = position_dodge(),col="black",linewidth=0.1)+
  scale_fill_manual(values = c("CHN" = "#ff0000", 
                               "USA"="#1f78b4",
                               "JPN"="#fdb462",
                               "AUS"="#cab2d6",
                               "reserve" = "#6a3d9a"))+
  labs(x="",y="")+
  coord_flip(expand = F)+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        legend.position = "none")

ggsave("Figures/Graphical/Countries_li.png",
       units="cm",dpi=600,
       height = 2,width = 2)



# EoF
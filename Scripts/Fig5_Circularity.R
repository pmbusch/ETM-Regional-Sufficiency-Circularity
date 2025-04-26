# Analysis of Mineral Demand Results -----
# Circularity potential under differeny spatial aggregation and scenarios
# PBH January 2025

# Libraries ----
source("Scripts/00-Libraries.R", encoding = "UTF-8")
dict_scen <- read_excel("Inputs/Dict/Dict_Scenario.xlsx")
scen_order <- dict_scen %>% filter(demand_scen==1) %>% pull(name)
theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))

# Load Results -----------

# Load from folder results
url_results <- "Results/Mineral Demand/"
runs <- list.files(url_results,recursive = T)
(runs <- runs[str_detect(runs,"Circular")]) # circular scenarios

# Read all
df <- do.call(rbind, lapply(runs, function(path) 
  transform(read.csv(file.path(url_results, path)) %>% mutate(X=NULL), 
            file_name = str_remove(path,"\\.csv"))))
unique(df$file_name)

df <- df %>% filter(Year>2023) # 2024 to 2050

sectors_order <- c("EV Production","LIB Replacement","Recycling")
# Add scenario names
df <- df %>% 
  mutate(file_name=str_remove_all(file_name,"Circular/")) %>%
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

unique(df$name)

# Circularity Potential Estimation ----------

target_yr <- 2050
max_recovery <- 0.95 # 95% recovery under ideal conditions

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
  filter(Year==target_yr) %>% 
  left_join(dict_region) %>% mutate(c=ISO) %>%
  mutate(c=if_else(c %in% agg_iso,ISO_Region,c)) %>% # aggregate
  group_by(category,name,c,Mineral,Sector) %>%
  reframe(mtons=sum(tons)/1e6) %>% ungroup() %>% 
  pivot_wider(names_from = Sector, values_from = mtons,values_fill = 0) %>% 
  mutate(circularity_potential=(-Recycling*max_recovery)/(`EV Production`+`LIB Replacement`)) %>%
  mutate(label_cp=paste0(round(circularity_potential*100,0),"%")) %>% 
  mutate(region_agg="Country")


## Region ----
order_region <- dict_region %>% mutate(ISO=factor(ISO,levels=iso_order)) %>% 
  pull(Region_EV) %>% unique()
order_region <- order_region %>% str_replace(" ","\n")

data_fig_region <- df %>% 
  filter(Year==target_yr) %>% 
  left_join(dict_region) %>% mutate(c=Region_EV) %>%
  group_by(category,name,c,Mineral,Sector) %>%
  reframe(mtons=sum(tons)/1e6) %>% ungroup() %>% 
  pivot_wider(names_from = Sector, values_from = mtons,values_fill = 0) %>% 
  mutate(circularity_potential=(-Recycling)/(`EV Production`+`LIB Replacement`)) %>%
  mutate(label_cp=paste0(round(circularity_potential*100,0),"%")) %>% 
  mutate(region_agg="Region") %>% 
  mutate(c=str_replace(c," ","\n"))

## World and aggregate -----
data_fig_world <- df %>% 
  filter(Year==target_yr) %>% 
  group_by(category,name,Mineral,Sector) %>%
  reframe(mtons=sum(tons)/1e6) %>% ungroup() %>% 
  pivot_wider(names_from = Sector, values_from = mtons,values_fill = 0) %>% 
  mutate(circularity_potential=(-Recycling)/(`EV Production`+`LIB Replacement`)) %>%
  mutate(label_cp=paste0(round(circularity_potential*100,0),"%")) %>% 
  mutate(region_agg="W",c="World")

c_order <- c("World",order_region,iso_order)

#bind
data_fig <- rbind(data_fig_country,data_fig_region,data_fig_world)


data_fig <- data_fig %>% 
  mutate(c=factor(c,levels=rev(c_order))) %>% 
  mutate(Mineral=factor(Mineral,levels=min_interest)) %>% 
  mutate(region_agg=factor(region_agg,levels = c("W","Region","Country")))

data_Notrade <- data_fig %>% filter(name=="No Trade")

data_fig <- data_fig %>% 
  filter(name!="No Trade") %>% 
  mutate(name=factor(name,levels=scen_order))

# limit to >100%
data_fig <- data_fig %>% 
  mutate(circularity_potential2=if_else(circularity_potential>1,1,circularity_potential))

# Barplot ------

unique(data_fig$category)
data_fig2 <- data_fig %>% 
  group_by(region_agg,category,Mineral,c) %>% 
  reframe(minCP=min(circularity_potential),maxCP=max(circularity_potential),
          cp=mean(circularity_potential),n=n()) %>% 
  mutate(c=factor(c,levels=c_order))

# annotations 
pos_rel <- 3
ann_text <- data.frame(region_agg=factor(rep("Region",5),
                                         levels=c("W","Region","Country")),
                       cp=c(rep(0.46,3),rep(0.43,2)),
                       x=pos_rel+c(-0.45,-0.3,-0.15,0.2,0.4),
                       label=c("LIB Capacity","LIB Chemistry",
                               "LIB & EV Life","New Veh Trade","Used Veh Trade"))
ann_trade <- data.frame(region_agg=factor("Country",levels=c("W","Region","Country")),
                        cp=0.45,x=pos_rel+2-c(2.2,1.8),label=c("Trade","Omission"))


# mark countries/regions with >100% CP (just ref case)
data_fig2 <- data_fig2 %>% 
  mutate(above1=if_else(cp>=1,">100%"," ") %>% 
           factor(levels=c(">100%"," ")),
         label_cp=if_else(cp>=1,paste0("",round(cp*100,0),"%"),"")) 

# get CP reference with No Trade
data_Notrade <- data_Notrade %>% rename(cp=circularity_potential)


# Export and production share - original ----
supply.stats <- read_excel("Inputs/Trade Ratios/2023_MONET_tstats_sharematrix.xlsx",
                           sheet="2023.tstats")
head(supply.stats)
supply.stats <- supply.stats %>% 
  mutate(c=c %>% str_replace("Asia/oceania","Asia/Oceania")) %>% 
  left_join(dict_region) %>%
  # share of production that goes to export (domestic export, no re-exports)
  mutate(share=if_else(p>0,(de-fs)/p,0)) %>%  
  arrange(desc(share)) %>% 
  rename(iso=ISO) %>% 
  filter(iso %in% unique(data_fig2$c)) # only highlight countries 

order_c <- supply.stats %>% pull(iso) %>% unique()
order_r <- supply.stats %>% pull(Region_EV) %>% unique()
order_r <- order_r %>% str_replace(" ","\n")

# figure
p2 <- supply.stats %>%
  mutate(c=factor(iso,levels=order_c)) %>% 
  ggplot(aes(c,share,group=1))+
  geom_area(fill="darkgrey",col="darkgrey")+
  # geom_col()+
  annotate(geom="text",x=15,y=0.8,label="2023 Net Vehicle Exports over Production",
           size=9*5/14 * 0.8)+
  coord_cartesian(expand = F,ylim = c(-1,1))+
  labs(x="",y="",tag="(b)")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw(9)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.tag = element_text(face = "bold"),
        axis.text.y = element_text(size=5),
        axis.text.x = element_text(size=5,angle=60,hjust=1,vjust=1,lineheight = 0.7),
        strip.background = element_blank(),
        strip.text=element_text(size=8))
# p2

# change order
c_order <- c("World",order_r,order_c)
data_fig2 <- data_fig2 %>% mutate(c=factor(c,levels=c_order))


# loop by mineral
for (i in unique(data_fig2$Mineral)){
  
  i <- "Lithium" # debug

  # Filter by mineral
  data_Notrade_min <- data_Notrade %>% filter(Mineral==i)
  data_fig2_min <- data_fig2 %>% filter(Mineral==i)
  
  # Barplot
  p1 <- ggplot(data_fig2_min,aes(c,cp))+
    geom_col(data=filter(data_fig2_min,category=="Reference"),aes(fill=above1))+
    # Segments by scenarios
    geom_segment(data=filter(data_fig2_min,category=="LIB Capacity"),
                 aes(y = minCP,yend=maxCP),col="darkred",
                 linewidth = 0.5,position = position_nudge(x=-0.45))+
    geom_segment(data=filter(data_fig2_min,category=="LIB Chemistry"),
                 aes(y = minCP,yend=maxCP),col="darkblue",
                 linewidth = 0.5,position = position_nudge(x=-0.3))+
    geom_segment(data=filter(data_fig2_min,category=="LIB & EV Lifetime"),
                 aes(y = minCP,yend=maxCP),col="darkorange",
                 linewidth = 0.5,position = position_nudge(x=-0.15))+
    geom_segment(data=filter(data_fig2_min,category=="New Vehicle Trade"),
                 aes(y = minCP,yend=maxCP),col="#008080",
                 linewidth = 0.5,position = position_nudge(x=0.2))+
    geom_segment(data=filter(data_fig2_min,category=="Used Vehicle Trade"),
                 aes(y = minCP,yend=maxCP),col="darkviolet",
                 linewidth = 0.5,position = position_nudge(x=0.4))+
    geom_text(data = ann_text,aes(label = label,col=label,x=x),angle=90,
              size=7*5/14 * 0.8,hjust=0)+
    geom_text(data = ann_trade,aes(label = label,col=label,x=x),angle=90,
              size=8*5/14 * 0.8,hjust=0,col="black")+
    geom_text(data=filter(data_fig2_min,category=="Reference"),aes(label=label_cp),
              y=0.97,fontface = "italic",size=6*5/14 * 0.8,col="white")+
    geom_point(data=data_Notrade_min,size=0.5)+
    # ggh4x::facet_grid2(. ~ region_agg,scales = "free", axes="all",
    # independent = "y",space = "free_x")+
    ggh4x::facet_manual(~region_agg,scales = "free",widths = c(1,4,5),
                        design=matrix(c(1,2,3,3), 2, 2,byrow = T))+
    coord_cartesian(expand=F,ylim=c(0,1))+
    scale_y_continuous(labels=scales::percent)+
    scale_color_manual(values=c("LIB Capacity"="darkred","LIB Chemistry"="darkblue",
                                "LIB & EV Life"="darkorange","New Veh Trade"="#008080","Used Veh Trade"="darkviolet"))+
    scale_fill_manual(values=c(">100%"="#444444"," "="#888888"))+
    labs(x="",y="",title=paste0(i," ",target_yr," Circularity Potential"),col="Demand \nScenario",
         fill="",tag="(a)")+
    guides(color = "none",
           fill=guide_legend(override.aes = list(fill = c("#444444","white"))))+
    theme_bw(9)+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          axis.text.y = element_text(size=5),
          plot.tag = element_text(face = "bold"),
          axis.text.x = element_text(size=5,angle=60,hjust=1,vjust=1,lineheight = 0.7),
          strip.background = element_blank(),
          strip.text=element_text(size=9),
          legend.text = element_text(size=8),
          legend.key.height= unit(0.25, 'cm'),
          legend.key.width= unit(0.25, 'cm'),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.position = c(0.28,0.975))
  p1
  
  # merge figures
  cowplot::plot_grid(p1,p2,ncol=1,rel_heights = c(8,2))
  
  ggsave(paste0("Figures/CircularityPotential_",i,".png"), 
         ggplot2::last_plot(),units="cm",dpi=600,width=17.8,height=12)
  
  ggsave(paste0("Figures/pdf/fig5",i,".pdf"),width=17.8/2.54,height=12/2.54)
  ggplot2::last_plot()
  dev.off()
  
  
}


# Graphical abstract

data_99 <- data_fig2 %>% 
  filter(Mineral=="Lithium",category=="Reference") %>% 
  filter(c=="World")

# Barplot
data_99
ggplot(data_99,aes(c,cp,fill=c))+
  geom_col(col="black",linewidth=0.1)+
  scale_fill_manual(values = c("World" = "#cab2d6"))+
  labs(x="",y="")+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels=scales::percent,limits = c(0,1))+
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        legend.position = "none")

ggsave("Figures/Graphical/World_circularity.png",
       units="cm",dpi=600,
       height = 5,width = 2)

# for selected countries
sel <- c("CHN","USA","JPN","MEX","DEU")
data_99 <- data_fig2 %>% 
  filter(Mineral=="Lithium",category=="Reference") %>% 
  filter(c %in% sel)

# Barplot
data_99
for (s in sel){
  data_99 %>% 
    filter(c==s) %>% 
    ggplot(aes(c,cp,fill=c))+
    geom_col(col="black",linewidth=0.1)+
    scale_fill_manual(values = c("CHN" = "#ff0000", 
                                   "USA"="#1f78b4",
                                   "JPN"="#fdb462",
                                   "MEX"="#b2df8a",
                                   "DEU"="#a6cee3"))+
    labs(x="",y="")+
    coord_cartesian(expand = F)+
    scale_y_continuous(labels=scales::percent,limits = c(0,1))+
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text=element_blank(),
          legend.position = "none")
  
  ggsave(paste0("Figures/Graphical/",s,"_circularity.png"),
         units="cm",dpi=600,
         height = 2.5,width = 1)
}

  
  
  # Heatmap -----
head(data_fig)
# data_fig <- data_fig %>% mutate(name=substr(name,0,6))
# data_fig <- data_fig %>% filter(Mineral=="Lithium")
ggplot(data_fig,aes(c,name,fill=circularity_potential2))+
  geom_tile(col="black")+
  geom_text(aes(label=label_cp),size=5*5/14 * 0.8)+
  # ggh4x::facet_grid2(Mineral~region_agg,scales = "free",axes = "y",
  #                    independent = "y",space = "free_x")+
  facet_grid(category~region_agg,space="free",scales="free")+
  coord_cartesian(expand = F)+
  scale_fill_continuous(labels=scales::percent,
                        low = "white",high = "darkgreen")+  
  labs(x="",y="",title=paste0(target_yr," Circularity Potential"),
       col="Demand \nScenario",fill="Circularity Potential")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=5),
        # axis.text.x = element_text(size=5),
        axis.text.x = element_text(size=5,angle=60,hjust=1,vjust=1,lineheight = 0.7),
        strip.background = element_blank(),
        strip.text=element_text(size=8),
        legend.text = element_text(size=6))

ggsave("Figures/CircularityPotential2.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*2,height=8.7*2)


# EoF
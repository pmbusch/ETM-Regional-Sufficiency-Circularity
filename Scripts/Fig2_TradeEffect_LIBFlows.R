# Analysis of Mineral Demand Results -----
# Figure to Show the Trade Effect
# PBH January 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
dict_scen <- read_excel("Inputs/Dict/Dict_Scenario.xlsx")
scen_order <- dict_scen %>% filter(demand_scen==1) %>% pull(name)

# Load Common Results -----------

# Load from folder results
url_results <- "Results/Mineral Demand/"
runs <- list.files(url_results,recursive = F)
runs <- runs[str_detect(runs,"csv")]
# remove unwanted scenarios for the figure
# trade does not matter at global level
# chemistry does not matter for battery flows
(runs <- runs[str_detect(runs,"Reference|Lifetime|LIB|recycling")])
runs <- c(runs,
          "No Trade/Reference.csv", # Fig a - Trade Effect
          "Circular/Reference.csv") # Fig  c - LIB outflow availability


# Read all - still fast enough
df <- do.call(rbind, lapply(runs, function(path) 
  transform(read.csv(file.path(url_results, path)), 
            file_name = str_remove(path,"\\.csv")) %>% mutate(X=NULL)))
unique(df$file_name)

range(df$Year)
df <- df %>% filter(Year>2023) # 2024 to 2050

sectors_order <- c("EV Production","LIB Replacement","Recycling")

# Add scenario names
df <- df %>% 
  mutate(NoTrade=str_detect(file_name,"No Trade")) %>% 
  mutate(circular=str_detect(file_name,"Circular")) %>% 
  mutate(file_name=str_remove(file_name,"No Trade/|Circular/")) %>% 
  left_join(dict_scen) %>% 
  mutate(Sector=case_when(
    Sector=="EV_Production" ~ sectors_order[1],
    Sector=="add_LIB" ~ sectors_order[2],
    Sector=="LIB_recycling" ~ sectors_order[3],
    T ~ "NA"))

# Keep only one scenario for comparison
df_all <- df
unique(df$name)
df <- df %>% filter(name=="Reference"&circular==F)
table(df$name,df$NoTrade)

# Fig a - Trade Effect ---- Cumulative battery demand ----------
unique(df$name)
unique(df$Sector)

# cumulative demand
data_fig <- df %>% 
  filter(Mineral=="kWh") %>% # name is kWH, but unit is MWh, confusing I know
  mutate(tons=tons*1e3) %>%  # just for kWh adjustment
  left_join(dict_region) %>% 
  group_by(NoTrade,name,Region_EV,c,Mineral) %>% 
  reframe(TWh=sum(tons)/1e6) %>% ungroup() %>% # becomes GWh
  mutate(TWh=TWh/1e3) %>%  # TWh
  mutate(NoTrade=if_else(NoTrade,"NoTrade","GlobalTrade")) %>% 
  pivot_wider(names_from = NoTrade, values_from = TWh,values_fill = 0) %>% 
  mutate(rel_diff=(NoTrade-GlobalTrade)/GlobalTrade) %>%
  mutate(label_text=if_else(abs(rel_diff)>0.1,c,""))

head(data_fig)


p1 <- ggplot(data_fig,aes(NoTrade,GlobalTrade))+
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed")+
  # illustrative segments
  geom_segment(data=filter(data_fig,c=="United Kingdom"),col="#cb181d",linetype = "dashed",linewidth=0.2,
               aes(x=NoTrade,xend=NoTrade,y=NoTrade,yend=GlobalTrade))+
  geom_text(data=filter(data_fig,c=="United Kingdom"),label="Battery\noverestimation\n(importers)",
            size=9*5/14 * 0.8,nudge_x = 0.25,nudge_y = 1.3,hjust=0,col="#cb181d")+
  geom_segment(data=filter(data_fig,c=="Mexico"),col="#cb181d",linetype = "dashed",linewidth=0.2,
               aes(x=NoTrade,xend=NoTrade,y=NoTrade,yend=GlobalTrade))+
  geom_text(data=filter(data_fig,c=="Mexico"),label="Battery\nunderestimation\n(exporters)",
            size=9*5/14 * 0.8,nudge_x = 0.05,nudge_y = -1,hjust=0,col="#cb181d")+
  geom_point(aes(col=Region_EV),alpha=0.8,size=1)+
  geom_text_repel(aes(label=c),size=7.5*5/14 * 0.8)+
  ggforce::facet_zoom(xlim = c(0,7.5),ylim = c(0,7.5),zoom.size = 1.5)+
  # for legend
  geom_segment(data=filter(data_fig,c=="Mexico"),col="darkgrey",linewidth=0.2,
               x=-1,xend=1.7,y=4.2,yend=4.2)+
  geom_segment(data=filter(data_fig,c=="Mexico"),col="darkgrey",linewidth=0.2,
               x=1.7,xend=1.7,y=4.2,yend=8)+
  xlim(0,56)+ylim(0,56)+
  scale_color_manual(values=region_colors)+
  labs(title="2024-2050 Battery Requirements [TWh]",col="Region",tag="(a)",
       x="Calculated ommiting Trade [TWh]",y="Calculated considering Trade [TWh]")+
  theme_bw(9)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.tag = element_text(face = "bold"),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.08,0.78))
p1

# set to alpha in zommed example to avoid cluterring
{
  
pb <- ggplot_build(p1)
# remove segments on unzoomed panel
pb$data[[2]][1, 'alpha'] <- 0
pb$data[[3]][1, 'alpha'] <- 0
pb$data[[4]][1, 'alpha'] <- 0
pb$data[[5]][1, 'alpha'] <- 0
pb$data[[8]][1, 'alpha'] <- 0
pb$data[[9]][1, 'alpha'] <- 0
# labels from other countries
pos_aux <- which(pb$data[[7]]$label %in% c("China","United States","Japan","Germany","Venezuela")&
                   pb$data[[7]]$PANEL==4) # zoomed panel
pb$data[[7]][pos_aux, 'alpha'] <- 0 
spain_pos <- which(pb$data[[7]]$label %in% c("Spain")&pb$data[[7]]$PANEL==4) 
pb$data[[7]][spain_pos,'x'] <- pb$data[[7]][spain_pos,'x']+0.3
pb$data[[7]][spain_pos,'y'] <- pb$data[[7]][spain_pos,'y']-0.2
# labels from other countries in zoomed panel
pos_aux <- which(!(pb$data[[7]]$label %in% c("China","United States","Japan","Germany"))&
                   pb$data[[7]]$PANEL==1) 
pb$data[[7]][pos_aux, 'alpha'] <- 0 
pg <- ggplot_gtable(pb)
plot(pg)
}


# Fig b - Time series ----

unique(df_all$name)
df <- df_all %>% 
  filter(Mineral=="kWh") %>% # NAME IS kWH, but unit is MWh, confusing I know ....
  mutate(tons=tons*1e3) %>%  # just for kWh adjustment
  left_join(dict_region) %>% 
  group_by(Year,category,NoTrade,circular,name,Mineral,Sector,Region_EV) %>% 
  reframe(twh=sum(tons)/1e9) %>% ungroup()  # becomes TWh
head(df)
df %>% group_by(circular,NoTrade,name) %>% tally()


# line for scenario
col_scens <- c("Reference"="black",
               "Small Capacity LIB"="#FF9999",
               "Large Capacity LIB"="#660000",
               "Shorter LIB lifetime"="#FFCC99",
               "Longer EV & LIB life"="#994C00",
               "30% Global Recycling"="#99FF99",
               "80% Global Recycling"="#006600")

unique(df$name)
data_fig_a <- df %>% 
  filter(NoTrade==F,circular==F) %>% 
  group_by(Year,name) %>% reframe(twh=sum(twh)) %>% ungroup() %>% 
  mutate(name=name %>% str_replace("EV & LIB lifetime","EV & LIB life")) %>% 
  mutate(name=factor(name,levels=names(col_scens)))

p2 <- ggplot(data_fig_a,aes(Year,twh))+
  geom_line(aes(col=name),linewidth=0.5)+
  geom_line(aes(col=name),data=filter(data_fig_a,name=="Reference"),linewidth=0.8)+
  geom_text(data=filter(data_fig_a,Year==2050,name=="Reference"),
            aes(label=name),nudge_x = 0.4,
            size=9*5/14 * 0.8,hjust = 0)+
  geom_text(data=filter(data_fig_a,Year==2050,name!="Reference"),
            aes(label=name),nudge_x = 0.4,nudge_y = c(-0.2,0,0,0.2,0,0),
            size=7*5/14 * 0.8,hjust = 0)+
  labs(x="",y="TWh",title = "Primary LIB requirements [TWh]",fill="",col="",tag="(b)")+
  scale_fill_manual(values= c("EV Production" = "#FFE5B4", "LIB Replacement" = "#004D40"))+
  scale_color_manual(values=col_scens)+
  scale_x_continuous(breaks = c(2024, 2030, 2040, 2050))+
  coord_cartesian(expand=F,xlim = c(2024,2059),ylim = c(0,16.2))+
  theme_bw(9)+
  theme(panel.grid=element_blank(),
        plot.tag = element_text(face = "bold"),
        legend.position = "none")
p2

# graphical abstract
data_fig_99 <- df %>% 
  filter(NoTrade==F,circular==F) %>%
  filter(category=="Reference") %>% 
  group_by(Year,Region_EV) %>% reframe(twh=sum(twh))


ggplot(data_fig_99,aes(Year,twh,fill=Region_EV))+
  geom_area()+
  scale_fill_manual(values=region_colors)+
  labs(x="",y="",title="",tag="")+
  coord_cartesian(expand=F,xlim = c(2024,2050))+
  theme(legend.position = "none",
    axis.ticks = element_blank(),axis.text=element_blank())

ggsave("Figures/Graphical/LIBdemand.png",
       units="cm",dpi=600,
       height = 4,width = 6)


# Fig c - reference detail -----
sectors_order2 <- c("EV Production","LIB Replacement","End-of-Life LIB")
# cumulative demand 2024-2050
data_fig_b <- df %>% 
  filter(circular,name=="Reference") %>% 
  # filter(Year==2050) %>%
  group_by(Region_EV,Sector) %>% reframe(twh=sum(twh)) %>% ungroup() %>% 
  mutate(twh=if_else(Sector=="Recycling",-twh,twh),
         Sector=if_else(Sector=="Recycling","End-of-Life LIB",Sector)) %>% 
  mutate(Sector=factor(Sector,levels=(sectors_order2)),
         Region_EV=factor(Region_EV,levels=names(region_colors))) %>% 
  # by proportion
  # group_by(Sector) %>% mutate(twh=twh/sum(twh)) %>% ungroup() %>% 
  mutate(white_font=if_else(Region_EV %in% c("South America","North America","China",
                                             "Middle East"),
                            "whit","blac")) %>% 
  mutate(label_reg=if_else(twh>4,Region_EV,""))

totals <- data_fig_b %>% group_by(Sector) %>% reframe(twh=sum(twh)) %>% ungroup() %>% 
  mutate(label_total=paste0(round(twh,0),""))

p3 <- ggplot(data_fig_b,aes(Sector,twh))+
  geom_col(col="black",aes(fill=Region_EV),linewidth=0.1)+
  geom_text(data=totals,aes(label=label_total),nudge_y = 5,size=9*5/14 * 0.8)+
  geom_text(aes(label = label_reg,group=Region_EV,col=white_font), position = position_stack(vjust = 0.5),
            angle=0,size=7.5*5/14 * 0.8) +
  coord_cartesian(expand = F,ylim = c(0,167))+
  scale_fill_manual(values=region_colors)+
  scale_color_manual(values=c( "whit"="#FFFFFF","blac"="#000000"))+
  guides(color="none")+
  labs(x="",y="TWh",title="2024-2050 Battery Flows",tag="(c)")+
  theme_bw(9)+
  theme(panel.grid = element_blank(),
        plot.tag = element_text(face = "bold"),
        axis.text.x = element_text(size=7),
        legend.position = "none")
p3

# Merge -----
library(cowplot)

plot_grid(pg,
          plot_grid(p2,p3,nrow=1,rel_widths = c(0.6,0.4)),
          ncol=1)

ggsave("Figures/Fig2.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=17.8,height=8.7*2)


pdf("Figures/pdf/Fig2.pdf",width=17.8/2.54,height=8.7*2/2.54)
ggplot2::last_plot()
dev.off()




# EoF
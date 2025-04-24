# Analysis of Mineral Demand Results -----
# Figure to mineral reserve stock evolution
# PBH January 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load Results -----------

# Load from folder results
url_results <- "Results/Mineral Demand/"
# No MONET Trade ratios captures that the EV fleet demand is located at sales, and so are minerals stock
runs <- c("Circular/NoMonet.csv") 

# Read all - still fast enough
df <- do.call(rbind, lapply(runs, function(path) 
  transform(read.csv(file.path(url_results, path)), 
            file_name = str_remove(path,"\\.csv"))))
unique(df$file_name)

sectors_order <- c("EV Production","LIB Replacement","Recycling")

range(df$Year)

# Add scenario names
df <- df %>% 
  mutate(Sector=case_when(
    Sector=="EV_Production" ~ sectors_order[1],
    Sector=="add_LIB" ~ sectors_order[2],
    Sector=="LIB_recycling" ~ sectors_order[3],
    T ~ "NA"))


# Calculate Mineral Stocks and Reserve ------
# Circular and No MONET (but including 2-hand trade) to get LIB at 
# fleet location country

# get Stock in EV and LIB from 2015 to 2023
df_previous <- df %>% filter(Year<2024)
df_previous <- df_previous %>% 
  filter(Mineral!="kWh") %>% 
  mutate(tons=if_else(Sector=="Recycling",-tons,tons)) %>% 
  mutate(Sector=if_else(Sector=="Recycling","EoL LIB","EV")) %>% 
  group_by(Mineral,c,Sector) %>% 
  reframe(mtons=sum(tons)/1e6) %>% ungroup()
# Need to subtract recycling from EV flows
df_previous <- df_previous %>% 
  pivot_wider(names_from = Sector, values_from = mtons,values_fill = 0) %>% 
  mutate(EV=EV-`EoL LIB`) %>% 
  pivot_longer(c(EV,`EoL LIB`), names_to = "Sector", values_to = "mtons")
range(df_previous$mtons)

  
df <- df %>% filter(Year>2023) # 2024 to 2050
# Mineral that goes towards EV, or failing LIBs
demand <- df %>%
  filter(Mineral!="kWh") %>% 
  # filter(Mineral=="Lithium") %>% 
  mutate(tons=if_else(Sector=="Recycling",-tons,tons)) %>% 
  mutate(Sector=if_else(Sector=="Recycling","EoL LIB","EV")) %>% 
  left_join(dict_region) %>% 
  group_by(Mineral,Year,Region_Agg,c,Sector) %>% 
  reframe(mtons=sum(tons)/1e6) %>% ungroup() %>%
  arrange(Year) %>% 
  # cumulate them
  group_by(Mineral,Sector,Region_Agg,c) %>% mutate(cumTons=cumsum(mtons)) %>% ungroup()
  
cons <- demand %>% filter(Sector=="EV") %>% 
  group_by(Mineral,Year) %>% reframe(mtons=sum(mtons)) %>% ungroup()

# Need to subtract recycling from EV flows
demand <- demand %>% 
  dplyr::select(-mtons) %>% 
  pivot_wider(names_from = Sector, values_from = cumTons,values_fill = 0) %>% 
  mutate(EV=EV-`EoL LIB`) %>% 
  pivot_longer(c(EV,`EoL LIB`), names_to = "Sector", values_to = "cumTons")
range(demand$cumTons) # no weird things

  
# add reserves
source("Scripts/Load Data/05-Load_USGS.R", encoding = "UTF-8")
# show countries with a lot of reserves by Mineral
mineral_region <- usgs %>% 
    left_join(dict_region,by="c") %>% 
  mutate(Region_Agg=case_when(
    Mineral=="Lithium" & Country %in% c("Chile","Australia","Argentina")~Country,
    Mineral=="Nickel" & Country %in% c("Brazil","Australia","Indonesia")~Country,
    Mineral=="Cobalt" & Country %in% c("Congo (Kinshasa)")~"Congo",
    Mineral=="Cobalt" & Country %in% c("Australia")~Country,
    Mineral=="Graphite" & Country %in% c("Brazil")~Country,
    Mineral=="Graphite" & Region_EV.x=="Africa" ~"Africa",
    Country=="Other countries" ~ "RoW",
    T ~ Region_Agg)) %>% 
  group_by(Region_Agg,c,Mineral) %>% 
  reframe(mtons=sum(mtons,na.rm=T)) %>% ungroup()


# do it for each mineral
# reduce proportionally based on consumption - each year
stock_all <- mineral_region %>% mutate(Year=2024) %>% filter(mtons>0)

# debug Lithium
cons %>% filter(Mineral=="Lithium") %>% pull(mtons) %>% sum() # 17.8 
stock_all %>% filter(Mineral=="Lithium") %>% pull(mtons) %>% sum() # 30
stock_save <- c()
remaining_reserves <- c()

for (m in unique(cons$Mineral)){
  cat(m,"\n")
  stock <- stock_all %>% filter(Mineral==m)
  cons_aux <- cons %>% filter(Mineral==m)
  
  remaining_reserves <- rbind(remaining_reserves,
                              sum(stock$mtons)-sum(cons_aux$mtons))
  for (i in 2024:2050) {
    
    # if(i==2032){break}
    
    # reduction
    redu <- cons_aux %>% filter(Year==i) %>% pull(mtons)
    shares <- stock %>% filter(Year==i)
    total_stock <- sum(shares$mtons)
    
    redu <- pmin(redu,total_stock)
    while(redu>0.0001){
      
      # get shares
      if (redu<=total_stock) {
        shares <- shares %>% mutate(share=mtons/sum(mtons))
        shares <- shares %>% 
          mutate(reduction=pmin(share*redu,mtons),
                 mtons=mtons-reduction)
        redu <- redu-sum(shares$reduction)
        
      } else { # deficit
        shares <- tibble(c="Deficit",Mineral=m,
                         Year=i,mtons=redu)
        redu=0
        }
    }
  
    shares <- shares %>% 
      dplyr::select(Region_Agg,c,Mineral,mtons,Year) %>% 
      mutate(Year=i+1)
    
    stock <- rbind(stock, shares)
  }
  stock_save <- rbind(stock_save,stock)
}

stock <- stock_save
stock %>% filter(Year==2051,Mineral=="Lithium") %>% pull(mtons) %>% sum() # 12.2
remaining_reserves[3] # Lithium

stock <- stock %>% rename(cumTons=mtons) %>% 
  mutate(Sector="Reserve") %>% 
  mutate(Year=Year-1) %>% 
  filter(Year>2023)

# add to consumption
data_fig_c <- rbind(demand,stock)

# add 2015-2023 stock
df_previous %>% group_by(Mineral) %>% reframe(mtons=sum(mtons)) # almost neglibe
data_fig_c <- data_fig_c %>% 
  left_join(rename(df_previous,addTons=mtons)) %>% 
  mutate(addTons=if_else(is.na(addTons),0,addTons)) %>%  # Iran-Venezuela, no sales before 2024
  mutate(cumTons=cumTons+addTons,addTons=NULL)

# HHI at 2024 and 2050 - done at country level (65 unique)...
unique(data_fig_c$c)
hhi <- data_fig_c %>% 
  filter(Year %in% c(2024,2050)) %>% 
  group_by(Mineral,Year,c) %>% 
  reframe(cumTons=sum(cumTons)) %>% ungroup() %>% 
  group_by(Mineral,Year) %>% 
  mutate(market_share=cumTons/sum(cumTons)) %>% 
  reframe(hhi=sum(market_share^2)) %>% ungroup()
hhi %>% pivot_wider(names_from = Year, values_from = hhi)

# aggregate at regions
data_fig_c <- data_fig_c %>%
  group_by(Mineral,Year,Region_Agg,Sector) %>% 
  reframe(cumTons=sum(cumTons)) %>% ungroup() %>% 
  mutate(categ=paste0(Sector,Region_Agg)) %>% 
  arrange(Sector,Year,Region_Agg)

# get new colors
new_colors <- unique(data_fig_c$categ)
regionAgg_colors2 <- c(regionAgg_colors,
                       "Chile"="#d95f02",
                       "Argentina"="#ff7f00",
                       "Australia"="#cab2d6",
                       "Africa"="#4682b4",
                       "Congo"="#8b4513",
                       "Brazil"="#33a02c",
                       "Indonesia"="#66c2a5")
new_colors <- map_chr(new_colors, ~ {
  match <- names(regionAgg_colors2)[str_detect(.x, names(regionAgg_colors2))]
  if (length(match) > 0) regionAgg_colors2[match[1]] else "NA"  # Default if no match
})
names(new_colors) <- unique(data_fig_c$categ)

# Big line dividers at sector level
data_sector <- data_fig_c %>% 
  group_by(Mineral,Year,Sector) %>% 
  reframe(cumTons=sum(cumTons)) %>% ungroup() %>%
  complete(Year, Sector, fill = list(cumTons = 0)) %>% 
  mutate(Sector=factor(Sector,levels = c("Reserve","EV","EoL LIB"))) %>% 
  arrange(Sector) %>% arrange(Year) %>% 
  group_by(Mineral,Year) %>%
  mutate(cumTons=cumsum(cumTons)) %>% ungroup()


data_fig_c_all <- data_fig_c
data_sector_all <- data_sector
hhi_all <- hhi
## Lithium plot ----- 

data_fig_c <- data_fig_c_all %>% filter(Mineral=="Lithium")
data_sector <- data_sector_all %>% filter(Mineral=="Lithium")
hhi <- hhi_all %>% filter(Mineral=="Lithium") %>% mutate(Mineral=NULL)

# Add main country labels
label_fig <- data_fig_c %>% 
  filter(Year==2025) %>% 
  mutate(lab=if_else(cumTons>1&Sector=="Reserve",Region_Agg,""),
         angle=0)
label_fig2 <- data_fig_c %>% 
  filter(Year==2046) %>% 
  mutate(lab=if_else(Sector=="EV" & Region_Agg %in% c("EU","South Asia"),Region_Agg,""),
         angle=if_else(Region_Agg=="South Asia",-35,0))
label_fig <- rbind(label_fig,label_fig2)

# Figure - Mineral Stocks
p1 <- ggplot(data_fig_c,aes(Year,cumTons))+ 
  geom_area(aes(fill=categ,alpha=Sector),col="black",linewidth=0.1)+
  geom_line(data=data_sector,aes(group=Sector),linewidth=1)+
  geom_text(data=label_fig,aes(label = lab,group=categ,angle=angle),size=7*5/14 * 0.8,
            position = position_stack(vjust = 0.5),hjust=0,
            fontface = "italic")+
  annotate("text", x = 2031, y = 26.5, fontface="bold",angle=-25,
           size=8*5/14 * 0.8,label = "Reserves") +  
  annotate("text", x = 2036, y = 29, fontface="bold",angle=-5,
           size=8*5/14 * 0.8,label = "EV Stock") +  
  annotate("text", x = 2046, y = 29, fontface="bold",
           size=8*5/14 * 0.8,label = "EoL LIB Stock") + 
  geom_hline(yintercept = remaining_reserves[3],linetype="dashed",linewidth=0.5)+
  scale_fill_manual(values=new_colors)+
  scale_x_continuous(breaks = c(2024, 2030, 2040, 2050))+
  # add HHI as axis titles
  scale_y_continuous(sec.axis = dup_axis(name = paste0("HHI: ",round(hhi[2,2],2))),
                     name=paste0("HHI: ",round(hhi[1,2],2)))+
  scale_alpha_manual(values = c("Reserve"=1,"EV"=0.8,"EoL LIB"=0.6)) +
  coord_cartesian(expand=F)+
  labs(x="",title="Lithium Stock [million tons]",tag="(a)")+
  theme_bw(8)+
  theme(panel.grid=element_blank(),
        plot.tag = element_text(face = "bold"),
        plot.tag.position = c(0.03, 1),
        axis.title.y = element_text(margin=margin(0,0,-20,0)),
        axis.title.y.right = element_text(angle = 90,margin=margin(0,0,0,-5)),
        legend.position = "none")
p1

# add fig to the right
data_bar <- data_fig_c %>% filter(Year==2050) %>% 
  group_by(Sector) %>% reframe(cumTons=sum(cumTons)) %>% ungroup() %>% 
  mutate(Sector=factor(Sector,levels = rev(c("Reserve","EV","EoL LIB"))),
         x=" ")
p1_bar <- ggplot(data_bar,aes(x,cumTons,group = Sector))+
  geom_col(fill="darkgrey",col="black",linewidth=0.1,aes(alpha=Sector))+
  geom_text(aes(label = Sector),size=8*5/14 * 0.8,angle=90,
            position = position_stack(vjust = 0.5),hjust=0.6)+
  scale_alpha_manual(values = c("Reserve"=1,"EV"=0.7,"EoL LIB"=0.4)) +
  theme_bw(8)+labs(x="",y="",title=" ")+
  theme(legend.position = "none",
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        panel.border = element_blank())

library(cowplot)
p1_bar <- p1_bar+theme(plot.margin = margin(-7,0,3,-5)) #trbl
p_li <- plot_grid(p1,p1_bar,nrow=1,rel_widths = c(0.93,0.07)) 
p_li

## Nickel -----

data_fig_c <- data_fig_c_all %>% filter(Mineral=="Nickel")
data_sector <- data_sector_all %>% filter(Mineral=="Nickel")
hhi <- hhi_all %>% filter(Mineral=="Nickel") %>% mutate(Mineral=NULL)

# Add main country labels
label_fig <- data_fig_c %>% 
  filter(Year==2025) %>% 
  mutate(lab=if_else(cumTons>1&Sector=="Reserve",Region_Agg,""),
         angle=0)
label_fig2 <- data_fig_c %>% 
  filter(Year==2046) %>% 
  mutate(lab=if_else(Sector=="EV" & Region_Agg %in% c("EU"),Region_Agg,""),
         angle=0)
label_fig <- rbind(label_fig,label_fig2)

# Figure - Mineral Stocks
p2 <- ggplot(data_fig_c,aes(Year,cumTons))+ 
  geom_area(aes(fill=categ,alpha=Sector),col="black",linewidth=0.1)+
  geom_line(data=data_sector,aes(group=Sector),linewidth=1)+
  geom_text(data=label_fig,aes(label = lab,group=categ,angle=angle),size=7*5/14 * 0.8,
            position = position_stack(vjust = 0.45),hjust=0,
            fontface = "italic")+
  annotate("text", x = 2031, y = 115, fontface="bold",angle=-25,
           size=8*5/14 * 0.8,label = "Reserves") +  
  annotate("text", x = 2036, y = 126, fontface="bold",angle=-5,
           size=8*5/14 * 0.8,label = "EV Stock") +  
  annotate("text", x = 2046, y = 126.5, fontface="bold",
           size=8*5/14 * 0.8,label = "EoL LIB Stock") + 
  geom_hline(yintercept = remaining_reserves[4],linetype="dashed",linewidth=0.5)+
  scale_fill_manual(values=new_colors)+
  scale_x_continuous(breaks = c(2024, 2030, 2040, 2050))+
  # add HHI as axis titles
  scale_y_continuous(sec.axis = dup_axis(name = paste0("HHI: ",round(hhi[2,2],2))),
                     breaks = c(0,50,100),
                     name=paste0("HHI: ",round(hhi[1,2],2)))+
  scale_alpha_manual(values = c("Reserve"=1,"EV"=0.8,"EoL LIB"=0.6)) +
  coord_cartesian(expand=F)+
  labs(x="",title="Nickel Stock [million tons]",tag="(b)")+
  theme_bw(8)+
  theme(panel.grid=element_blank(),
        plot.tag = element_text(face = "bold"),
        plot.tag.position = c(0.03, 1),
        axis.title.y = element_text(margin=margin(0,0,-20,0)),
        axis.title.y.right = element_text(angle = 90,margin=margin(0,0,0,-5)),
        legend.position = "none")
p2

# add fig to the right
data_bar <- data_fig_c %>% filter(Year==2050) %>% 
  group_by(Sector) %>% reframe(cumTons=sum(cumTons)) %>% ungroup() %>% 
  mutate(Sector=factor(Sector,levels = rev(c("Reserve","EV","EoL LIB"))),
         x=" ")
p2_bar <- ggplot(data_bar,aes(x,cumTons,group = Sector))+
  geom_col(fill="darkgrey",col="black",linewidth=0.1,aes(alpha=Sector))+
  geom_text(aes(label = Sector),size=8*5/14 * 0.8,angle=90,
            position = position_stack(vjust = 0.5),hjust=0.6)+
  scale_alpha_manual(values = c("Reserve"=1,"EV"=0.7,"EoL LIB"=0.4)) +
  theme_bw(8)+labs(x="",y="",title=" ")+
  theme(legend.position = "none",
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        panel.border = element_blank())

p2_bar <- p2_bar+theme(plot.margin = margin(-7,0,3,-5)) #trbl
p_ni <- plot_grid(p2,p2_bar,nrow=1,rel_widths = c(0.93,0.07)) 
p_ni

## Cobalt ------

data_fig_c <- data_fig_c_all %>% filter(Mineral=="Cobalt")
data_sector <- data_sector_all %>% filter(Mineral=="Cobalt")
hhi <- hhi_all %>% filter(Mineral=="Cobalt") %>% mutate(Mineral=NULL)

# No longer deficit
# add reserve deficit - only in 2050
(diff <- data_fig_c %>% filter(Year>2048) %>% group_by(Year) %>% 
  reframe(cumTons=sum(cumTons)) %>% ungroup())
diff <- (diff[1,2]-diff[2,2]) %>% as.numeric()
# reduce diff proportionally in EVs!

# data_2050 <- data_fig_c %>% filter(Year==2050) %>% 
#   filter(categ!="ReserveRoW") %>% 
#   filter(Sector=="EV") %>% 
#   mutate(share=cumTons/sum(cumTons)) %>% 
#   mutate(cumTons=cumTons+share*diff) %>% 
#   mutate(share=NULL)

# add it as deficit
# data_fig_c <- rbind(filter(data_fig_c,Year<2050|Sector=="EoL LIB"),
#                     data_2050,
#                     tibble(Mineral="Cobalt",Year=c(2049,2050),Region_Agg="ROw",
#                            Sector="Reserve",categ="ReserveRoW",
#                            cumTons=c(0,diff)))

# re-calculate lines
# data_sector_2050 <- data_sector %>% filter(Year==2050) %>% 
#   mutate(cumTons=cumTons+diff)
# 
# data_sector <- rbind(filter(data_sector,Year<2050),data_sector_2050)

# Add main country labels
label_fig <- data_fig_c %>% 
  filter(Year==2025) %>% 
  mutate(lab=if_else(cumTons>0.5&Sector=="Reserve",Region_Agg,""),
         angle=0)
label_fig2 <- data_fig_c %>% 
  filter(Year==2046) %>% 
  mutate(lab=if_else(Sector=="EV" & Region_Agg %in% c("North America","EU"),Region_Agg,""),
         angle=if_else(Region_Agg=="North America",-40,0))
label_fig <- rbind(label_fig,label_fig2)

# Figure - Mineral Stocks
p3 <- ggplot(data_fig_c,aes(Year,cumTons))+ 
  geom_area(aes(fill=categ,alpha=Sector),col="black",linewidth=0.1)+
  geom_line(data=data_sector,aes(group=Sector),linewidth=1)+
  geom_text(data=label_fig,aes(label = lab,group=categ,angle=angle),size=7*5/14 * 0.8,
            position = position_stack(vjust = 0.5),hjust=0,
            fontface = "italic")+
  annotate("text", x = 2031, y = 8.8, fontface="bold",angle=-25,
           size=8*5/14 * 0.8,label = "Reserves") +  
  annotate("text", x = 2036, y = 10.2, fontface="bold",angle=-5,
           size=8*5/14 * 0.8,label = "EV Stock") +  
  annotate("text", x = 2046, y = 10.2, fontface="bold",
           size=8*5/14 * 0.8,label = "EoL LIB Stock") + 
  # annotate("text", x = 2047, y = -0.3, fontface="bold",
  #          size=8*5/14 * 0.8,label = "Deficit") + 
  geom_hline(yintercept = remaining_reserves[1],linetype="dashed",linewidth=0.5)+
  scale_fill_manual(values=new_colors)+
  scale_x_continuous(breaks = c(2024, 2030, 2040, 2050))+
  # add HHI as axis titles
  scale_y_continuous(sec.axis = dup_axis(name = paste0("HHI: ",round(hhi[2,2],2))),
                     breaks=c(0,3.5,7,10.5),
                     name=paste0("HHI: ",round(hhi[1,2],2)))+
  scale_alpha_manual(values = c("Reserve"=1,"EV"=0.8,"EoL LIB"=0.6)) +
  coord_cartesian(expand=F)+
  labs(x="",title="Cobalt Stock [million tons]",tag="(c)")+
  theme_bw(8)+
  theme(panel.grid=element_blank(),
        plot.tag = element_text(face = "bold"),
        plot.tag.position = c(0.03, 1),
        axis.title.y = element_text(margin=margin(0,0,-20,0)),
        axis.title.y.right = element_text(angle = 90,margin=margin(0,0,0,-5)),
        legend.position = "none")
p3

# add fig to the right
data_bar <- data_fig_c %>% filter(Year==2050) %>% 
  group_by(Sector) %>% reframe(cumTons=sum(cumTons)) %>% ungroup() %>% 
  mutate(Sector=str_replace(Sector,"Reserve"," ")) %>% 
  mutate(Sector=factor(Sector,levels = rev(c(" ","EV","EoL LIB"))),
         x=" ")
p3_bar <- ggplot(data_bar,aes(x,cumTons,group = Sector))+
  geom_col(fill="darkgrey",col="black",linewidth=0.1,aes(alpha=Sector))+
  geom_text(aes(label = Sector),size=8*5/14 * 0.8,angle=90,
            position = position_stack(vjust = 0.5),hjust=0.8)+
  scale_alpha_manual(values = c(" "=1,"EV"=0.7,"EoL LIB"=0.4)) +
  theme_bw(8)+labs(x="",y="",title=" ")+
  theme(legend.position = "none",
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        panel.border = element_blank())

p3_bar <- p3_bar+theme(plot.margin = margin(-7,0,3,-5)) #trbl
p_co <- plot_grid(p3,p3_bar,nrow=1,rel_widths = c(0.93,0.07)) 
p_co



## Graphite ------
data_fig_c <- data_fig_c_all %>% filter(Mineral=="Graphite")
data_sector <- data_sector_all %>% filter(Mineral=="Graphite")
hhi <- hhi_all %>% filter(Mineral=="Graphite") %>% mutate(Mineral=NULL)

# Add main country labels
label_fig <- data_fig_c %>% 
  filter(Year==2025) %>% 
  mutate(lab=if_else(cumTons>1&Sector=="Reserve",Region_Agg,""),
         angle=0)
label_fig2 <- data_fig_c %>% 
  filter(Year==2045) %>% 
  mutate(lab=if_else(Sector=="EV" & Region_Agg %in% c("EU","North America"),Region_Agg,""),
         angle=if_else(Region_Agg=="North America",-30,0))
label_fig <- rbind(label_fig,label_fig2)

# Figure - Mineral Stocks
p4 <- ggplot(data_fig_c,aes(Year,cumTons))+ 
  geom_area(aes(fill=categ,alpha=Sector),col="black",linewidth=0.1)+
  geom_line(data=data_sector,aes(group=Sector),linewidth=1)+
  geom_text(data=label_fig,aes(label = lab,group=categ,angle=angle),size=7*5/14 * 0.8,
            position = position_stack(vjust = 0.5),hjust=0,
            fontface = "italic")+
  annotate("text", x = 2031, y = 245, fontface="bold",angle=-25,
           size=8*5/14 * 0.8,label = "Reserves") +  
  annotate("text", x = 2036, y = 265, fontface="bold",angle=-5,
           size=8*5/14 * 0.8,label = "EV Stock") +  
  annotate("text", x = 2046, y = 270, fontface="bold",
           size=8*5/14 * 0.8,label = "EoL LIB Stock") + 
  geom_hline(yintercept = remaining_reserves[2],linetype="dashed",linewidth=0.5)+
  scale_fill_manual(values=new_colors)+
  scale_x_continuous(breaks = c(2024, 2030, 2040, 2050))+
  # add HHI as axis titles
  scale_y_continuous(sec.axis = dup_axis(name = paste0("HHI: ",round(hhi[2,2],2))),
                     name=paste0("HHI: ",round(hhi[1,2],2)))+
  scale_alpha_manual(values = c("Reserve"=1,"EV"=0.8,"EoL LIB"=0.6)) +
  coord_cartesian(expand=F)+
  labs(x="",title="Natural Graphite Stock [million tons]",tag="(d)")+
  theme_bw(8)+
  theme(panel.grid=element_blank(),
        plot.tag = element_text(face = "bold"),
        plot.tag.position = c(0.03, 1),
        axis.title.y = element_text(margin=margin(0,0,-20,0)),
        axis.title.y.right = element_text(angle = 90,margin=margin(0,0,0,-5)),
        legend.position = "none")
p4

# add fig to the right
data_bar <- data_fig_c %>% filter(Year==2050) %>% 
  group_by(Sector) %>% reframe(cumTons=sum(cumTons)) %>% ungroup() %>%
  mutate(Sector=factor(Sector,levels = rev(c("Reserve","EV","EoL LIB"))),
         x=" ")
p4_bar <- ggplot(data_bar,aes(x,cumTons,group = Sector))+
  geom_col(fill="darkgrey",col="black",linewidth=0.1,aes(alpha=Sector))+
  geom_text(aes(label = Sector),size=8*5/14 * 0.8,angle=90,
            position = position_stack(vjust = 0.5),hjust=0.6)+
  scale_alpha_manual(values = c("Reserve"=1,"EV"=0.7,"EoL LIB"=0.4)) +
  theme_bw(8)+labs(x="",y="",title=" ")+
  theme(legend.position = "none",
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        panel.border = element_blank())

p4_bar <- p4_bar+theme(plot.margin = margin(-7,0,3,-5)) #trbl
p_gr <- plot_grid(p4,p4_bar,nrow=1,rel_widths = c(0.93,0.07)) 
p_gr

# Merge them together ----

plot_grid(p_li,p_ni,p_co,p_gr,ncol=2)


ggsave("Figures/MineralStock.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=17.8,height=8.7*1.5)

# EoF
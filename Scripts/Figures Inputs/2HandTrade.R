# Calculates 2 hand flows of vehicles towards other countries, for recycling purposes
# Figure 2-Hand Trade data
# PBH August 2024

# Load data --------

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Francisco Pares collected and organized the data
tradeVeh <- readxl::read_excel("Inputs/Trade Ratios/2023.reported.scn.xlsx",
                               sheet="export.share")
tradeVeh <- as.matrix(tradeVeh)
rowSums(tradeVeh)
colnames(tradeVeh) <- colnames(tradeVeh) %>% str_replace_all("\\."," ") %>% 
  str_replace("Asia/oceania","Asia/Oceania")
rownames(tradeVeh) <- colnames(tradeVeh)

fromto <- melt(tradeVeh,id.vars = "origin")
colnames(fromto)<-c("origin","destination","value")

# Collapse to selected countries ----
high_c <- c("Germany","Spain","Turkey","Czechia","United Kingdom",
                            "Russia","Sweden","Slovakia","Netherlands","Belgium",
                            "Morocco","China","Japan","South Korea",
                            "United States","Mexico","Canada",
                            "South Africa","Iran",
                            "Argentina","Brazil",
                            "Thailand","India")


aux1 <- dict_region %>% mutate(origin=c,r.origin=Region_EV) %>% dplyr::select(origin,r.origin)
aux2 <- dict_region %>% mutate(destination=c,r.destination=Region_EV) %>% dplyr::select(destination,r.destination)

fromto <- fromto %>% 
  mutate(origin=if_else(origin=="Taiwan","China",origin)) %>% 
  mutate(destination=if_else(destination=="Taiwan","China",destination)) %>% 
  left_join(aux1) %>% 
  left_join(aux2) %>% 
  mutate(origin=if_else(origin %in% high_c,origin,
                        paste0("Rest of ",r.origin)),
         destination=if_else(destination %in% high_c,destination,
                             paste0("Rest of ",r.destination))) %>% 
  group_by(r.origin,origin,r.destination,destination) %>% 
  reframe(value=sum(value)) %>% ungroup()


# calculate trade ratios
fromto <- fromto %>% 
  group_by(origin) %>% 
  mutate(value=value/sum(value)) %>% 
  arrange(value)
fromto %>% group_by(origin) %>% reframe(x=sum(value))
fromto %>% filter(origin==destination) %>% arrange((value)) %>% head()

# Figure
head(fromto)

level_origin <- c("<0.1%","0.1-1%","1-10%","10-20%","20-30%","30-40%","40-50%",
                  "50-60%","60-70%","70-80%","80-90%","90-100%")
ratios <-fromto %>% 
    mutate(interval=case_when(
      value<0.001 ~ "<0.1%",
      value<0.01 ~ "0.1-1%",
      value<0.1 ~ "1-10%",
      value<0.2 ~ "10-20%",
      value<0.3 ~ "20-30%",
      value<0.4 ~ "30-40%",
      value<0.5 ~ "40-50%",
      value<0.6 ~ "50-60%",
      value<0.7 ~ "60-70%",
      value<0.8 ~ "70-80%",
      value<0.9 ~ "80-90%",
      T ~ "90-100%") %>% factor(levels=level_origin))


order_region <- c("European Union","Other Europe","North America",
                  "South America","Oceania","Asia","China",
                  "Middle East","Africa")
ratios <- ratios %>% 
  mutate(Region_EV=factor(r.origin,levels=order_region)) %>%
  arrange(Region_EV)

ratios %>% filter(is.na(Region_EV)) %>% pull(origin) %>% unique()

# country order
c_order <- ratios %>% 
  mutate(last=str_detect(origin,"Rest of")) %>% 
  arrange(Region_EV,last,origin) %>% 
  pull(origin) %>% unique()


ratios <- ratios %>% 
  mutate(origin=factor(origin,levels=rev(c_order))) %>% 
  mutate(destination=factor(destination,levels=c_order))

text_caption <- "Horizontal sum = 100%"
  
  
new_scale <- function(new_aes) {
    structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
  }
  
ggplot_add.new_aes <- function(object, plot, object_name) {
    plot$layers <- lapply(plot$layers, bump_aes, new_aes = object)
    plot$scales$scales <- lapply(plot$scales$scales, bump_aes, new_aes = object)
    plot$labels <- bump_aes(plot$labels, new_aes = object)
    plot
  }
  
# order for box
ratios %>% group_by(Region_EV,origin) %>% tally() %>% ungroup() %>% 
  group_by(Region_EV) %>% tally() %>% mutate(x=cumsum(n))
box_x <- c(0.5,8.5,11.5,14.5,17.5,18.5,23.5,24.5,27.5,30.5)
box_y <- c(0.5,3.5,6.5,7.5,12.5,13.5,16.5,19.5,22.5,30.5)
text_x <- c(6,12.8,12,19,16.7,21.5,24.5,24,28.5)
text_y <- c(4,3,8,13,13,16,16,22,22)


size_text=12
  
colors <- brewer_pal(palette = "RdYlGn", direction = -1)(length(levels(ratios$interval)))
colors[1] <- "transparent"

ratios %>% 
  mutate(border=if_else(value<0.001,"omit","include")) %>%
    ggplot(aes(destination,origin,fill=interval))+
    geom_tile(aes(color=border))+
    # boxes
    geom_rect(xmin=box_x[1],xmax=box_x[2],ymin=box_y[9],ymax=box_y[10],linewidth=1,col="brown",fill="transparent")+
    geom_rect(xmin=box_x[2],xmax=box_x[3],ymin=box_y[8],ymax=box_y[9],linewidth=1,col="brown",fill="transparent")+
    geom_rect(xmin=box_x[3],xmax=box_x[4],ymin=box_y[7],ymax=box_y[8],linewidth=1,col="brown",fill="transparent")+
    geom_rect(xmin=box_x[4],xmax=box_x[5],ymin=box_y[6],ymax=box_y[7],linewidth=1,col="brown",fill="transparent")+
    geom_rect(xmin=box_x[5],xmax=box_x[6],ymin=box_y[5],ymax=box_y[6],linewidth=1,col="brown",fill="transparent")+
    geom_rect(xmin=box_x[6],xmax=box_x[7],ymin=box_y[4],ymax=box_y[5],linewidth=1,col="brown",fill="transparent")+
    geom_rect(xmin=box_x[7],xmax=box_x[8],ymin=box_y[3],ymax=box_y[4],linewidth=1,col="brown",fill="transparent")+
    geom_rect(xmin=box_x[8],xmax=box_x[9],ymin=box_y[2],ymax=box_y[3],linewidth=1,col="brown",fill="transparent")+
    geom_rect(xmin=box_x[9],xmax=box_x[10],ymin=box_y[1],ymax=box_y[2],linewidth=1,col="brown",fill="transparent")+
  scale_color_manual(values = c("omit" = "white", "include" = "black"))+
    # geom_tile(data = filter(ratios,value>0.01),color="black")+
    scale_fill_manual(values=colors, breaks=levels(ratios$interval),
                      labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
    geom_text(x=text_x[1],y=text_y[9],label="European Union",col="brown")+
    geom_text(x=text_x[2],y=text_y[8],label="Other Europe",col="brown")+
    geom_text(x=text_x[3],y=text_y[7],label="North America",col="brown")+
    geom_text(x=text_x[4],y=text_y[6],label="South America",col="brown")+
    geom_text(x=text_x[5],y=text_y[5],label="Oceania",col="brown")+
    geom_text(x=text_x[6],y=text_y[4],label="Asia",col="brown")+
    geom_text(x=text_x[7],y=text_y[3],label="China",col="brown")+
    geom_text(x=text_x[8],y=text_y[2],label="Middle East",col="brown")+
    geom_text(x=text_x[9],y=text_y[1],label="Africa",col="brown")+
    labs(x="Country Target",y="Source",fill="% Destination",
         caption=paste0(text_caption,". "))+
    # xlim(country_order)+ylim(rev(countries_prod))+
  guides(color="none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=size_text),
          axis.text.y = element_text(size=size_text))
  
ggsave("Figures/Inputs/2hand_ratios.png",
       units="cm",dpi=600,
       height = 12.4*2,width = 12.8*3)
  
# EoF
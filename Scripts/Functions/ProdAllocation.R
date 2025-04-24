# Calculates EV production using trade ratios matrix
# NO Segments
# PBH January 2025

# MONET Trade Ratios -------
# From MONET - Public Tool
sales_ratio <- readxl::read_excel("Inputs/Trade Ratios/2023_MONET_tstats_sharematrix.xlsx",
                                  sheet="2023.import.share")
colSums(sales_ratio)

# Flat
names(sales_ratio) <- colnames(sales_ratio) %>% str_replace_all("\\."," ") %>% 
  str_replace("Asia/oceania","Asia/Oceania") %>% 
  str_replace("East Africa","East/Africa") %>% 
  str_replace("Eastern Europe Central Asia","Eastern Europe/Central Asia")
rownames(sales_ratio) <- colnames(sales_ratio)

# Remove Taiwan, no sales data from ICCT
aux <- which(rownames(sales_ratio)=="Taiwan") 
sales_ratio <- sales_ratio[-aux,-aux]
rm(aux)

sales_ratio <- as.matrix(sales_ratio)
rownames(sales_ratio) <- colnames(sales_ratio)
# same order of countries
countries <- read.csv("Inputs/EV_Sales.csv") %>% arrange(c) %>% 
  pull(c) %>% unique() # 64
sales_ratio <- sales_ratio[countries,countries]

# fix Taiwan removal by re-balancing
sales_ratio <- sweep(sales_ratio,2,colSums(sales_ratio),"/")
colSums(sales_ratio)

# 2-Hand vehicle trade ratios -----
tradeVeh <- readxl::read_excel("Inputs/Trade Ratios/2023.reported.scn.xlsx",
                       sheet="export.share")
tradeVeh <- as.matrix(tradeVeh)
rowSums(tradeVeh)
colnames(tradeVeh) <- colnames(tradeVeh) %>% str_replace_all("\\."," ") %>% 
  str_replace("Asia/oceania","Asia/Oceania")
rownames(tradeVeh) <- colnames(tradeVeh)
# Remove Taiwan
aux <- which(rownames(tradeVeh)=="Taiwan") # Fix at trade ratios level
tradeVeh <- tradeVeh[-aux,-aux]
tradeVeh <- tradeVeh[countries,countries]
rm(aux)

# fix Taiwan removal by re-balancing
tradeVeh <- sweep(tradeVeh,1,rowSums(tradeVeh),"/")
rowSums(tradeVeh)


# Input:
# - Sales vector by C (or fleet vector by country, for second hand)
# Output: Production vector
# Made in vectorized form for computational efficiency
# ORDER IS KEY!
f.TradeRatios <- function(sales,MONET=T){

  if (MONET){
    # allocate production
    return(drop(sales_ratio%*%sales)) # Matrix to vector multiplication
  } else {
    # 2 hand 
    return(drop(sales%*%tradeVeh))
  }
   
}

# debug - MONET
sales <- icct %>% filter(Year==2022)
prod <- f.TradeRatios(sales$Sales)
# Basic Check
sum(sales$Sales);sum(prod)

# 2-hand 
prod2 <- f.TradeRatios(sales$Sales,MONET = F)
sum(sales$Sales);sum(prod2)


# EoF
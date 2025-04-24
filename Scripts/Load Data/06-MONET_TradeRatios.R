# 2023 MONET LDV TRADE RATIOS
# Original MONET Data: Francisco Pares (already balanced)
# Scenarios: PBH, same logic as MONET - https://journals.sagepub.com/doi/full/10.1177/03611981241244797
# MARCH 2025

library(tidyverse)

# MONET 2023 Ratios
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

# Higher Domestic Supply ---------

#  Relative increase of DS
inc_ds <- 0.4
max_ds <- 0.95 # max increase

sales.share.matrix <- as.matrix(sales_ratio)
ds <- diag(sales.share.matrix)
diag(sales.share.matrix) <- 0
  
# Note: need to allocate correctly both of these matrix
colSums(sales.share.matrix)+ds
  
max_ds <- ifelse(max_ds>ds,max_ds,ds) # allow max at current limit
new_ds <-ds*(1+inc_ds) 
new_ds <- ifelse(new_ds>max_ds,max_ds,new_ds) # limit DS to allowed limit
  
# get total increase in DS
increase_ds <- new_ds- ds
  
# Allocate increase in DS by reducing share of foreign supply proportionally
# 1. We need to scale the Foreign Supply to 100%
x <- sweep(sales.share.matrix,2,colSums(sales.share.matrix),"/")
# replace NaN to zero for countries with 100% DS
x[] <- apply(x, 2, function(x) replace(x, is.nan(x), 0))
  
colSums(x) # now share is 100%
  
# 2. Now we can assign the increase of DS proportionally
y <- sweep(x,2,increase_ds,"*")
  
# 3. Substract to each country the proportional decrease in FS
sales.share.matrix <- sales.share.matrix-y
diag(sales.share.matrix) <- new_ds
colSums(sales.share.matrix) # add 1

# Save
write.csv(sales.share.matrix,"Inputs/Trade Ratios/salesShare_HigherDS.csv",row.names = T)


# Global Free Trade -----------

# new Foreign supply quota to allocate 
fs_allocation <- 0.5
top_count <- 12

sales.share.matrix <- sales_ratio
ds <- diag(as.matrix(sales.share.matrix))

# Use shares of Domestic exports, not considering Domestic supply to allocate 
# Allocation based on LDV Exports shares
ldv_prod <- readxl::read_excel("Inputs/Trade Ratios/2023_MONET_tstats_sharematrix.xlsx",
                                sheet="2023.tstats")
ldv_prod <- ldv_prod %>% dplyr::select(c,de) %>% 
  mutate(perc=de/sum(de))

# Top 12 producers and their share
ldv_prod <- ldv_prod %>% 
  top_n(top_count) %>%  
  mutate(s.p=perc/sum(perc)) %>% # re-scale to 100
  rename(country=c)
ldv_prod
sum(ldv_prod$s.p) 

# Plant seeds - assign new FS supply to other countries
prod_seeds <- data.frame(country=colnames(sales.share.matrix)) %>% 
  left_join(ldv_prod) %>% replace_na(list(s.p=0)) %>% pull(s.p)
sum(prod_seeds)

sales.share.matrix <- as.matrix(sales.share.matrix)

# add new seeds and reduce current FS proportionally
s.fs <- 1-ds # Share of imports for in country sales 

# prod seeds per country FS share (cross-product)
prod_seeds_c <- prod_seeds %*% t(s.fs)

diag(sales.share.matrix) <- 0
sales.share.matrix <- sales.share.matrix*(1-fs_allocation)
sales.share.matrix <- sales.share.matrix+prod_seeds_c*fs_allocation

# remove diagonal and add to DS
sales.share.matrix <- as.matrix(sales.share.matrix)
ds <- ds+diag(sales.share.matrix)
diag(sales.share.matrix) <- ds
sales.share.matrix <- as.data.frame(sales.share.matrix)

colSums(sales.share.matrix) # add 1

rowSums(sales_ratio)[31]
rowSums(sales.share.matrix)[31] # mexico grew


# Save
write.csv(sales.share.matrix,"Inputs/Trade Ratios/salesShare_GlobalTrade.csv",row.names = T)


# EoF
# Set key parameters for Demand Module Runs
# Mineral Demand Module
# PBH Dec 2023 - Recycled from Critical Minerals Project

library(tidyverse)

# Battery Survival Parameters ------
delay_recycling_year <- 1 # 1 year to recycle to get new demand
cathode_scrap <- 0.04 # Manufacturing scrap

world_baseline_recycling <- 0 # no recycling

mat_recovery_recycling <- expand.grid(c=countries,Year=2022:2050,
            Mineral=c("Lithium","Nickel","Cobalt","Graphite","kWh")) %>% 
  mutate(mat_recov_recyc=world_baseline_recycling)
range(mat_recovery_recycling$Year)
unique(mat_recovery_recycling$c) # 64


# to calculate later circularity potential under best case scenario - 
## also useful to quantify LIB outflow 
circular_recycling <- expand.grid(c=countries,Year=2022:2050,
                                  Mineral=c("Lithium","Nickel","Cobalt","Graphite","kWh")) %>% 
  mutate(mat_recov_recyc=1)


# # old WITH CURRENT TARGETS -----
# 
#  
# # European Union members recycling
# # % of material recovery in recycling
# # 90% for cobalt, nickel, copper and lead by the end of 2027, rising to 95% in 2031; 
# # and 50% for lithium by 2027, rising to 80% in 2031.
# # No mention of graphite
# EU_targets <- tibble(
#   Mineral=c("Lithium","Nickel","Cobalt","Graphite","kWh"), 
#   # kWh" rec equal to Ni and Co
#   mat_recov_recyc1=c(0.5,0.9,0.9,0.05,0.9),
#   mat_recov_recyc2=c(0.8,0.95,0.95,0.05,0.95),
#   dummy=1) %>% 
#   left_join(tibble(Year=2022:2050,dummy=1),
#             relationship = "many-to-many") %>% 
#   # linear adoption of targets
#   mutate(mat_recov_recyc=case_when(
#     Year<2029 ~ mat_recov_recyc1/7*(Year-2021),
#     Year<2033 ~mat_recov_recyc1+(mat_recov_recyc2-mat_recov_recyc1)/5*(Year-2027),
#     T ~ mat_recov_recyc2)) %>% 
#   dplyr::select(-mat_recov_recyc1,-mat_recov_recyc2,-dummy)
# 
# China_targets <- tibble(
#   Mineral=c("Lithium","Nickel","Cobalt","Graphite","kWh"),
#   mat_recov_recyc=c(0.85,0.98,0.98,0.05,0.98))
# 
# 
# # need list of countries from ICCT Sales
# mat_recovery_recycling <- tibble(c=countries) %>% 
#   mutate(c=str_replace(c,"\\/"," ")) %>% 
#   left_join(dict_region) %>% dplyr::select(c,EU_Union)
# 
# # EU targets
# EU_rec <- mat_recovery_recycling %>% filter(EU_Union==1) %>% 
#   mutate(dummy=1) %>% 
#   left_join(mutate(EU_targets,dummy=1), relationship = "many-to-many") %>% 
#   dplyr::select(-dummy,-EU_Union)
# 
# # Rest
# rest_rec <- mat_recovery_recycling %>% filter(EU_Union!=1) %>% 
#   mutate(dummy=1) %>% 
#   left_join(tibble(Year=2022:2050,dummy=1),relationship = "many-to-many") %>%
#   left_join(mutate(China_targets,dummy=1), relationship = "many-to-many") %>%
#   dplyr::select(-dummy,-EU_Union) %>% 
#   # fix rest of the world
#   mutate(mat_recov_recyc=if_else(c=="China",mat_recov_recyc,world_baseline_recycling))
# 
# # Join
# mat_recovery_recycling <- rbind(rest_rec,EU_rec) %>% 
#   arrange(c)
# range(mat_recovery_recycling$Year)
# unique(mat_recovery_recycling$c) # 64
# 
# rm(EU_targets,China_targets,EU_rec,rest_rec)
#   

# EoF
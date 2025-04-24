# Set Lifetime parameter
# Output: EV lifetime, LIB lifetime, with sd 
# No Country nor Chemistry detail for now
# PBH January  2025

# Need to load ICCT data before-hand
# icct <- read.csv("Inputs/EV_Sales.csv")

lifetime <- icct %>% group_by(c) %>% 
  tally() %>% mutate(n=NULL)

# Common for all countries for now
mean_ev=17;sd_ev=4;mean_lib=15;sd_lib=4

# EoF
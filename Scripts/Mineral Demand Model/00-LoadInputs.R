# Mineral Demand Module
# PBH July 2024
# Reference Scenario
# Updated: January 2025 for inputs that change over time (indexed by year)

# ++++++++++++++++++++++++++++++++++
# LOAD INPUTS ---------
# ++++++++++++++++++++++++++++++++++
source("Scripts/00-Libraries.R", encoding = "UTF-8")

## ICCT Sales Data by 65 regions -----
icct <- read.csv("Inputs/EV_Sales.csv")
icct <- icct %>% arrange(c)
countries <- unique(icct$c) # 64
icct <- icct %>% complete(c,Year, fill = list(Sales = 0))

## Battery size ------- 
batsize <- read.csv("Inputs/bat_size_forecast.csv")
chem <- read.csv("Inputs/chem.csv")
range(batsize$Year)

## Trade Ratios  allocation function -----
# Both MONET and 2-hand in same function
source("Scripts/Functions/ProdAllocation.R")
source("Scripts/Functions/shiftYears.R")

## Mineral intensity -----
mineral <- read_excel("Inputs/Mineral_Intensity_2025.xlsx",sheet = "Study_Scope")
mineral <- mineral %>% dplyr::select(Mineral,chemistry,kg_per_kwh)
mineral %>% filter(Mineral %in% min_interest) %>% mutate(kg_per_kwh=kg_per_kwh*1e3) %>% 
  pivot_wider(names_from = Mineral, values_from = kg_per_kwh)

## Lifetime ----
source("Scripts/Load Data/03-Lifetime.R")

## Recycling ------
source("Scripts/Load Data/04-Recycling.R")

## Survival functions -----------
source("Scripts/Functions/SurvivalOutflows.R", encoding = "UTF-8")


## Minerals per EV by Year -----
# Idea: Table by country, year of mineral content per EV, to estimate right away the mineral
mineral_perLIB_all <- chem %>% 
  left_join(batsize) %>% 
  mutate(kwh_veh=kwh_veh*share_units) %>% 
  left_join(filter(mineral,Mineral %in% min_interest)) %>% 
  mutate(kg_per_LIB=kwh_veh*kg_per_kwh) %>% 
  # aggregate chemistries
  group_by(chem_scenario,capacity_scenario,c,Year,Mineral) %>% 
  reframe(kg_per_LIB=sum(kg_per_LIB)) %>% ungroup()

# EoF
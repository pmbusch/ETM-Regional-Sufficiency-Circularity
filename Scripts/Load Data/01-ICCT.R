# Load ICCT Data and merge to MONET 65 regions
# Output: Car Sales by country, year
# PBH July  2024
# Updated MAarch 2024


source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Data from ICCT - Roadmap v2.6
icct <- readxl::read_excel("Inputs/Original/2025_sales_data_updated_roadmapv2.6.xlsx")
(names(icct) <- names(icct) %>% str_replace_all(" ","_") %>%  # correct names
  str_remove_all("\\(|\\)") %>% 
  str_replace("CY","Year"))

# dictionary or equivalency for ICCT
dict <- read_excel("Inputs/Dict/Dict_Countries_ICCT_EVV_MONET.xlsx",sheet="ICCT_to_EVV") %>% 
  dplyr::select(ICCT_Region,ICCT_Country,c)
(names(dict) <- names(dict) %>% str_remove("ICCT_"))

# Process data ------
unique(icct$Powertrain)
unique(icct$Scenario)
unique(icct$Vehicle)

# collapse to 65 MONET Regions and filter
icct <- icct %>% 
  filter(Scenario=="Ambitious") %>% 
  filter(Vehicle=="Cars") %>% 
  filter(Powertrain %in% c("BEV")) %>% 
  left_join(dict)
icct %>% filter(is.na(c)) %>% pull(Country) %>% unique() # North Korea
icct <- icct %>% filter(!is.na(c))

icct <- icct %>% 
  group_by(c,Year) %>% 
  reframe(Sales=sum(Sales)) %>% ungroup()

head(icct)

unique(icct$c) # 64 unique, Taiwan missing

## Add historical sales - From EV Volumes, prior to 2024
icct <- icct %>% filter(Year>2023)
historical_sales <- read.csv("Inputs/Processed/battery_size_country.csv")
historical_sales <- historical_sales %>% 
  rename(Sales=unit) %>% dplyr::select(Year,c,Sales) %>% 
  filter(Year<2024)
historical_sales %>% group_by(Year) %>% reframe(sum(Sales)/1e6)

icct <- rbind(icct,historical_sales) %>% arrange(Year) %>% arrange(c)

# complete all data
icct <- icct %>% complete(c, Year, fill = list(Sales = 0))
nrow(icct) # 2304, 64*36

write.csv(icct,"Inputs/EV_Sales.csv",row.names = F)

icct %>% 
  filter(Year %in% c(2024,2050)) %>% 
  group_by(Year) %>% 
  reframe(Sales=sum(Sales)/1e6)

# EoF
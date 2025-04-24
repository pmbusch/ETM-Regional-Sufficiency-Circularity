### MONET
## Load all required libraries to use
## PBH Feb 2023

# Library -----
list_libraries <- c("tidyverse", "tidyverse","readr","readxl",
                    "ggplot2","data.table","dplyr","gridExtra",
                    "glmnet","openxlsx","reshape2",
                    "scales",
                    # "plotly", # sankey
                    "RColorBrewer",
                    "sf","ggrepel") # maps

# Install libraries if they are not present
# UNCOMMENT THE CODE TO INSTALL LIBRARIES THE FIRST TIME
# new_libraries <- list_libraries[!(list_libraries %in% installed.packages()[,"Package"])]
# lapply(new_libraries, install.packages)
# rm(new_libraries)

lapply(list_libraries, require, character.only = TRUE)

rm(list_libraries) 

theme_set(theme_bw(11)+ theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              axis.title.y=element_text(angle=0)))


# Load Dimensions and Levels ----------
min_interest <- c("Lithium","Nickel","Cobalt","Graphite","kWh")


# Create a named vector of colors for each region
region_colors <- c("European Union"="#a6cee3",
                   "Other Europe" = "#b2df8a",
                   "North America" = "#1f78b4",
                   # "Mexico" = "#33a02c",
                   # "Canada" = "#ff7f00",
                   # "Brazil" = "#e31a1c",
                   "South America" = "#6a3d9a",
                   # "EFTA" = "#b2df8a",
                   # "United Kingdom" = "#fb9a99",
                   "Oceania" = "#cab2d6",
                   "Asia" = "#fdb462",
                   "China" = "#ff0000",
                   # "Japan" = "#6a5acd",
                   # "ASEAN" = "#66c2a5",
                   # "India" = "#ff7f50",
                   # "Other Asia Pacific" = "#8b008b",
                   "Middle East" = "#8b4513",
                   "Africa" = "#4682b4",
                   "World"="#808080")

regionAgg_colors <- c("EU"="#a6cee3",
                   "North America" = "#1f78b4",
                   "South America" = "#6a3d9a",
                   "UK" = "#fb9a99",
                   "Australia" = "#cab2d6",
                   "South Asia" = "#fdb462",
                   "China" = "#ff0000",
                   "Japan-Korea" = "#6a5acd",
                   "India" = "#ff7f50",
                   # "Middle East" = "#8b4513",
                   # "Africa" = "#4682b4",
                   "RoW"="#808080")


# Names of scenarios
scens_names <- c("(1) Reference","(2) Bigger LIBs","(3) Smaller LIBs",
                 "(4) LFP Adoption","(5) NMC811 Adoption",
                 "(6) Enhanced Recycling")
# Color
scen_colors <- c(
  "(1) Reference" = "#808080",
  "(2) Bigger LIBs" = "#8b4513",
  "(3) Smaller LIBs" = "#0072B2",
  "(4) LFP Adoption" = "#CC79A7",
  "(5) NMC811 Adoption" = "#D55E00",
  "(6) Enhanced Recycling" = "#009E73")


dict_region <- read_excel("Inputs/Dict/Dict_Region.xlsx")


# EoF
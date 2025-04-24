# One Script to Run them all...

# Get no Trade Results as well
no_Trade <- T
# no_Trade <- F

# Get results using recycling 100%
circular <- T
# circular <- F

# Run all Demand Scenario
source("Scripts/Mineral Demand Model/01-Reference.R")
source("Scripts/Mineral Demand Model/02-BatterySize.R")
source("Scripts/Mineral Demand Model/03-BatteryChemistry.R")
source("Scripts/Mineral Demand Model/04-EndLife.R")
source("Scripts/Mineral Demand Model/05-Recycling.R")
source("Scripts/Mineral Demand Model/06-TradeRatios.R")


# EoF
## Survival Curve based on Normal Distribution
# Calculates dynamics for each region based on survival curves
# Results in detailed outflows of EVs and LIBs additional requirements,
# as well as LIB outflows to EVs, SSPS and recycling
## PBH January 2024

ev_age_newLib <- 8 # year were a new battery is needed, after that an old battery will be sufficient
# 8 years assuming a warranty over this period
max_reuse_lib <- 0.5
# Max age when an EV gets a battery, either 2-hand or new
max_ev_age <- 16
# Max age of LIB to be used in an EV
max_lib_age_ev <- 12


# Function to get flows (numbers of cars,EV,LIB) depending on the 
# vehicle and battery starting age
# Discretized by year using Normal Distribution
# n vehicles: vehicles currently on stock, 
f.getOutflows <- function(n_veh=1,EV_age,LIB_age,
                          maxEV_age=30, maxLIB_age=30,
                          dist.Age="Logistic"){
  
  # get probability of failure based on CDF of Normal
  # EV
  # y1 = pnorm(EV_age+1, mean = mean_ev, sd = sd_ev)-pnorm(EV_age, mean = mean_ev, sd = sd_ev) 
  # # LIB
  # y2 = pnorm(LIB_age+1, mean = mean_lib, sd = sd_lib)-pnorm(LIB_age, mean = mean_lib, sd = sd_lib)
  
  # option 2: get fraction year to year of survival, based on CDF ratios
  # represent proportion that survives year to year
  
  if(dist.Age=="Normal"){
    y1 = (1-pnorm(EV_age+1, mean = mean_ev, sd = sd_ev))/
      (1-pnorm(EV_age, mean = mean_ev, sd = sd_ev))
    y2 = (1-pnorm(LIB_age+1, mean = mean_lib, sd = sd_lib))/
      (1-pnorm(LIB_age, mean = mean_lib, sd = sd_lib))
  } else{ # Logistic
    y1 = (1-plogis(EV_age+1, mean_ev, sd_ev*sqrt(3)/pi))/ # CONVERT SCALE TO Stand Dev.
      (1-plogis(EV_age, mean_ev, sd_ev*sqrt(3)/pi))
    y2 = (1-plogis(LIB_age+1, mean_lib, sd_lib*sqrt(3)/pi))/
      (1-plogis(LIB_age, mean_lib, sd_lib*sqrt(3)/pi))
  }
  
  
  # max age
  if(EV_age>=maxEV_age) {y1 = 0}
  if(LIB_age>=maxLIB_age) {y2 = 0}
  
  # # get probabilities as independent events
  # ret <- tibble(
  #   both_fail=y1*y2*n_veh,
  #   ev_fail=y1*(1-y2)*n_veh,
  #   lib_fail=(1-y1)*y2*n_veh,
  #   none=(1-y1)*(1-y2)*n_veh) # none fails
  
  # case 2 - independent events to get proportions into 4 cases
  ret <- tibble(
    both_fail=(1-y1)*(1-y2)*n_veh,
    ev_fail=(1-y1)*y2*n_veh,
    lib_fail=y1*(1-y2)*n_veh,
    none=y1*y2*n_veh)
  
  return(ret)
}


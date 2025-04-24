# Mineral Demand Function - for each scenario
# PBH July 2024
# Updated: January 2025 for inputs that change over time (indexed by year)


# ++++++++++++++++++++++++++++++++++
# DEMAND LOOP MODEL  ---------
# ++++++++++++++++++++++++++++++++++

# Function Inputs
# icct: EV demand vector, must be from 2015 to 2050, indexed by country and year
# mineral_perLIB: Kg of mineral per EV or LIB, by country and year
# mat_recovery_recycling: Recovery material assumptions, per country and year
# Rest are default
# Bools indicate whether to use trade ratios to change country of production (MONET) or recycling (2-Hand)
# Recycling flow: from available LIBs, which % goes to recycling (instead of grid storage)
# Output: Dataframe of demand by country, in TONS for minerals, and in MWh for battery
f.MineralDemand <- function(icct,
                            mineral_perLIB,
                            mat_recovery_recycling,
                            min_interest=c("Lithium","Nickel","Cobalt","Graphite","kWh"),
                            considerMONET=T,
                            consider2HandTrade=T,
                            recycling_flow=1){
  
  countries <- unique(icct$c) # 64
  
  # Survival part with matrices - vehicle age and battery age
  # Third dimension are countries
  matrix_data <- array(0, c(length(countries),31, 31))
  dimnames(matrix_data)[[1]] <- countries
  dimnames(matrix_data)[[2]] <-paste0("EV_",0:30) # ROWS are EV
  dimnames(matrix_data)[[3]] <- paste0("LIB_",0:30) # COLS are Battery
  
  
  # results
  df_final <- c()
  start_time <- proc.time()
  icct <- icct %>% arrange(c)
  # range(icct$Year) 
  
  for (yr in 2015:2050){
    cat(yr,"\n")
    
    # Sales at that year
    sales_aux <- icct %>% filter(Year==yr)
    
    # Minerals calculated based on LIB size of country of Sales
    df <- sales_aux %>% 
      left_join(mineral_perLIB,by=c("c","Year")) %>%  
      mutate(EV_Production=Sales*kg_per_LIB/1e3) # in tons
    
    # Estimate production - MONET
    if(considerMONET){
      aux <- c()
      for (m in min_interest){
        prod_aux <- df %>% filter(Mineral==m) %>% arrange(c)
        prod_aux <- f.TradeRatios(prod_aux$EV_Production)
        prod_aux <- tibble(c=names(prod_aux),EV_Production=prod_aux,Mineral=m)
        aux <- rbind(aux,prod_aux)
      }
      # Minerals are allocated to country of production
      df <- df %>% 
        dplyr::select(-EV_Production) %>% 
        left_join(aux,by=c("c","Mineral"))
      rm(aux)
    } 
    
    
    # EV and LIB Failure modelling, for each country
    EV_Stock <- rep(0,length(countries))
    names(EV_Stock) <- countries
    LIB_Available <- EV_Stock2<- LIB_reuse_EV <- EV_Stock
    # matrix by mineral
    add_LIB <- matrix(0,nrow=length(countries),ncol=length(min_interest))
    rownames(add_LIB) <- countries;colnames(add_LIB) <- min_interest
    LIB_recycling <- add_LIB2 <- LIB_recycling2 <- add_LIB # second hand
    # Loop by country
    for (ct in countries){
      # Assign new sales to top left cuadrant (0,0)
      matrix_data[ct,1, 1] <- sales_aux %>% filter(c==ct) %>% pull(Sales)
      
      # clear stock of 10 or less batteries or EVs
      matrix_data[matrix_data < 10] <- 0
      
      # Get new matrix of EV stock with ages, LIBs in good use 
      matrix_lib <- array(0, c(length(countries),31, 31))
      dimnames(matrix_lib)[[1]] <- countries
      dimnames(matrix_lib)[[2]] <-paste0("EV_",0:30) # ROWS are EV
      dimnames(matrix_lib)[[3]] <- paste0("LIB_",0:30) # COLS are Battery
      new_matrix <- matrix_ev <- matrix_lib
      
      # Iterate through the matrix
      for (i in 1:31) { # EV
        for (j in 1:31) { # LIB
          if (matrix_data[ct,i, j] != 0) {
            result <- f.getOutflows(matrix_data[ct,i, j],i-1,j-1) # age is minus 1 for the index
            if (i!=31 & j!=31){ # to avoid border case
              new_matrix[ct,i + 1, j + 1] <- result$none # move 1 age for both EV and LIB
              matrix_ev[ct,i+1,j+1] <- result$lib_fail+result$both_fail # EVs that need LIB
              matrix_lib[ct,i+1,j+1] <- result$ev_fail # LIBs available to use
            } else if (j==31 & i!=31){ # BATTERIES TOO OLD
              matrix_ev[ct,i+1,j] <- result$lib_fail+result$both_fail # EVs that need LIB, no LIBs available as they died
            } else if (j!=31 & i==31){ # EV TOO OLD
              matrix_lib[ct,i,j+1] <- result$ev_fail # LIBs available to use, no EV at border
            }
          }
        }
      }
      # get vector of outflows of EV and outflows of LIBs
      ev_need <- rowSums(matrix_ev[ct,,])
      
      # Above certain age simply no LIB required, THEY DIED
      ev_need[(max_ev_age+1):31] <- 0
      
      # LIBs that failed
      lib_failed <- colSums(matrix_ev[ct,,])
      # EVs that failed
      lib_available <- colSums(matrix_lib[ct,,]) 
      
      # assigning old batteries TO EVs within the country
      lib_to_EV <- lib_available*max_reuse_lib
      # limit age of LIB for EV
      lib_to_EV[(max_lib_age_ev+1):31] <- 0
      
      lib_available <- lib_available-lib_to_EV
      
      # first match year to year with offset of years - 8 years
      ev_need <- c(ev_need,rep(0,ev_age_newLib))
      lib_to_EV <- c(rep(0,ev_age_newLib),lib_to_EV)
      allocation <- pmin(ev_need,lib_to_EV)
      
      ev_need <- ev_need - allocation
      lib_to_EV <- lib_to_EV - allocation
      
      # remove offsets
      ev_need <- ev_need[1:31]
      lib_to_EV <- lib_to_EV[-(1:ev_age_newLib)]
      allocation <- allocation[-(1:ev_age_newLib)]
      
      # update new_matrix with stock of EVs and old batteries
      for (i in 1:(31-ev_age_newLib)){
        new_matrix[ct,i+ev_age_newLib,i] <- new_matrix[ct,i+ev_age_newLib,i]+allocation[i]
      }
      
      allocation <- sum(allocation)
      
      # do rest of allocation with LOOP
      start_bat <- 1
      for (i in 31:1) { # start with old
        if (i<=ev_age_newLib){
          # new_matrix[i,0] <- ev_need[i] # new battery DUPLICATED
        } else {
          for (j in start_bat:31) {
            allocated <- min(ev_need[i], lib_to_EV[j])
            ev_need[i] <- ev_need[i] - allocated
            lib_to_EV[j] <- lib_to_EV[j] - allocated
            # update new_matrix with stock of EVs and old batteries
            new_matrix[ct,i,j] <- new_matrix[ct,i,j]+allocated
            allocation <- allocation+allocated
            start_bat <- j
            if (ev_need[i] == 0) { break }
          }
        }
      }
      
      # add remaining batteries back to pool
      lib_available <- lib_available+lib_to_EV
      
      # add EVs with new batteries to stock - note, no other battery with 0 age
      new_matrix[ct,,1] <-  ev_need
      
      # Calculate Minerals - based on Years
      
      # SUMPRODUCT, shifting vectors to corresponding years
      # vector of mineral content per year 
      
      # 2-Hand Trade part - Split fleet before and after age 8
      # only works with static 2-hand Trade ratios
      # need to do mineral calculation first - to do shift right
      
      # to get vehicles age 0 to 8, proper index
      if (yr<2023){
        age8_index <- 1
        age0_index_end <- 8
      } else {
        age8_index <- yr-2015-8+1
        age0_index_end <- yr-2015
      }
      
      # Loop for each mineral
      for (m in min_interest){
        # Kg of mineral per LIB or EV
        mineral_year <- mineral_perLIB %>% 
          filter(c==ct,Mineral==m) %>% 
          pull(kg_per_LIB)
        
        # Additional LIBS
        add_lib_aux <- shift_X(yr,ev_need)*mineral_year/1e3 #tons
        add_LIB[ct,m] <- sum(add_lib_aux[age8_index:age0_index_end]) # age 0 to 8
        add_LIB2[ct,m] <- sum(add_lib_aux[-(age8_index:age0_index_end)]) # age >8
        
        # Recycling - x% flows goes to recycling, rest to grid storage
        recyc <- lib_failed+lib_available*recycling_flow
        
        LIB_recycling_aux <- shift_X(yr,recyc)*mineral_year/1e3 # tons
        LIB_recycling[ct,m] <- sum(LIB_recycling_aux[age8_index:age0_index_end]) # age 0 to 8
        LIB_recycling2[ct,m] <- sum(LIB_recycling_aux[-(age8_index:age0_index_end)]) # age >8 
      }
      
      EV_Stock[ct] <- round(sum(new_matrix[ct,1:9,]),0)
      EV_Stock2[ct] <- round(sum(new_matrix[ct,10:31,]),0)
      
      
      # store matrix 
      matrix_data[ct,,] <- new_matrix[ct,,]
      
      # end for loop, next year
      # keep balance of removed EV Sales from stock
      rm(new_matrix,matrix_ev,matrix_lib,lib_to_EV,lib_available,allocated,allocation,start_bat)
      
    } # End Loop countries
    
    # 2 hand flows allocation - vehicles age 8 and older
    # Each country has fleet matrix always, I move the age 8 older later
    
    # Additional LIBs and recycling Minerals go to new country
    if(consider2HandTrade){
      for (m in min_interest){
        add_LIB2[,m] <- f.TradeRatios(add_LIB2[,m],MONET = F)
        LIB_recycling2[,m] <- f.TradeRatios(LIB_recycling2[,m],MONET = F)
        EV_Stock2 <- f.TradeRatios(EV_Stock2,MONET = F)
      }
    } 
    
    # Add them all to df - convert to tibble and flatten
    add_LIB <- as.data.frame(add_LIB) %>% rownames_to_column() %>% rename(c=rowname) %>%  
      pivot_longer(c(-c), names_to = "Mineral", values_to = "add_LIB")
    add_LIB2 <- as.data.frame(add_LIB2) %>% rownames_to_column() %>% rename(c=rowname) %>%  
      pivot_longer(c(-c), names_to = "Mineral", values_to = "add_LIB2")
    LIB_recycling <- as.data.frame(LIB_recycling) %>% rownames_to_column() %>% rename(c=rowname) %>%  
      pivot_longer(c(-c), names_to = "Mineral", values_to = "LIB_recycling")
    LIB_recycling2 <- as.data.frame(LIB_recycling2) %>% rownames_to_column() %>% rename(c=rowname) %>%  
      pivot_longer(c(-c), names_to = "Mineral", values_to = "LIB_recycling2")
    EV_Stock <- tibble(c=names(EV_Stock),EV_Stock=EV_Stock)
    EV_Stock2 <- tibble(c=names(EV_Stock2),EV_Stock2=EV_Stock2)
    
    df <- df %>% 
      left_join(add_LIB,by=c("c","Mineral")) %>% 
      left_join(add_LIB2,by=c("c","Mineral")) %>%
      left_join(LIB_recycling,by=c("c","Mineral")) %>%
      left_join(LIB_recycling2,by=c("c","Mineral")) %>% 
      left_join(EV_Stock,by=c("c")) %>%
      left_join(EV_Stock2,by=c("c"))
    
    # Consider recycling recovery rates
    recyc_aux <- mat_recovery_recycling %>% 
      filter(Year==yr) %>% 
      rename(recovery=mat_recov_recyc)
    df <- df %>% left_join(recyc_aux,by=c("c","Year","Mineral"))
    
    
    df <- df %>% 
      mutate(LIB_recycling=LIB_recycling*recovery,
             # 2-hnad is recycled with the country of destination
             LIB_recycling2=LIB_recycling2*recovery)
    
    # Join
    df_final <- rbind(df_final,df)
    rm(sales_aux,prod_aux,df,recyc_aux)
  }  
  end_time <- proc.time() # Capture the ending time
  cat("Running time: ",end_time - start_time,"\n") # 2 min for one scenario
  
  # Re-format database
  # names(df_final)
  df <- df_final %>%
    # filter(Year>2023) %>% # 2024 to 2050
    # join mineral source from 2-hand
    mutate(add_LIB=add_LIB+add_LIB2,
           LIB_recycling=-(LIB_recycling+LIB_recycling2),
           EV_Stock=EV_Stock+EV_Stock2) %>% 
    # consider scrap, and also that could go towards recycling with recovery rate %
    mutate(EV_Production=EV_Production/(1-cathode_scrap),
           add_LIB=add_LIB/(1-cathode_scrap),
           scrap_rec=-(EV_Production+add_LIB)*cathode_scrap*recovery) %>%
    mutate(LIB_recycling=LIB_recycling+scrap_rec) %>% 
    dplyr::select(c,Year,Mineral,EV_Production,add_LIB,LIB_recycling) %>% 
    pivot_longer(-c(c,Year,Mineral), names_to = "Sector", values_to = "tons") %>% 
    filter(abs(tons)>0)
  
  # Delay in recycling
  df <- df %>% 
    mutate(Year=if_else(Sector=="LIB_recycling",Year+delay_recycling_year,Year)) %>% 
    filter(Year<2051)
  
  return(df)
}






# EoF
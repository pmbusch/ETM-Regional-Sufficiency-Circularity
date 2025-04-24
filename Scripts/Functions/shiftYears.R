# convert vector of age with reference year to vector of years 
# Define a custom function to calculate Y for each row
# Function simply moves with the vector indexing
# Reference year is always 2015, and the length is up to 2050 (36 of size)
# x is always 30 years of length


shift_X <- function(starting_year, x) {
  x=unname(x)
  shift <- starting_year-2015+1
  # x=x/sum(x) # do relative terms for flows inside X
  y = x[shift:1]
  if(starting_year<2045){
    y[1] <- y[1]+sum(x[-(1:shift)]) # add prior to 2015
  } else {
    # complete vector before and after, as in 2045 all 2015 cars are gone, for example
    y[1:(starting_year-2045+1)] <- 0 # early than 2015
  }
  # complete vector towards 2050
  if (shift<36){ # avoid border car
    y[(shift+1):36] <- 0 # 36 is 2015 to 2050
  }
  
  return(y) # vector from 2015 to 2050
}

################################################################################
#                       NORBERG (INDIRECT) MODEL - TRAIT                       #
################################################################################

# Created by: Sarah Rauf 
# Date: 2026-04-19 

# Description: this function generates traits (Topt, rmax, Tmax, Tmin) using the  
#              norberg model (bootstrap approach), using growth rate estimates 

# a = a, b = b, z = tref, w = c

norberg_indirect_trait_func <- function(z, w, a, b) {
  
  ###################### estimating topt & rmax ##########################
  temp_grid <- seq(3, 45, length.out = 999) 
  
  # 1. Topt via optim (JB's OG code) (modified slightly)
  grfunc<-function(x){
    -nbcurve(x, z = z[[1]],w = w[[1]],a = a[[1]],b = b[[1]])
  }
  
  optinfo <- optim(c(x=z[[1]]),grfunc)
  maxgrowth <- c(-optinfo$value)
  topt <- c(optinfo$par[[1]])
  

  ## throwback to this code 
  growth_rate_model <- function(temp_grid, z, w, a, b) {
    r <- a * exp(b * temp_grid) * (1 - ((temp_grid - z) / (w / 2))^2)
  }
  
  # calculating growth rates across temp grid for each param row
  rates <- growth_rate_model(temp_grid, z, w, a, b)
  
  # 2. Rmax 
  rmax <- max(rates, na.rm = TRUE)
  
  # 3A. Tmin, Tmax = temps when growth rate ~ 20% max
  threshold <- 0.2 * rmax
  viable_temps <- temp_grid[rates >= threshold]
  
  if (length(viable_temps) == 0) {
    return(c(tmin = NA, topt = topt, tmax = NA))  # if no values found
  }
  
  tmin_0.2rmax <- min(viable_temps)
  tmax_0.2rmax <- max(viable_temps)
  
  
  # 3B. Tmin, Tmax = temps when growth rate ~ 0% max
  threshold <- 0
  viable_temps <- temp_grid[rates >= threshold]
  
  if (length(viable_temps) == 0) {
    return(c(tmin = NA, topt = topt, tmax = NA))  # if no values found
  }
  
  tmin_0rmax <- min(viable_temps)
  tmax_0rmax <- max(viable_temps)
  
  return(c(topt = topt, rmax = maxgrowth, 
           tmin_0.2rmax = tmin_0.2rmax, tmax_0.2rmax = tmax_0.2rmax,
           tmin_0rmax = tmin_0rmax, tmax_0rmax = tmax_0rmax))
  
}

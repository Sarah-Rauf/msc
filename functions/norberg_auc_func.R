################################################################################
#                     NORBERG (INDIRECT) MODEL - AUC CALC                       #
################################################################################

# Created by: Sarah Rauf 
# Date: 2026-04-26

# Description: this function generates auc using the params generated from  
#              norberg model (bootstrap approach), using growth rate estimates 

norberg_auc_func <- function(boot_params) {
  
  library(gcplyr) # for auc() (for calculating AUC) 
  
  # a = a, b = b, z = tref, w = c
  
  # creating temp grid df & norberg growth rate model func
  temp_grid <- seq(3, 45, length.out = 999) 
  
  norberg_gr_model <- function(temp_grid, z, w, a, b) {
    r <- a * exp(b * temp_grid) * (1 - ((temp_grid - z) / (w / 2))^2)}
  
  calc_auc <- function(preds, temp_grid){
    
    y <- as.vector(preds) %>% unlist() %>% as.numeric()
    x <- as.vector(temp_grid) %>% unlist() %>% as.numeric()
  
    auc_value <-
      auc(y, x,
          xlim = NULL, blank = 0, subset = NULL, na.rm = TRUE, neg.rm = FALSE,
          warn_xlim_out_of_range = TRUE, warn_negative_y = TRUE) %>% 
      as.data.frame() %>% 
      rename(auc = ".") # rename column to "auc"
    
    return(auc_value)
  } # end of calc_ea_from_slope func
  
  # removing "treatment" column from params 
  norberg_params <- boot_params %>% select(z, w, a, b)
  
  # no. of rows in boot_params so the "for" loop runs for each row, and 
  # populates the "ea_df" (which I'm creating here)
  nboot <- nrow(boot_params) 
  auc_df <- numeric(nboot)
  
  # for each row...
  for (i in 1:nboot) {
    
    pars <- norberg_params[i, ] # take the params and run them through... 
    
    preds <- norberg_gr_model( # norberg model to generate preds for one tpc
      temp_grid,
      a = pars$a,
      b = pars$b,
      w = pars$w,
      z = pars$z
    )
    
    auc_df[i] <- calc_auc(preds, temp_grid) # estimate ea and put into ea_df 
  }
  
  
  return(auc_df)
  
} # end of norberg_indirect_ea_func

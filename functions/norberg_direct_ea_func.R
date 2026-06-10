################################################################################
#               ESTIMATION OF EA FROM NORBERG (INDIRECT) MODEL                 #
################################################################################

# Created by: Sarah Rauf 
# Date: 2026-04-20 

# Description: this function generates ea using the norberg model
#              (bootstrap approach) 

# Note: 

norberg_direct_ea_func <- function(boot_params) {
  
  
  
  # creating temp grid df & norberg growth rate model func
  temp_grid <- seq(3, 45, length.out = 999) 
  norberg_gr_model <- function(temp_grid, z, w, a, b) {
    r <- a * exp(b * temp_grid) * (1 - ((temp_grid - z) / (w / 2))^2)}
  
  calc_ea_from_slope <- function(df) {
    
    threshold_low  <- 0.2 * max(preds)
    threshold_high <- 0.8 * max(preds)
    topt <- temp_grid[which.max(preds)]
    
    # filter rising limb
    regression_df <- df %>%
      dplyr::filter(temp_grid < topt) %>% 
      dplyr::filter(preds >= threshold_low, preds <= threshold_high) %>%
      dplyr::mutate(
        lnpreds = log(preds),
        one_kt = 1/(0.00008617333*(temp_grid+273))
      )
    
    # replace INFs with NAs 
    regression_df[is.na(regression_df) | regression_df=="-Inf" | regression_df == "Inf"] = NA
    
    # regression
    lm_fit <- lm(lnpreds ~ one_kt, data = regression_df)
    
    # obtain ea (slope of regression) and multiply with -1 
    ea_slope <- coef(lm_fit)[2]*-1
    
    return(ea_slope)
    
  } # end of calc_ea_from_slope func
  
  # removing "treatment" column from params 
  norberg_params <- boot_params %>% select(z, w, a, b)
  
  # no. of rows in boot_params so the "for" loop runs for each row, and 
  # populates the "ea_df" (which I'm creating here)
  nboot <- nrow(boot_params) 
  ea_df <- numeric(nboot)
  
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
    
    preds_df <- data.frame(preds, temp_grid) # take these preds, join with temp
    
    ea_df[i] <- calc_ea_from_slope(preds_df) # estimate ea and put into ea_df 
  }
  
  
  return(ea_df)
  
} # end of norberg_indirect_ea_func

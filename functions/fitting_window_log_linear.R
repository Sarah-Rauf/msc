################################################################################
################### Fitting window log linear function #########################
################################################################################

# Created by: unknown 
# Date created: unknown 

# Note from Sarah Rauf: received this code from Joey Bernhardt 

# Description: this function identifies which points capture exponential growth 
# by calc slope between points 1-2, 1-3, 1-4, ...

# function called "fitting_window_log_linear" 
# function(x) = takes one argument called x 
# x represents how many time points (or sampling intervals) should be used 
# when fitting the linear model 
fitting_window_log_linear <- function(x) {
  
  # safely handle errors. If an error occurs during model fitting, it will 
  # print a message instead of stopping the entire script
  tryCatch({
    growth_rates <- time_temp_rfu_2nd %>%
      
      # Groups the data by temp (temperature), treatment (e.g., concentration), 
      # and well. This means models will be fit within each group
      group_by(temp, treatment, plate_well) %>%
      
      # Selects the earliest x time points (e.g., days 1 to 5 if x = 5) for 
      # each group. 
      # top_n(n = -x, wt = days) gets the bottom x rows based on days
      top_n(n = -x, wt = days) %>%
      
      # Fits a linear model of log(RFU) vs days for each group using only the 
      # selected early time points. Then tidy() from the broom package extracts 
      # clean model output (like slope, p-value, etc.).
      # log(data) assumes the RFU values are stored in a column named data
      # do() is used to apply the model to each group and tidy the results
      do(tidy(lm(log(data) ~ days, data = .))) %>%
      
      # Adds a column to the result recording how many time points (x) were 
      # used to fit the model
      mutate(number_of_points = x) %>%
      
      # removes grouping so final output is a nice clean data frame 
      ungroup()
    
    # Returns the dataframe of model results for each group
    return(growth_rates)
    
    # If there's any error in the above block (e.g., missing data, not enough 
    # points to fit a model), this part prints a message like:
    # "An error occurred in fitting_window_log_linear: <error>". AND
    # Returns NULL instead of crashing the script.
  }, error = function(e) {
    message("An error occurred in fitting_window_log_linear: ", e$message)
    return(NULL)
  })
}


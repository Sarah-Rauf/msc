################################################################################
################# Safe Nonlinear Model Fitting of RFU data #####################
################################################################################

# Created by: unknown 
# Date created: unknown 

# Note from Sarah: Received this code from Joey Bernhardt 

# Description: the `safe_nls_fit()` takes a data frame (`df`) as 
# input. This function fits a nonlinear model fit to one group of data 
# (e.g., one `well` × `temp` × `treatment`).

safe_nls_fit <- function(df) {
  
  # Wraps the fitting process in a tryCatch(), which catches any errors
  tryCatch({
    
    # fitting an exponential growth model 
    tidy(nlsLM(data ~ N0 * exp(r * days),
               data = df,
               start = list(r = 0.1),
               control = nls.lm.control(maxiter = 2000, ftol = 1e-10, ptol = 1e-10)))
    
    # prints error message (not fatal to the script) 
  }, error = function(e) {
    message("Error in fitting model for group: ", e$message)
    
    # if error occurs, a dummy row of NAs is returned 
    return(tibble(term = NA, estimate = NA, std.error = NA, 
                  statistic = NA, p.value = NA))
  })
}

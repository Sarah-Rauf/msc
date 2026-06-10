################################################################################
#                       Growth data processing & analysis                      # 
################################################################################
# Created by: Sarah Rauf 


# Description: processing and analysis of growth data 



############################## Relevant packages ###############################
## data wrangling 
library(tidyverse) # everywhere  
library(readxl) # pull out times and RFU/OD data from excel files

## data import
library(here) # to pull "plate_layout.csv" from "processed_data" folder
library(ggpubr) # for making figures, it has ggarrange which I like 

## data analysis 
library(minpack.lm) # bootstrapping 
library(car) # bootstrapping
library(boot) # bootstrapping CIs for thermal traits 
library(broom) # turns outputs of lm/nls/t.test into tidy tibbles 
library(nls.multstart) # for logistic TPCs, specifically nls_multstart()

############################# Processing Data ##################################
################################ load data #####################################
RFU_files <- c(list.files("2025.05.08_cnplimit_attempt1_data", full.names = TRUE))

names(RFU_files) <- 
  RFU_files %>% gsub(pattern = "2025.05.08_cnplimit_attempt1_data/", replacement = "") %>% 
  gsub(pattern = ".xlsx", replacement = "")

############################ Pulling out times #################################
all_times <- 
  map_df(RFU_files, read_excel, range = "A6:B8", .id = "file_name") %>% 
  rename(data = "Plate 1") %>%
  pivot_wider(names_from = "Plate Number", values_from = data) %>% 
  mutate(Time = ymd_hms(Time)) %>% 
  separate(Time, into = c("Wrong_Date", "Time"), sep = " ") %>% 
  select(-Wrong_Date) %>% 
  mutate(date_time = ymd_hms(date_time)) 

################## Combining Plate Reads & Plate Layout ########################
# pulling RFU data into df 
all_plate_reads <- 
  map_df(RFU_files, read_excel, range = "B36:I44", .id = "file_name") %>% 
  rename(row = `...1`) %>% rename(readtype = `...8`) %>% mutate(row = zoo::na.locf(row))

# combining rfu data with time 
plate_data_time <- 
  dplyr::left_join(all_plate_reads, all_times, by = "file_name") %>% 
  pivot_longer(., cols = c("1", "2", "3", "4", "5", "6"), 
               names_to = "well_number", values_to = "data") %>%
  unite(well, row, well_number, sep = "")

# reconfiguring file name so it can be joined with plate layout 
# creating a new column with the incubator's actual temperatures 
plate_data_time_platewell <- 
  plate_data_time %>% separate(file_name, into = c("initials", "plate.temp", 
                                                   "plate_no","time_sampling_interval"), sep = "_", remove = FALSE) %>% 
  separate(time_sampling_interval, into = c("tim", "sampling_interval"), sep ="e") %>%
  separate(plate.temp, into = c("plat", "temp"), sep ="e") %>% 
  select(-c(plat, tim, initials, plat)) %>% 
  unite(plate_well, plate_no, well) %>% 
  rename(incubator_temp = temp) %>%  
  mutate(temp = ifelse(incubator_temp == 6, 6.72, ifelse(incubator_temp == 12, 12.4,
                                                         ifelse(incubator_temp == 18, 18.44, ifelse(incubator_temp == 24, 23.95, 
                                                                                                    ifelse(incubator_temp == 30, 29.82, ifelse(incubator_temp == 36, 34.79, 
                                                                                                                                               ifelse(incubator_temp == 38, 36.49, ifelse(incubator_temp == 40, 38.74, 
                                                                                                                                                                                          ifelse(incubator_temp == 42, 41.11, ifelse(incubator_temp == 44, 42.38, NA))))))))))) 

####################### load plate layout excel sheet ##########################
plate_layout <- 
  read_csv(here("processed_data", "may22025_plate_layout.csv")) %>% 
  unite(plate_well, plate, well, remove = FALSE)

############### merge plate layout & plate reads using left_join ###############
plate_data_with_plate_layout <- left_join(plate_data_time_platewell, plate_layout, by="plate_well") 

all_rfu_data <- 
  plate_data_with_plate_layout %>% filter(readtype == "Read 1:440,670") %>% 
  filter(!str_detect(treatment, "COMBO")) 

########################## adding days column to df ############################
time_temp_rfu <- 
  
  all_rfu_data %>% 
  mutate(start_time = min(date_time)) %>% 
  mutate(days = interval(start_time, date_time)/ddays(1)) %>% 
  unite(temp_plate_well, temp, plate_well, sep = "_", remove = FALSE) %>%
  filter(!temp_plate_well %in% c("6_4_D5", "12_4_D5", "18_4_D5", "24_1_D1", "38_3_D5")) %>% 
  mutate(treatment = ifelse(treatment == "A_1000N_20C_50P", "A", 
                            ifelse(treatment == "B_1000N_2C_50P", "B", ifelse(treatment == "C_1000N_0C_50P", "C",
                                                                              ifelse(treatment == "D_1000N_20C_5P", "D", ifelse(treatment == "E_1000N_2C_5P", "E",
                                                                                                                                ifelse(treatment == "F_1000N_0C_5P", "F", ifelse(treatment == "G_50N_20C_50P", "G",
                                                                                                                                                                                 ifelse(treatment == "H_50N_2C_50P", "H", ifelse(treatment == "I_50N_0C_50P", "I",
                                                                                                                                                                                                                                 ifelse(treatment == "J_50N_20C_5P", "J", ifelse(treatment == "K_50N_2C_5P", "K",
                                                                                                                                                                                                                                                                                 ifelse(treatment == "L_50N_0C_5P", "L", "WRONG"))))))))))))) %>% 
  mutate(sampling_interval = as.numeric(sampling_interval) + 1) %>% 
  filter(data > 0)

# identifying exponential growth and filtering out rfu that is not exponential #

### Creates a dataframe that selects the best (highest) growth rate
x = 2
windows <- seq(1,10, by = 1)

source(here::here("functions", "fitting_window_log_linear.R"))

multi_fits <- windows %>% map_df(fitting_window_log_linear, .id = "iteration")

# how growth rate estimates (slope of log RFU ~ days) vary 
# across window sizes, split by temperature and treatment.
multi_fits %>% filter(term == "days") %>% ggplot(aes(x = number_of_points, y = estimate, 
                                                     group = treatment, colour = treatment)) + geom_point() + facet_wrap( ~ temp, scales = "free")

# Selects the best growth rate for each replicate well (max slope across window sizes) (top_n)
exp_fits_top <- multi_fits %>% filter(term == "days") %>% 
  group_by(treatment, temp, plate_well) %>%  top_n(n = 1, wt = estimate)

# Plot of the window size that gave the best growth rate estimate across 
# the wells for each temperature*treatment combination
exp_fits_top %>% ggplot(aes(x = number_of_points, y = estimate, 
                            group = treatment, colour = factor(temp))) + geom_point() +
  facet_grid(temp ~ treatment, scales = "free")


# (1) join lm() results with OG data set, (2) ensure at least 3 time points 
# are used for filtering (3) filter df to include exponential data only  
all_data <- left_join(time_temp_rfu, exp_fits_top) %>% # (1)
  mutate(keep_data = ifelse(number_of_points <= 3, 3, number_of_points)) %>% # (2)
  filter(sampling_interval <= keep_data) # (3)

# (1) join lm() results with OG data set, (2) ensure at least 3 time points 
# are used for filtering (3) filter df to include exponential data only 

# this data includes all RFU data for temperatures 6.72, 41.1, 42.38, and 
# only exponential RFU data for every other temperature 
all_data_filtered <- left_join(time_temp_rfu, exp_fits_top) %>% # (1)
  mutate(keep_data = ifelse(number_of_points <= 3, 3, number_of_points)) %>% # (2)
  mutate(keep_data = ifelse(temp %in% c(12.4, 18.44, 23.95, 29.82, 34.79, 36.49, 38.74), keep_data, 10)) %>% 
  filter(sampling_interval <= keep_data)

# visualize early-phase growth data
all_data %>% 
  ggplot(aes(x = days, y = data, color = treatment, group = plate_well)) + 
  geom_point(size = 2) +
  facet_grid(treatment ~ temp, scales = "free_y") + geom_line()

all_data_filtered %>% 
  ggplot(aes(x = days, y = data, color = treatment, group = plate_well)) + 
  geom_point(size = 2) +
  facet_grid(treatment ~ temp, scales = "free_y") + geom_line()



# creating a new column called N0 (minimum RFU value of well*temp*treatment) 
all_data2  <- all_data %>% 
  group_by(plate_well, temp, treatment) %>% 
  mutate(N0 = min(data))

all_data_filtered2  <- all_data_filtered %>% 
  group_by(plate_well, temp, treatment) %>% 
  mutate(N0 = min(data))

# calling functions
source(here::here("functions", "safe_nls_fit.R"))

# applies safe_nls_fit() to each group (fits an exponential growth model: data ~ N0 * exp(r*days))
model_fit <- all_data2 %>%
  group_by(plate_well, temp, treatment) %>%
  group_modify(~ safe_nls_fit(.x)) 

model_fit_filtered <- all_data_filtered2 %>%
  group_by(plate_well, temp, treatment) %>%
  group_modify(~ safe_nls_fit(.x)) 

# plotting this 
model_fit %>% 
  ggplot(aes(x = temp, y = estimate, color = treatment)) + geom_point() +
  facet_grid( ~ treatment)

model_fit_filtered %>% 
  ggplot(aes(x = temp, y = estimate, color = treatment)) + geom_point() +
  facet_grid( ~ treatment)



############################# FINAL DATA FRAMES ################################
# relative fluorescence units df (includes only exponential RFU)
rfu_df <- all_data %>% select(plate_well, sampling_interval, data, temp, 
                              treatment, days, number_of_points, keep_data) 

# relative fluorescence units df (includes only exponential RFU except for 
# 6.72, 41.11, 42.38 where all RFU data was kept)
rfu_df_filtered <- all_data_filtered %>% 
  select(plate_well, sampling_interval, data, temp, 
         treatment, days, number_of_points, keep_data) 

# growth rate df (growth rates estimated using only exponential RFU) 
gr_df <- model_fit %>% select(plate_well, temp, treatment, estimate) %>% 
  rename(rate = estimate) %>% ungroup() %>% filter(!is.na(rate))

# growth rate df (growth rates estimated using only exponential RFU except for 
# 6.72, 41.11, 42.38 where all RFU data was kept)
gr_df_filtered <- model_fit_filtered %>% 
  select(plate_well, temp, treatment, estimate) %>% 
  rename(rate = estimate) %>% ungroup() %>% filter(!is.na(rate))


write_csv(rfu_df, here::here("processed_data", "2026.05.01", "rfu_unfiltered_df.csv"))
write_csv(rfu_df_filtered, here::here("processed_data", "2026.05.01", "rfu_filtered_df.csv"))






################ PLOTS FOR SUPPLEMENTARY FIGURE (RFU VS. TIME) #################
rfu_time_series_plot <- 
  time_temp_rfu %>% 
  group_split(treatment) %>% 
  map(~ ggplot(.x, aes(x = days, y = data, group = plate_well)) +
        geom_point() +
        facet_wrap(~temp, scales = "free") +
        theme_classic() + 
        theme(legend.position = "none") 
  )

# A/1, B/2, C/3, D/4, E/5, F/6, G/7, H/8, I/9, J/10, K/11, L/12

# Bootstrap (Norberg direct) using exp data for temps 10-40, all data for <10 & > 40
```{r}
dataframe_A <- rfu_df_filtered %>% filter(treatment == "A")
dataframe_B <- rfu_df_filtered %>% filter(treatment == "B")
dataframe_C <- rfu_df_filtered %>% filter(treatment == "C")
dataframe_D <- rfu_df_filtered %>% filter(treatment == "D")
dataframe_E <- rfu_df_filtered %>% filter(treatment == "E")
dataframe_F <- rfu_df_filtered %>% filter(treatment == "F")
dataframe_G <- rfu_df_filtered %>% filter(treatment == "G")
dataframe_H <- rfu_df_filtered %>% filter(treatment == "H")
dataframe_I <- rfu_df_filtered %>% filter(treatment == "I")
dataframe_J <- rfu_df_filtered %>% filter(treatment == "J")
dataframe_K <- rfu_df_filtered %>% filter(treatment == "K")
dataframe_L <- rfu_df_filtered %>% filter(treatment == "L")




################# Bootstrapping treatment (direct approach) ####################
library(car) # Boot() 

temp_seq <- seq(3, 45, length.out = 100)

# step 3: define growth rate model (Norberg) 
growth_rate_model <- function(temp, z, w, a, b) {
  r <- a * exp(b * temp) * (1 - ((temp - z) / (w / 2))^2)
}



############################# BOOTSTRAPPING A ##################################
# a = a, b = b, z = tref, w = c

# step 1: fit TPC model with nlsLM() 
# w needs to be increased 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_A$data[dataframe_A$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_A,  
        start= list(z = 25, w = 25, a = 0.2, b = 0.1),
        lower = c(z = 0, w = 0, a = -0.2, b = 0),
        upper = c(z = 40, w = 95, a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>%  
  filter(z > 0 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 95) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_A <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "A")

boot_params_A <- boot_params_reduced %>% mutate(treatment = "A")



############################# BOOTSTRAPPING B ##################################
# step 1: fit TPC model with nlsLM() 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_B$data[dataframe_B$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_B,  
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = -20, w= 0, a = -0.2, b = 0),
        upper = c(z = 40, w= 120,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>% 
  filter(z > -20 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 120) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_B_2 <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "B")

boot_params_B <- boot_params_reduced %>% mutate(treatment = "B")



############################# BOOTSTRAPPING C ##################################
# tough to fit 

# step 1: fit TPC model with nlsLM() 
# z & w needed to be increased 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_C$data[dataframe_C$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_C,  
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = 0, w= 0, a = -0.2, b = 0),
        upper = c(z = 80, w= 160,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>%
  filter(z > 0 & z < 80) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 160) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_C <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "C")

boot_params_C <- boot_params_reduced %>% mutate(treatment = "C")



############################# BOOTSTRAPPING D ##################################
# step 1: fit TPC model with nlsLM() 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_D$data[dataframe_D$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_D,  
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = 0, w= 0, a = -0.2, b = 0),
        upper = c(z = 40, w= 80,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>% 
  filter(z > 0 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 120) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2) 

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_D <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "D")

boot_params_D <- boot_params_reduced %>% mutate(treatment = "D")



############################# BOOTSTRAPPING E ##################################
# step 1: fit TPC model with nlsLM() 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_E$data[dataframe_E$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_E,  
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = 0, w= 0, a = -0.2, b = 0),
        upper = c(z = 40, w= 85,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>% 
  filter(z > 0 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 85) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_E <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "E")

boot_params_E <- boot_params_reduced %>% mutate(treatment = "E")



############################# BOOTSTRAPPING F ##################################

# step 1: fit TPC model with nlsLM() 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_F$data[dataframe_F$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_F,  
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = 0, w= 0, a = -0.2, b = 0),
        upper = c(z = 40, w= 85,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>%
  filter(z > 0 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 85) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_F <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "F")

boot_params_F <- boot_params_reduced %>% mutate(treatment = "F")



############################# BOOTSTRAPPING G ##################################
# step 1: fit TPC model with nlsLM() 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_G$data[dataframe_G$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_G,  
        start= list(z= 25,w= 28, a= 0.25, b= 0.08),
        lower = c(z = 15, w= 10, a = 0, b = 0.03),
        upper = c(z = 38, w= 55, a = 1, b = 0.2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>% 
  filter(z > 0 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 80) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_G_2 <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "G")

boot_params_G <- boot_params_reduced %>% mutate(treatment = "G")



############################# BOOTSTRAPPING H ##################################

# step 1: fit TPC model with nlsLM() 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_H$data[dataframe_H$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_H,  
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = 0, w= 0, a = -0.2, b = 0),
        upper = c(z = 40, w= 80,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>% 
  filter(z > 0 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 80) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_H <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "H")

boot_params_H <- boot_params_reduced %>% mutate(treatment = "H")



############################# BOOTSTRAPPING I ##################################

# step 1: fit TPC model with nlsLM() 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_I$data[dataframe_I$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_I,  
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = 0, w= 0, a = -0.2, b = 0),
        upper = c(z = 40, w= 85,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>% 
  filter(z > 0 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 85) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_I <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "I")

boot_params_I <- boot_params_reduced %>% mutate(treatment = "I")



############################# BOOTSTRAPPING J ##################################
# step 1: fit TPC model with nlsLM() 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_J$data[dataframe_J$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_J,  
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = 0, w= 0, a = -0.2, b = 0),
        upper = c(z = 40, w= 85,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>% 
  filter(z > 0 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 120) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_J <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "J")

boot_params_J <- boot_params_reduced %>% mutate(treatment = "J")



############################# BOOTSTRAPPING K ##################################
# step 1: fit TPC model with nlsLM() 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_K$data[dataframe_K$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_K,  
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = 0, w= 0, a = -0.2, b = 0),
        upper = c(z = 40, w= 80,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)


# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>% 
  filter(z > 0 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 80) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_K <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "K")

boot_params_K <- boot_params_reduced %>% mutate(treatment = "K")



############################# BOOTSTRAPPING L ##################################

# step 1: fit TPC model with nlsLM() 
fit <- 
  nlsLM(data ~
          # estimate N0 (starting value of RFU)
          mean(c(dataframe_L$data[dataframe_L$days < 0.25])) * 
          
          # Gaussian-like TPC 
          exp((a*exp(b*temp)*(1-((temp-z)/(w/2))^2))*(days)),
        data= dataframe_L,  
        start= list(z= 25,w= 25,a= 0.2, b= 0.1),
        lower = c(z = 0, w= 0, a = -0.2, b = 0),
        upper = c(z = 40, w= 85,a =  2, b = 2),
        control = nls.control(maxiter=1024, minFactor=1/204800000))

# step 2: bootstrap with Boot()
set.seed(123)
boot <- car::Boot(fit, method = 'residual', R = 9999)

# step 4: 
## 4A: extract params 
boot_params <- boot$t %>% as.data.frame() %>% 
  filter(z > 0 & z < 40) %>% # bigger than lower limit, lower than upper limit 
  filter(w > 0 & w < 85) %>%
  filter(a > -0.2 & a < 2) %>%
  filter(b > 0 & b < 2)

boot_params_reduced <- slice_sample(boot_params, n = 999)

## 4C: generate preds for growth rate model (step 3) 
preds <- apply(boot_params_reduced, 1, function(p) {
  growth_rate_model(temp_seq, p["z"], p["w"], p["a"], p["b"])
})

## 4D: put preds into a data frame (rows = bootstraps, columns = temps)
preds <- t(preds)

## 4E: generate 95% CI 
pred_mean <- apply(preds, 2, mean)
pred_lower <- apply(preds, 2, quantile, probs = 0.025)
pred_upper <- apply(preds, 2, quantile, probs = 0.975)

# 5: plot 
## 5A: make a dataframe for plotting 
boot_preds_L <- data.frame(
  temp = temp_seq,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper) %>% 
  mutate(treatment = "L")

boot_params_L <- boot_params_reduced %>% mutate(treatment = "L")




########################### FORMATTING PREDS DF ################################
## making & saving TPC preds df 
nb_direct_filtered_preds <- 
  rbind(boot_preds_A, boot_preds_B, boot_preds_C, boot_preds_D, 
        boot_preds_E, boot_preds_F, boot_preds_G, boot_preds_H, 
        boot_preds_I, boot_preds_J, boot_preds_K, boot_preds_L) %>% 
  mutate(method = "norberg, direct, filtered") %>% 
  mutate(c = ifelse(treatment %in% c("A", "E", "H", "K"), "Enriched C",
                    ifelse(treatment %in% c("B", "C", "F", "I", "L"), "Non-enriched C", "WRONG")),
         np = ifelse(treatment %in% c("A", "B", "C"), "High N,P", 
                     ifelse(treatment %in% c("D", "E", "F"), "P-limited", 
                            ifelse(treatment %in% c("G", "H", "I"), "N-limited", 
                                   ifelse(treatment %in% c("J", "K", "L"), "Low N,P", "WRONG")))),
         cnp = ifelse(treatment == "A", "High N,P; enriched C", 
                      ifelse(treatment == "B", "High N,P; non-enriched C",
                             ifelse(treatment == "C", "High N,P; non-enriched C",
                             ifelse(treatment == "E", "P-limited; enriched C",
                                    ifelse(treatment == "F", "P-limited; non-enriched C",
                                           ifelse(treatment == "H", "N-limited; enriched C", 
                                                  ifelse(treatment == "I", "N-limited; non-enriched C",
                                                         ifelse(treatment == "J", "Low N,P; extremely enriched C",
                                                                ifelse(treatment == "K", "Low N,P; enriched C", 
                                                                       ifelse(treatment == "L", "Low N,P; non-enriched C", "OTHER"))))))))))
  ) %>% 
  mutate(np = fct_relevel(np, "High N,P", "P-limited", "N-limited", "Low N,P"))

# saving the preds df 
write_csv(nb_direct_filtered_preds, 
          here::here("processed_data/nb_direct_filtered_preds.csv"))




ggplot(gr_df %>% rename(mean = rate), aes(x = temp, y = mean)) +
  geom_point() + 
  geom_line(aes(x = temp, y = mean, color = factor(c_num)), 
            data = nb_direct_filtered_preds) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(c_num)), 
              data = nb_direct_filtered_preds, alpha = 0.4) +
  labs(title = "Direct Nb TPCs (exp data for 10-40 C, all data for <10 & 40)",
       x = "Temperature (°C)", y = "Growth Rate") +
  ylim(-.2, NA) +
  facet_wrap(~np, scales = "free") + 
  theme_classic(base_size = 14)




## making & saving TPC preds over actual gr df 
preds_over_actual_gr_direct_norberg_filtered_plot <- 
  ggplot(gr_df %>% rename(mean = rate) %>% filter(treatment %in% c("A", "B", "E", "F", "G", "H", "K", "L")), 
         aes(x = temp, y = mean)) +
  geom_point() + 
  geom_line(aes(x = temp, y = mean, color = treatment), 
            data = nb_direct_filtered_preds %>% 
              filter(treatment %in% c("A", "C", "E", "F", "G", "H", "K", "L"))) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = treatment), 
              data = nb_direct_filtered_preds %>% 
                filter(treatment %in% c("A", "C", "E", "F", "G", "H", "K", "L")), 
              alpha = 0.4) +
  labs(title = "Direct Norberg TPCs (Includes exp data for temps 10-40, all data for <10 & 40)",
       x = "Temperature (°C)", y = "Growth Rate") +
  ylim(-.2, NA) +
  facet_wrap(~treatment, scales = "free") + 
  theme_classic(base_size = 14)

ggsave(preds_over_actual_gr_direct_norberg_filtered_plot, 
       dpi = 600, height = 200, width = 200, units = "mm",
       filename = here("processed_data/preds_over_actual_gr_direct_norberg_filtered_plot.png")) 





####################### CALCULATING TPC RELATED TRAITS #########################
nb_direct_filtered_params_2026.05.01 <- 
  rbind(boot_params_A, boot_params_B, boot_params_C, boot_params_D, 
        boot_params_E, boot_params_F, boot_params_G, boot_params_H, 
        boot_params_I, boot_params_J, boot_params_K, boot_params_L) %>% 
  mutate(method = "norberg, direct (filtered)") 

write_csv(nb_direct_params_2026.05.01, 
          here::here("processed_data/nb_direct_filtered_params_2026.05.01.csv"))



# calling functions to calculate traits relevant to models  
source(here::here("functions", "norberg_indirect_trait_func.R"))
source(here::here("functions", "norberg_direct_ea_func.R"))
source(here::here("functions", "norberg_indirect_auc_func.R"))



## calculation of traits 
thermal_A <- pmap_dfr(boot_params_A %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "A", num = 1:999)
thermal_B <- pmap_dfr(boot_params_B %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "B", num = 1:999)
thermal_C <- pmap_dfr(boot_params_C %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "C", num = 1:999)
thermal_D <- pmap_dfr(boot_params_D %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "D", num = 1:999)
thermal_E <- pmap_dfr(boot_params_E %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "E", num = 1:999)
thermal_F <- pmap_dfr(boot_params_F %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "F", num = 1:999)
thermal_G <- pmap_dfr(boot_params_G %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "G", num = 1:999)
thermal_H <- pmap_dfr(boot_params_H %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "H", num = 1:999)
thermal_I <- pmap_dfr(boot_params_I %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "I", num = 1:999)
thermal_J <- pmap_dfr(boot_params_J %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "J", num = 1:999)
thermal_K <- pmap_dfr(boot_params_K %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "K", num = 1:999)
thermal_L <- pmap_dfr(boot_params_L %>% select(z, w, a, b), norberg_indirect_trait_func) %>% 
  mutate(treatment = "L", num = 1:999)


# calc ea 
ea_A <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_A)), treatment = "A", num = 1:999)
ea_B <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_B)), treatment = "B", num = 1:999)
ea_C <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_C)), treatment = "C", num = 1:999)
ea_D <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_D)), treatment = "D", num = 1:999)
ea_E <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_E)), treatment = "E", num = 1:999)
ea_F <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_F)), treatment = "F", num = 1:999)
ea_G <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_G)), treatment = "G", num = 1:999)
ea_H <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_H)), treatment = "H", num = 1:999)
ea_I <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_I)), treatment = "I", num = 1:999)
ea_J <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_J)), treatment = "J", num = 1:999)
ea_K <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_K)), treatment = "K", num = 1:999)
ea_L <- tibble(ea = unlist(norberg_direct_ea_func(boot_params_L)), treatment = "L", num = 1:999)

# calc auc
auc_A <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_A)), treatment = "A", num = 1:999)
auc_B <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_B)), treatment = "B", num = 1:999)
auc_C <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_C)), treatment = "C", num = 1:999)
auc_D <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_D)), treatment = "D", num = 1:999)
auc_E <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_E)), treatment = "E", num = 1:999)
auc_F <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_F)), treatment = "F", num = 1:999)
auc_G <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_G)), treatment = "G", num = 1:999)
auc_H <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_H)), treatment = "H", num = 1:999)
auc_I <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_I)), treatment = "I", num = 1:999)
auc_J <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_J)), treatment = "J", num = 1:999)
auc_K <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_K)), treatment = "K", num = 1:999)
auc_L <- tibble(auc = unlist(norberg_indirect_auc_func(boot_params_L)), treatment = "L", num = 1:999)

# joining traits by treatment 
traits_A <- left_join(thermal_A, ea_A, by = c("treatment", "num")) %>% left_join(., auc_A, by = c("treatment", "num"))
traits_B <- left_join(thermal_B, ea_B, by = c("treatment", "num")) %>% left_join(., auc_B, by = c("treatment", "num"))
traits_C <- left_join(thermal_C, ea_C, by = c("treatment", "num")) %>% left_join(., auc_C, by = c("treatment", "num"))
traits_D <- left_join(thermal_D, ea_D, by = c("treatment", "num")) %>% left_join(., auc_D, by = c("treatment", "num"))
traits_E <- left_join(thermal_E, ea_E, by = c("treatment", "num")) %>% left_join(., auc_E, by = c("treatment", "num"))
traits_F <- left_join(thermal_F, ea_F, by = c("treatment", "num")) %>% left_join(., auc_F, by = c("treatment", "num"))
traits_G <- left_join(thermal_G, ea_G, by = c("treatment", "num")) %>% left_join(., auc_G, by = c("treatment", "num"))
traits_H <- left_join(thermal_H, ea_H, by = c("treatment", "num")) %>% left_join(., auc_H, by = c("treatment", "num"))
traits_I <- left_join(thermal_I, ea_I, by = c("treatment", "num")) %>% left_join(., auc_I, by = c("treatment", "num"))
traits_J <- left_join(thermal_J, ea_J, by = c("treatment", "num")) %>% left_join(., auc_J, by = c("treatment", "num"))
traits_K <- left_join(thermal_K, ea_K, by = c("treatment", "num")) %>% left_join(., auc_K, by = c("treatment", "num"))
traits_L <- left_join(thermal_L, ea_L, by = c("treatment", "num")) %>% left_join(., auc_L, by = c("treatment", "num"))

traits_filtered_df <- rbind(traits_A, traits_B, traits_C, traits_D, traits_E, traits_F, 
                            traits_G, traits_H, traits_I, traits_J, traits_K, traits_L) # end of rbind() 

######################### FORMATTING TRAITS (95% CI) DF ########################
traits_filtered_ci_df <- 
  traits_filtered_df %>% 
  mutate(breadth_0rmax = tmax_0rmax - tmin_0rmax, 
         breadth_0.2rmax = tmax_0.2rmax - tmin_0.2rmax) %>% 
  group_by(treatment) %>% 
  summarise(
    topt_mean = mean(topt, na.rm = TRUE),
    topt_lower = quantile(topt, 0.025, na.rm = TRUE),
    topt_upper = quantile(topt, 0.975, na.rm = TRUE),
    
    rmax_mean = mean(rmax, na.rm = TRUE),
    rmax_lower = quantile(rmax, 0.025, na.rm = TRUE),
    rmax_upper = quantile(rmax, 0.975, na.rm = TRUE),
    
    tmin_0rmax_mean = mean(tmin_0rmax, na.rm = TRUE),
    tmin_0rmax_lower = quantile(tmin_0rmax, 0.025, na.rm = TRUE),
    tmin_0rmax_upper = quantile(tmin_0rmax, 0.975, na.rm = TRUE),
    
    tmax_0rmax_mean = mean(tmax_0rmax, na.rm = TRUE),
    tmax_0rmax_lower = quantile(tmax_0rmax, 0.025, na.rm = TRUE),
    tmax_0rmax_upper = quantile(tmax_0rmax, 0.975, na.rm = TRUE),
    
    breadth_0rmax_mean = mean(breadth_0rmax, na.rm = TRUE), 
    breadth_0rmax_lower = quantile(breadth_0rmax, 0.025, na.rm = TRUE), 
    breadth_0rmax_upper = quantile(breadth_0rmax, 0.975, na.rm = TRUE),
    
    tmin_0.2rmax_mean = mean(tmin_0.2rmax, na.rm = TRUE),
    tmin_0.2rmax_lower = quantile(tmin_0.2rmax, 0.025, na.rm = TRUE),
    tmin_0.2rmax_upper = quantile(tmin_0.2rmax, 0.975, na.rm = TRUE),
    
    tmax_0.2rmax_mean = mean(tmax_0.2rmax, na.rm = TRUE),
    tmax_0.2rmax_lower = quantile(tmax_0.2rmax, 0.025, na.rm = TRUE),
    tmax_0.2rmax_upper = quantile(tmax_0.2rmax, 0.975, na.rm = TRUE),
    
    breadth_0.2rmax_mean = mean(breadth_0.2rmax, na.rm = TRUE), 
    breadth_0.2rmax_lower = quantile(breadth_0.2rmax, 0.025, na.rm = TRUE), 
    breadth_0.2rmax_upper = quantile(breadth_0.2rmax, 0.975, na.rm = TRUE),
    
    ea_mean = mean(ea, na.rm = TRUE),
    ea_lower = quantile(ea, 0.025, na.rm = TRUE),
    ea_upper = quantile(ea, 0.975, na.rm = TRUE),
    
    auc_mean = mean(auc, na.rm = TRUE),
    auc_lower = quantile(auc, 0.025, na.rm = TRUE),
    auc_upper = quantile(auc, 0.975, na.rm = TRUE),
  ) %>% 
  mutate(tmin_0rmax_bottom = tmin_0rmax_mean - tmin_0rmax_lower,
         tmin_0rmax_top = tmin_0rmax_upper - tmin_0rmax_mean,
         tmax_0rmax_bottom = tmax_0rmax_mean - tmax_0rmax_lower,
         tmax_0rmax_top = tmax_0rmax_upper - tmax_0rmax_mean,
         breadth_0rmax_bottom = breadth_0rmax_mean - breadth_0rmax_lower,
         breadth_0rmax_top = breadth_0rmax_upper - breadth_0rmax_mean,
         tmin_0.2rmax_bottom = tmin_0.2rmax_mean - tmin_0.2rmax_lower,
         tmin_0.2rmax_top = tmin_0.2rmax_upper - tmin_0.2rmax_mean,
         tmax_0.2rmax_bottom = tmax_0.2rmax_mean - tmax_0.2rmax_lower,
         tmax_0.2rmax_top = tmax_0.2rmax_upper - tmax_0.2rmax_mean,
         breadth_0.2rmax_bottom = breadth_0.2rmax_mean - breadth_0.2rmax_lower,
         breadth_0.2rmax_top = breadth_0.2rmax_upper - breadth_0.2rmax_mean,
         rmax_bottom = rmax_mean - rmax_lower,
         rmax_top = rmax_upper - rmax_mean,
         topt_bottom = topt_mean - topt_lower,
         topt_top = topt_upper - topt_mean) %>% 
  mutate(c = ifelse(treatment %in% c("A", "E", "G", "K"), "Enriched C",
                    ifelse(treatment %in% c("B", "C", "F", "H", "L"), "Non-enriched C", "WRONG")),
         np = ifelse(treatment %in% c("A", "B", "C"), "High N,P", 
                     ifelse(treatment %in% c("D", "E", "F"), "P-limited", 
                            ifelse(treatment %in% c("G", "H", "I"), "N-limited", 
                                   ifelse(treatment %in% c("J", "K", "L"), "Low N,P", "WRONG")))),
         cnp = ifelse(treatment == "A", "High N,P; enriched C", 
                      ifelse(treatment == "B", "High N,P; non-enriched C",
                             ifelse(treatment == "C", "High N,P; non-enriched C",
                                    ifelse(treatment == "E", "P-limited; enriched C",
                                           ifelse(treatment == "F", "P-limited; non-enriched C",
                                                  ifelse(treatment == "G", "N-limited; enriched C", 
                                                         ifelse(treatment == "H", "N-limited; non-enriched C",
                                                                ifelse(treatment == "I", "N-limited; non-enriched C",
                                                                ifelse(treatment == "J", "Low N,P; extremely enriched C",
                                                                       ifelse(treatment == "K", "Low N,P; enriched C", 
                                                                              ifelse(treatment == "L", "Low N,P; non-enriched C", "OTHER")))))))))))
  ) %>% 
  mutate(np = fct_relevel(np, "High N,P", "P-limited", "N-limited", "Low N,P"))

write_csv(traits_filtered_df, here::here("processed_data", "traits_filtered_df.csv"))  
write_csv(traits_filtered_ci_df, here::here("processed_data", "traits_filtered_ci_df.csv"))  






# this was used to check what temperature ranges where treatment x is greater
# significantly than treatment y 
x <- filter(nb_direct_filtered_preds) %>% filter(treatment == "C")
y <- filter(nb_direct_filtered_preds) %>% filter(treatment == "A")  

left_join(x, y, by = "temp") %>% filter(lower.x > upper.y)





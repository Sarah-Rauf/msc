################################################################################
#                 Cell volume and lipid processing & analysis                  # 
################################################################################
# Created by: Sarah Rauf 


# Description: processing and analysis of cell volume and lipid data 


############################## Relevant packages ###############################
## data processing 
library(tidyverse) # everywhere
library(broom) # for tidy()
library(readxl) # pull out times and RFU/OD data from excel files
library(here) # to pull "plate_layout.csv" from "processed_data" folder
library(ggpubr) # for ggarrange()

## data analysis 
library(dplyr)


################################ Cell Volume ###################################
######################## Processing df for figures #############################
## Processing for figures 
cellvol_layout <- read_csv(here("processed_data", "jan282026_cnp_volume_platelayout.csv"))

cellsize_data <- 
  read_csv(here("processed_data", "jan282026_cnp_cellsize_data.csv")) %>% 
  rename(cellvol = "single_cell_vol_um^3") %>% 
  left_join(., cellvol_layout, by = c("temp", "date", "well"))

# turn off scientific notation ! 
options(scipen=999)

cellsize_summarized_df <- 
  cellsize_data %>% 
  group_by(date, temp, treatment) %>%
  summarise(mean_cellvol = mean(cellvol),
            se_cellvol = sd(cellvol)/sqrt(sum(!is.na(cellvol)))) %>% 
  mutate(np = ifelse(treatment %in% c("A", "B", "C"), "High N,P", 
                     ifelse(treatment %in% c("D", "E", "F"), "P-limited", 
                            ifelse(treatment %in% c("G", "H", "I"), "N-limited", 
                                   ifelse(treatment %in% c("J", "K", "L"), "Low N,P", "WRONG")))),
         
         c = ifelse(treatment %in% c("A", "E", "G", "J"), "Enriched C", 
                    ifelse(treatment %in% c("B", "C", "F", "H", "I", "L"), "Non-enriched C", "WRONG")), 
         cnp = ifelse(treatment == "A", "High N,P; enriched C", 
                      ifelse(treatment == "B", "High N,P; non-enriched C",
                             ifelse(treatment == "C", "High N,P; non-enriched C",
                                    ifelse(treatment == "E", "P-limited; enriched C",
                                           ifelse(treatment == "F", "P-limited; non-enriched C",
                                                  ifelse(treatment == "G", "N-limited; enriched C", 
                                                         ifelse(treatment == "H", "N-limited; enriched C",
                                                                ifelse(treatment == "I", "N-limited; non-enriched C", 
                                                                       ifelse(treatment == "J", "Low N,P; extremely enriched C",
                                                                              ifelse(treatment == "K", "Low N,P; enriched C", 
                                                                                     ifelse(treatment == "L", "Low N,P; non-enriched C", "OTHER")))))))))))
  )  # end of mutate()

write_csv(cellsize_summarized_df, 
          here("processed_data", "cellsize_summarized_df.csv"))



#################### Processing df for model selection #########################
# pulling the TPC df into separate df 
cellvol_02.06_df <- cellvol_df %>% filter(date == "2026-02-06") %>% 
  select(-c(shape, plate_well.y)) %>% rename(plate_well = plate_well.x)

# summarizing cell vol for random forest/decision tree analysis 
cellvol_02.06_summarized <- 
  cellvol_02.06_df %>% group_by(treatment, temp, og_id) %>% 
  summarise(mean_vol = mean(cellvol))

# converting treatment & bio rep to "factors" for model fitting 
cellvol_02.06_df$treatment <- factor(cellvol_02.06_df$treatment)
cellvol_02.06_df$og_id <- factor(cellvol_02.06_df$og_id)




##################### Random forest / decision tree analysis ###################
# Decision tree 
library(rpart)

dt_fit <- rpart(cellvol ~ (temp + I(temp^2)) + treatment, 
                method = "anova",
                data = cellvol_02.06_summarized)

plot(dt_fit, uniform = TRUE)
text(dt_fit, use.n = TRUE, cex = .9)

print(dt_fit)

best_cp <- dt_fit$cptable[which.min(dt_fit$cptable[, "xerror"]), "CP"]

pruned_tree <- prune(dt_fit, cp = 0.01002563)

plot(pruned_tree)
text(pruned_tree, use.n = TRUE, cex = .9)

as.data.frame(pruned_tree$variable.importance) %>% 
  mutate(delta_importance = (pruned_tree$variable.importance)/5876415)

# Random forest 
library(randomForest)

rf_fit <- randomForest(cellvol ~ temp + treatment, data = cellvol_02.06_summarized)

as.data.frame(importance(rf_fit)) %>% 
  mutate(delta_importance = (IncNodePurity/2378362))

varImpPlot(rf_fit)



############################## Model selection #################################
# linear relationship, lipid_value~temp 
# assumes all treatments follows the same shape 
m0 <- gam(cellvol ~ s(temp, k = 5), data = cellvol_02.06_df, method = "ML")

# same temperature-response shape for all treatments
# treatments shift the curve vertically 
# shape fixed, intercept changes 
m1 <- gam(cellvol ~ s(temp, k = 5) + treatment, 
          data = cellvol_02.06_df, method = "ML")

# fit a smooth curve (lipid_value~temp) for each treatment level (AKA bs = "fs")
m2 <- gam(cellvol ~ s(temp, by = treatment, bs = "fs", k = 3), 
          data = cellvol_02.06_df, method = "ML")

# shared overall thermal response, but there are treatment-specific departures 
m3 <- gam(cellvol ~ s(temp, k = 5) + s(temp, by = treatment, bs = "fs", k = 3), 
          data = cellvol_02.06_df, method = "ML")

# m2 + treatment_well as a random effect (AKA bs = "re") 
m4 <- gam(cellvol ~ s(temp, by = treatment, bs = "fs", k = 3) + s(og_id, bs = "re"),
          data = cellvol_02.06_df, method = "ML")

# m3 + treatment_well as a random effect (AKA bs = "re") 
m5 <- gam(cellvol ~ s(temp, k = 5) + s(temp, by = treatment, bs = "fs", k = 3) + 
            s(og_id, bs = "re"), 
          data = cellvol_02.06_df, method = "ML")

# shared overall thermal response, but there are treatment-specific departures 
# treatment shifts the curve vertically 
m6 <- gam(cellvol ~ treatment + s(temp, k = 5) + s(temp, by = treatment, k = 3), 
          data = cellvol_02.06_df, method = "ML")

# m6 + treatment_well as a random effect (AKA bs = "re")
m7 <- gam(cellvol ~ s(temp, k = 5) + s(temp, by = treatment, k = 3) + treatment + 
            s(og_id, bs = "re"), family = gaussian(), 
          data = cellvol_02.06_df, method = "ML")

options(scipen = 999) # turn off scientific notation 

cellvol_aic_df <- 
  data.frame(aic = c(AIC(m0), AIC(m1), AIC(m2), AIC(m3), AIC(m4), AIC(m5), AIC(m6), AIC(m7)),
             model.names = c('m0', 'm1', 'm2', 'm3', 'm4', 'm5', 'm6', 'm7')) %>% 
  mutate(delta = aic - min(aic),
         weights = round((exp(-0.5 * delta) / sum(exp(-0.5 * delta))), 4
         ))

# this is the best model 
cellvol_model <-  gam(cellvol ~ s(temp, k = 5) + s(temp, by = treatment, k = 3) + 
                        treatment + s(og_id, bs = "re"), family = gaussian(), 
                      data = cellvol_02.06_df, method = "ML")


############################ Paired contrast analysis ##########################
library(emmeans)
emmeans(cellvol_model, pairwise ~ treatment)



# Lipid

################################ load data #####################################
RFU_files <- c(list.files("2026.01.28_cnp_lipid_data", full.names = TRUE))

names(RFU_files) <- 
  RFU_files %>% 
  # get rid of (2) things in file name 
  gsub(pattern = "2026.01.28_cnp_lipid_data/", replacement = "") %>% 
  gsub(pattern = ".xlsx", replacement = "")

# (1) extract RFU & OD values from plate reads 
all_lipid_reads <- 
  map_df(RFU_files, read_excel, range = "B32:O40", .id = "file_name") %>% # (1A) extract cells & add .id 
  rename(row = `...1`) %>% # (1B) rename ...1 as row
  rename(readtype = `...14`) %>% # (1C) rename ...14 as readtype 
  pivot_longer(., cols = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), 
               names_to = "well_number", values_to = "data") %>% # pivot_longer so all RFUs are under one column 
  unite(well, row, well_number, sep = "") %>% 
  separate(file_name, into = c("date", "temp", "lipid_data"), sep = "_", remove = FALSE) %>% 
  separate(temp, into = c("tem", "temp"), sep ="p") %>% 
  mutate(exp_date = ifelse(date == "feb32026", "2026-02-03", 
                           ifelse(date == "jan312026", "2026-01-31", 
                                  ifelse(date == "feb62026", "2026-02-06", "WRONG")))) %>%
  select(-c(tem, lipid_data, date, readtype)) %>% # remove columns 
  
  # for left_join() with plate layout 
  unite(exp_date_temp_well, exp_date, temp, well, remove = FALSE)

########################### loading plate layout ###############################
lipid_plate_layout <- 
  read_csv(here("processed_data", "jan282026_cnp_lipid_platelayout.csv")) %>% 
  
  # for left_join() blanks & sample data 
  unite(exp_date_temp_treatment, exp_date, temp, treatment, remove = FALSE) %>% 
  select(-c(well, temp, exp_date))

############### merge plate layout & plate reads using left_join ###############
lipid_data <- 
  left_join(all_lipid_reads, lipid_plate_layout, by="exp_date_temp_well") %>% 
  filter(!str_detect(treatment, "empty"))


lipid_data_samples <- 
  lipid_data %>% 
  filter(type == "lipid")

lipid_data_blanks <- 
  lipid_data %>% 
  filter(type == "blank") %>% 
  group_by(exp_date_temp_treatment) %>% 
  summarise(blank_data = mean(data))


# "corrected": blanks subtracted from sample data 
lipid_data_corrected <- 
  left_join(lipid_data_samples, lipid_data_blanks, by = "exp_date_temp_treatment") %>%
  # subtract blank data from sample data 
  mutate(lipid_value = data - blank_data) %>% 
  select(-type) %>% # removing columns for cleanliness 
  mutate(temp = as.numeric(temp))

# pulling the TPC df into separate df 
lipid_02.06_df <- 
  lipid_data_corrected %>% filter(exp_date == "2026-02-06") %>% 
  select(temp, treatment, lipid_value, treatment_well) %>% filter(!lipid_value > 9000)

# converting treatment & bio rep to "factors" for model fitting 
lipid_02.06_df$treatment <- factor(lipid_02.06_df$treatment)
lipid_02.06_df$treatment_well <- factor(lipid_02.06_df$treatment_well)



# table 
lipid_data_summarized <- 
  lipid_data_corrected %>% 
  rename(date = exp_date) %>% 
  select(-c(exp_date_temp_treatment, exp_date_temp_well, treatment_well)) %>% 
  group_by(date, temp, treatment) %>%
  # remove extreme outlier (lipid value read 9717)
  filter(!lipid_value == 9717.000000) %>% 
  summarise(mean_lipid = mean(lipid_value),
            se_lipid = sd(lipid_value)/sqrt(sum(!is.na(lipid_value)))) %>% 
  mutate(np = ifelse(treatment %in% c("A", "B", "C"), "High N,P", 
                     ifelse(treatment %in% c("D", "E", "F"), "P-limited", 
                            ifelse(treatment %in% c("G", "H", "I"), "N-limited", 
                                   ifelse(treatment %in% c("J", "K", "L"), "Low N,P", "WRONG")))),
         
         c = ifelse(treatment %in% c("A", "E", "G", "J"), "Enriched C", 
                    ifelse(treatment %in% c("B","C", "F", "H", "I", "L"), "Non-enriched C", "WRONG")), 
         cnp = ifelse(treatment == "A", "High N,P; enriched C", 
                      ifelse(treatment == "B", "High N,P; non-enriched C",
                             ifelse(treatment == "C", "High N,P; non-enriched C", 
                                    ifelse(treatment == "E", "P-limited; enriched C",
                                           ifelse(treatment == "F", "P-limited; non-enriched C",
                                                  ifelse(treatment == "G", "N-limited; enriched C", 
                                                         ifelse(treatment == "H", "N-limited; enriched C",
                                                                ifelse(treatment == "I", "N-limited; non-enriched C",
                                                                       ifelse(treatment == "J", "Low N,P; extremely enriched C",
                                                                              ifelse(treatment == "K", "Low N,P; enriched C", 
                                                                                     ifelse(treatment == "L", "Low N,P; non-enriched C", "OTHER")))))))))))
  ) # end of mutate()

write_csv(lipid_data_summarized, 
          here("processed_data", "lipid_data_summarized.csv"))




##################### Random forest / decision tree analysis ###################
# Decision tree 
library(rpart)

dt_fit <- rpart(lipid_value ~ (temp + I(temp^2)) + treatment, 
                method = "anova",
                data = lipid_02.06_df)

plot(dt_fit, uniform = TRUE)
text(dt_fit, use.n = TRUE, cex = .9, xpd = TRUE)

print(dt_fit)

printcp(dt_fit)
plotcp(dt_fit)

best_cp <- dt_fit$cptable[
  which.min(dt_fit$cptable[, "xerror"]), "CP"]

pruned_tree <- prune(dt_fit, cp = 0.03814079)

plot(pruned_tree)
text(pruned_tree, use.n = TRUE, cex = .9)

as.data.frame(pruned_tree$variable.importance) %>% 
  mutate(delta_importance = (pruned_tree$variable.importance)/5876415)

# Random forest 
library(randomForest)

rf_fit <- randomForest(lipid_value ~ temp + treatment, data = lipid_02.06_df)

as.data.frame(importance(rf_fit)) %>% 
  mutate(delta_importance = (IncNodePurity/5805739))

varImpPlot(rf_fit)


############################## Model selection #################################
# linear relationship, value~temp 
# assumes all treatments follows the same shape 
m0 <- gam(lipid_value ~ s(temp, k = 5), data = lipid_02.06_df, method = "ML")

# same temperature-response shape for all treatments
# treatments shift the curve vertically 
# shape fixed, intercept changes 
m1 <- gam(lipid_value ~ s(temp, k = 5) + treatment, 
          data = lipid_02.06_df, method = "ML")

# fit a smooth curve (lipid_value~temp) for each treatment level (AKA bs = "fs")
m2 <- gam(lipid_value ~ s(temp, by = treatment, bs = "fs", k = 3), 
          data = lipid_02.06_df, method = "ML")

# shared overall thermal response, but there are treatment-specific departures 
m3 <- gam(lipid_value ~ s(temp, k = 5) + s(temp, by = treatment, bs = "fs", k = 3), 
          data = lipid_02.06_df, method = "ML")

# m2 + treatment_well as a random effect (AKA bs = "re") 
m4 <- gam(lipid_value ~ s(temp, by = treatment, bs = "fs", k = 3) + s(treatment_well, bs = "re"),
          data = lipid_02.06_df, method = "ML")

# m3 + treatment_well as a random effect (AKA bs = "re") 
m5 <- gam(lipid_value ~ s(temp, k = 5) + s(temp, by = treatment, bs = "fs", k = 3) + 
            s(treatment_well, bs = "re"), 
          data = lipid_02.06_df, method = "ML")

# shared overall thermal response, but there are treatment-specific departures 
# treatment shifts the curve vertically 
m6 <- gam(lipid_value ~ treatment + s(temp, k = 5) + s(temp, by = treatment, k = 3), 
          data = lipid_02.06_df, method = "ML")

# m6 + treatment_well as a random effect (AKA bs = "re")
m7 <- gam(lipid_value ~ s(temp, k = 5) + s(temp, by = treatment, k = 3) + treatment + 
            s(treatment_well, bs = "re"), family = gaussian(), 
          data = lipid_02.06_df, method = "ML")

options(scipen = 999) # turn off scientific notation 

lipid_aic_df <- 
  data.frame(aic = c(AIC(m0), AIC(m1), AIC(m2), AIC(m3), AIC(m4), AIC(m5), AIC(m6), AIC(m7)),
             model.names = c('m0', 'm1', 'm2', 'm3', 'm4', 'm5', 'm6', 'm7')) %>% 
  mutate(delta = aic - min(aic),
         weights = round((exp(-0.5 * delta) / sum(exp(-0.5 * delta))), 4
         ))


# this is the second best model, but it has treatment as an effect so 
# I can use in the paired contrast (another confirmation that lipid content 
# is not affected by nutrient level)
lipid_model <-  gam(lipid_value ~ s(temp, k = 5) + treatment, data = lipid_02.06_df, method = "ML")



############################ Paired contrast analysis ##########################
library(emmeans)

emmeans(
  lipid_model,
  pairwise ~ treatment
)
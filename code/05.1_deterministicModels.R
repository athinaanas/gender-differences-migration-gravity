
library(tidyverse)

# run 04_errorMetrics.R script first

############################
### DETERMINISTIC MODELS ###
############################

data_all <- read_csv("./data/dataclean_gravity_final.csv")

### apply typology for each year and flow ###
typedata <- data_all %>% 
  group_by(orig, dest, startyear) %>% 
  mutate(totalflow = round(flow[sex == "male"] + flow[sex == "female"], 0),
         femshare = if_else(totalflow == 0, NA, round(flow[sex == "female"],0)/totalflow),
         type = case_when(
           femshare > 0.53 ~ "FPD",
           femshare < 0.47 ~ "MPD",
           femshare >= 0.47 & femshare <= 0.53 ~ "GB",
           totalflow == 0 ~ "ZERO")) %>% 
  ungroup()

### make subsets ###
data_train <- typedata %>% 
  filter(!startyear == 2015,# remove flows where orig and dest are same
         !orig == dest)
data_test <- typedata %>% filter(startyear == 2015, !orig == dest)

### historic mean flow & persistence ###
data_train <- data_train %>% 
  arrange(startyear) %>%
  group_by(orig, dest, sex) %>% 
  mutate(pred_hist = mean(flow),
         pred_pers = last(flow)) %>% 
  ungroup()

subset <- data_train %>% dplyr::select(orig, dest, sex, pred_hist, pred_pers) %>% distinct()

data_deter <- data_test %>%left_join(subset)

### error metrics historical mean ###
data <- data_deter
sex <- c("female", "male")
type <- c("GB", "FPD", "MPD")
results <- data.frame(sex = character(), type = character(), MAPE = numeric(), MAE = numeric(), MASE = numeric())

# loop over all combinations
for (s in sex) {
  for (t in type) {
    subset_data <- data[data$sex == s & data$type == t, ]
    
    if (nrow(subset_data) > 0) {
      N <- nrow(subset_data)
      trueval <- subset_data$flow
      pred <- subset_data$pred_hist
      pers <- subset_data$pred_pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, type = t, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
error_hist <- as.data.frame(results)

### error metrics persistence ###
results <- data.frame(sex = character(), type = character(), MAPE = numeric(), MAE = numeric(), MASE = numeric())

# loop over all combinations
for (s in sex) {
  for (t in type) {
    subset_data <- data[data$sex == s & data$type == t, ]
    
    if (nrow(subset_data) > 0) {
      N <- nrow(subset_data)
      trueval <- subset_data$flow
      pred <- subset_data$pred_pers
      pers <- subset_data$pred_pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, type = t, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
error_pers <- as.data.frame(results)

### for total female vs. male flows ###
data <- data_deter
sex <- c("female", "male")
results <- data.frame(sex = character(), MAPE = numeric(), MAE = numeric(), MASE = numeric())

# loop over all combinations
for (s in sex) {
  for (t in type) {
    subset_data <- data[data$sex == s, ]
    
    if (nrow(subset_data) > 0) {
      N <- nrow(subset_data)
      trueval <- subset_data$flow
      pred <- subset_data$pred_hist
      pers <- subset_data$pred_pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
error_histall <- as.data.frame(results) %>% distinct()

### error metrics persistence ###
results <- data.frame(sex = character(), MAPE = numeric(), MAE = numeric(), MASE = numeric())

# loop over all combinations
for (s in sex) {
  for (t in type) {
    subset_data <- data[data$sex == s, ]
    
    if (nrow(subset_data) > 0) {
      N <- nrow(subset_data)
      trueval <- subset_data$flow
      pred <- subset_data$pred_pers
      pers <- subset_data$pred_pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
error_persall <- as.data.frame(results) %>% distinct()

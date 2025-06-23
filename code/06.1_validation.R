

library(tidyverse)

# run 04_errorMetrics.R script first

#########################
### ROBUSTNESS CHECKS ###
#########################

### robustness checks ###
## other gravity type models based on Beyer et al. 2022 and Cohen et al. 2008

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
  filter(!startyear == 2015, # remove zero flows for OLS model
         !totalflow == 0,
         !orig == dest) %>% 
  mutate(t = startyear - 2005,
         t2 = (startyear - 2005)^2)

data_test <- typedata %>% 
  filter(startyear == 2015, 
         !totalflow == 0,
         !orig == dest) %>% 
  mutate(t = startyear - 2005,
         t2 = (startyear - 2005)^2)

### Beyer models ###
model1 <- lm(log(flow + 1) ~ log(POPtot.o + 1) + log(POPtot.d + 1) + log(distance + 1) + log(GDP.o +1) + log(GDP.d +1),
             data = data_train)
summary(model1)  

model2 <- lm(log(flow +1) ~ log(POPtot.o + 1) + log(POPtot.d + 1) + log(distance + 1) + log(GDP.o +1) + log(GDP.d +1)
             + UEMtot.o + UEMtot.d + schooling.o + schooling.d + LEXtot.o + LEXtot.d + POL.o + POL.d + MOBtot.o + MOBtot.d + COL + OL,
             data = data_train)
summary(model2)

### Cohen model ###
model3 <- lm(log(flow+1) ~ t + log(LA.o +1) + log(LA.d +1) + log(distance +1), data = data_train)
summary(model3)

### make predictions ###
data_test$pred_model1 <- exp(predict(model1, data_test, interval = "confidence"))
data_test$pred_model2 <- exp(predict(model2, data_test, interval = "confidence"))
data_test$pred_model3 <- exp(predict(model3, data_test, interval = "confidence"))

### error metrics model 1 ###
pers_values <- data_train %>% filter(startyear==2010) %>% rename(pers = flow) %>% dplyr::select(pers, orig, dest, sex)
data <- data_test %>% left_join(pers_values)
sex <- c("female", "male")
type <- c("GB", "FPD", "MPD")
results <- data.frame(sex = character(), type = character(), MAPE = numeric(), MAE = numeric(), MASE = numeric())

# loop over all combinations for each model (adjust pred input and results object)
for (s in sex) {
  for (t in type) {
    subset_data <- data[data$sex == s & data$type == t, ]
    
    if (nrow(subset_data) > 0) {
      N <- nrow(subset_data)
      trueval <- subset_data$flow
      pred <- subset_data$pred_model1[,"fit"]
      pers <- subset_data$pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, type = t, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
error_model1 <- as.data.frame(results) 

### error metrics model 2 ###
pers_values <- data_train %>% filter(startyear==2010) %>% rename(pers = flow) %>% dplyr::select(pers, orig, dest, sex)
data <- data_test %>% left_join(pers_values)
sex <- c("female", "male")
type <- c("GB", "FPD", "MPD")
results <- data.frame(sex = character(), type = character(), MAPE = numeric(), MAE = numeric(), MASE = numeric())

# loop over all combinations for each model (adjust pred input and results object)
for (s in sex) {
  for (t in type) {
    subset_data <- data[data$sex == s & data$type == t, ]
    
    if (nrow(subset_data) > 0) {
      N <- nrow(subset_data)
      trueval <- subset_data$flow
      pred <- subset_data$pred_model2[,"fit"] ## change based on model
      pers <- subset_data$pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, type = t, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
error_model2 <- as.data.frame(results) 


### error metrics model 3 ###
pers_values <- data_train %>% filter(startyear==2010) %>% rename(pers = flow) %>% dplyr::select(pers, orig, dest, sex)
data <- data_test %>% left_join(pers_values)
sex <- c("female", "male")
type <- c("GB", "FPD", "MPD")
results <- data.frame(sex = character(), type = character(), MAPE = numeric(), MAE = numeric(), MASE = numeric())

# loop over all combinations for each model (adjust pred input and results object)
for (s in sex) {
  for (t in type) {
    subset_data <- data[data$sex == s & data$type == t, ]
    
    if (nrow(subset_data) > 0) {
      N <- nrow(subset_data)
      trueval <- subset_data$flow
      pred <- subset_data$pred_model3[,"fit"]
      pers <- subset_data$pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, type = t, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
error_model3 <- as.data.frame(results) 


## combined to plots in F.PlotTables.R




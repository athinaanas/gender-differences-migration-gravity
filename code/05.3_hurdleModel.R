
library(pscl)
library(tidyverse)

# run 04_errorMetrics.R script first

####################
### HURDLE MODEL ###
####################

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
  filter(!startyear == 2015,
         !orig == dest) %>% 
  mutate(t = startyear - 2005,
         t2 = (startyear - 2005)^2,
         flow = round(flow, 0)) %>% # round to integers, so that Poisson distribution applies
  dplyr::select(orig, dest, sex, startyear, flow, POPtot.o, POPtot.d, distance, IMR.o, IMR.d,
                PSR.o, PSR.d, URB.o, URB.d, LA.o, LA.d, LB, OL, COL, t, t2, type)

data_test <- typedata %>% filter(startyear == 2015, !orig == dest) %>% 
  mutate(t = startyear - 2005,
         t2 = (startyear - 2005)^2,
         flow = round(flow,0)) %>% 
  dplyr::select(orig, dest, sex, startyear, flow, POPtot.o, POPtot.d, distance, IMR.o, IMR.d,
                PSR.o, PSR.d, URB.o, URB.d, LA.o, LA.d, LB, OL, COL, t, t2, type)

### model hurdle ###
hurdle_model <- hurdle(flow ~ scale(POPtot.o) + scale(POPtot.d) + scale(distance) + scale(PSR.o) + scale(PSR.d) + scale(IMR.o) + scale(IMR.d) +
    scale(URB.o) + scale(URB.d) + scale(LA.o) + scale(LA.d) + LB + OL + COL + t + t2,
  data = data_train, 
  dist = "poisson",
  zero.dist = "binomial")
summary(hurdle_model)

### make predictions ###
data_test$pred_hurdle <- predict(hurdle_model, data_test, type = "response")

### error metrics hurdle ###
pers_values <- data_train %>% filter(startyear==2010) %>% rename(pers = flow) %>% dplyr::select(pers, orig, dest, sex)
data <- data_test %>% left_join(pers_values)
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
      pred <- subset_data$pred_hurdle
      pers <- subset_data$pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, type = t, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
error_hurdle <- as.data.frame(results)


### for total female vs. male flows ###
pers_values <- data_train %>% filter(startyear==2010) %>% rename(pers = flow) %>% dplyr::select(pers, orig, dest, sex)
data <- data_test %>% left_join(pers_values)
sex <- c("female", "male")
results <- data.frame(sex = character(), MAPE = numeric(), MAE = numeric(), MASE = numeric())

# loop over all combinations
for (s in sex) {
  for (t in type) {
    subset_data <- data[data$sex == s, ]
    
    if (nrow(subset_data) > 0) {
      N <- nrow(subset_data)
      trueval <- subset_data$flow
      pred <- subset_data$pred_hurdle
      pers <- subset_data$pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
error_hurdleall <- as.data.frame(results) %>% distinct()



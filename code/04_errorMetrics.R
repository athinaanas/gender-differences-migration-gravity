rm(list = ls())
cat("\014")

#####################
### ERROR METRICS ###
#####################

### MAPE ###
MAPE <- function(pred, trueval, N){
  mape <- (100/N)*sum(abs(trueval-pred)/(trueval+1), na.rm = TRUE)
  return(mape)
}

### MASE ###
MASE <- function(pred, trueval, N, pers){
  mase <- sum(abs(trueval-pred), na.rm = TRUE)/sum(abs(trueval-pers), na.rm = TRUE)
  return(mase)
}

### MAE ###
MAE <- function(pred, trueval, N){
  mae <- (1/N)*sum(abs(trueval-pred), na.rm = TRUE)
  return(mae)
}





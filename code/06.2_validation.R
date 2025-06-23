

##################
### VALIDATION ###
##################

### data collection ###

library(tidyverse)
library(glue)
library(countrycode)
library(raster)
library(WDI)

# run 04_errorMetrics.R script first

## Quant Mig ##
# download csv for each country from 
# LINK: https://www.quantmig.eu/data_and_estimates/estimates_explorer/
download.file("https://www.quantmig.eu/res/saved_sql/83abcd25e6762939ea3ae98e85431120.csv", destfile = "./temp/AUT.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/241b758d094119ff851c50b641c5d175.csv", destfile = "./temp/BEL.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/51d1315bf3dfc7c5d9c9ae14ebc59b45.csv", destfile = "./temp/BGR.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/03cdf6506231d46d3a372b4309578af9.csv", destfile = "./temp/HRV.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/602785fb8eb398cc042df1cd35fc0d9e.csv", destfile = "./temp/CYP.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/0233c598557629c0f83b8b905a3d7a7c.csv", destfile = "./temp/CZE.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/7ae9b4d34d11b08855d40cdce4323cc6.csv", destfile = "./temp/DNK.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/193fce72f1958dd2d702e86bd871eeaf.csv", destfile = "./temp/EST.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/8de5912be4804807d1cc3f77e67fe2f7.csv", destfile = "./temp/FIN.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/b21b7637d489727cb111ad004f5f9c41.csv", destfile = "./temp/FRA.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/eee1d73ce2c36ffb81a0ba423babda07.csv", destfile = "./temp/DEU.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/f6c5c10ac06d6a49b82fb77714ed6bd3.csv", destfile = "./temp/GRC.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/dbef1fda5ba676eb811dcb611477fef0.csv", destfile = "./temp/HUN.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/b83d490216234fa692c9021ea7354a01.csv", destfile = "./temp/ISL.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/1a739455e68982fafba10519658dc1b6.csv", destfile = "./temp/IRL.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/a8a420ec86ff0c008f5f46224ba65794.csv", destfile = "./temp/ITA.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/df4015cdacd109c5d9e4da3603533ee9.csv", destfile = "./temp/LVA.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/a830e395a11238a9e724a810f0ef5354.csv", destfile = "./temp/LIE.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/1a836765381edbe36a9aac2a2a302b73.csv", destfile = "./temp/LTU.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/86b61504f0941aad0e531b54bc5e9082.csv", destfile = "./temp/LUX.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/265840e0982dda391f4a6f81ef76fcbd.csv", destfile = "./temp/MLT.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/76d649323c1ba02ea112688681ee810a.csv", destfile = "./temp/NLD.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/80a00d21c8b7e0cb3c37dd733ca04cf5.csv", destfile = "./temp/MKD.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/624e175b6db9212924cbcb20466f36a5.csv", destfile = "./temp/NOR.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/cded651a0231103d4695529f604ea786.csv", destfile = "./temp/POL.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/05b095805d84fe2586d3849505a94859.csv", destfile = "./temp/PRT.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/3400c927fa7b3ebd38b916b4a0d1cfad.csv", destfile = "./temp/ROU.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/3a372fc787cedda1886fa84e0ec16c04.csv", destfile = "./temp/SVK.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/fb6f7e8d38cc774603c1205f802eba54.csv", destfile = "./temp/SVN.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/877c55e0fb5dc6d132c0c9da6368f1fc.csv", destfile = "./temp/ESP.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/d979097ee63f375bb12c08c0bfe745a0.csv", destfile = "./temp/SWE.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/bdd1e79d14754bcea0cd81c77fc64ab8.csv", destfile = "./temp/CHE.csv", mode = "wb")
download.file("https://www.quantmig.eu/res/saved_sql/b02e56648d1b6633b650bf764eec3db2.csv", destfile = "./temp/GBR.csv", mode = "wb")

# read in the files
ISO3 <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL",
          "IRL", "ITA", "LVA", "LIE", "LTU", "LUX", "MLT", "NLD", "MKD", "NOR", "POL", "PRT", "ROU", "SVK",
          "SVN", "ESP", "SWE", "CHE", "GBR")

for (i in 1:length(ISO3)) {
  name <- ISO3[i]
  obj <- read_csv(glue("./temp/{name}.csv"))
  assign(name, obj)
}

# add name column ISO3

for (i in 1:length(ISO3)) {
  name <- ISO3[i]
  obj <- get(glue({ISO3[i]}))%>% 
    mutate(dest = glue("{name}")) %>% 
    rename(orig = country)
  assign(name, obj)
}

data_qm <- rbind(AUT, BEL, BGR, HRV, CYP, CZE, DNK, EST, FIN, FRA, DEU, 
                 GRC, HUN, ISL, IRL, ITA, LVA, LIE, LTU, LUX, MLT, NLD,
                 MKD, NOR, POL, PRT, ROU, SVK, SVN, ESP, SWE, CHE, GBR)

# change the full names of origin to ISO3

data_qm <- data_qm %>% 
  filter(!orig == "Rest World") %>% 
  mutate(orig = countrycode(sourcevar = orig, origin = "country.name", destination = "iso3c"),
         sex = case_when(
           sex == "F" ~ "female",
           sex == "M" ~ "male")) %>% 
  rename(startyear = year,
         flow = median) # renaming this so i dont have to change the code for analysis


# collect covariates for the countries (as we have one-year intervals, we do this from scratch)

## distances between capitals ##
## distances between capitals ##
data(countryref)

coords <- countryref %>% 
  dplyr::select(iso3, capital, capital.lon, capital.lat) %>% 
  distinct() %>% 
  na.omit()

dist_mat <- matrix(nrow = length(coords$iso3), ncol = length(coords$iso3))
colnames(dist_mat) <- c(coords$iso3)
rownames(dist_mat) <- c(coords$iso3)

for (i in 1:nrow(dist_mat)){
  dist_mat[,i] <- pointDistance(coords[i, c("capital.lon", "capital.lat")],
                                coords[, c("capital.lon", "capital.lat")],
                                lonlat = T)} # point distance in meters

dist_mat <- as.data.frame(dist_mat)
distance_list <- list()

for (i in 1:ncol(dist_mat)) {
  orig <- rownames(dist_mat)
  dest <- rep(colnames(dist_mat)[i], times = length(orig))
  dist <- as.numeric(dist_mat[, i])  # ensure it's numeric here
  
  dat <- data.frame(orig = orig, dest = dest, distance = dist, stringsAsFactors = FALSE)
  distance_list[[i]] <- dat
}

# Combine all into one data frame
distance <- do.call(rbind, distance_list)

## other covariates ##
# WDI indicators at the start of each period
# population size: SP.POP.TOTL
# urban population in %: SP.URB.TOTL.IN.ZS
# infant mortality per 1,000 births: SP.DYN.IMRT.IN
# potential support ratio (100 * No. of people aged 15 to 64 / person aged 65+): 100*(SP.POP.1564.TO / SP.POP.65UP.TO) 
# land area (in sq. km): AG.LND.TOTL.K2

WDIdata <- WDI(indicator=c("SP.POP.TOTL", "SP.URB.TOTL.IN.ZS", "SP.DYN.IMRT.IN", "SP.POP.1564.TO", "SP.POP.65UP.TO", "AG.LND.TOTL.K2"), 
               country="all", start = 1984, end = 2019, extra = T)
WDIdata <- WDIdata %>% 
  dplyr::select(-lending, -capital, -region, -iso2c, -lastupdated, -status, -longitude, -latitude) %>% 
  arrange(iso3c, desc(year)) %>% 
  filter(!iso3c == "")
WDIdata.o <- WDIdata %>% 
  rename(orig = iso3c,
         INCOME.o = income,
         startyear = year,
         URB.o = SP.URB.TOTL.IN.ZS,
         IMR.o = SP.DYN.IMRT.IN,
         LA.o = AG.LND.TOTL.K2,
         POP.o = SP.POP.TOTL) %>% 
  mutate(PSR.o = 100*(SP.POP.1564.TO/SP.POP.65UP.TO))

WDIdata.d <- WDIdata %>% 
  rename(dest = iso3c,
         INCOME.d = income,
         startyear = year,
         URB.d = SP.URB.TOTL.IN.ZS,
         IMR.d = SP.DYN.IMRT.IN,
         LA.d = AG.LND.TOTL.K2,
         POP.d = SP.POP.TOTL) %>% 
  mutate(PSR.d = 100*(SP.POP.1564.TO/SP.POP.65UP.TO))

## other covariates downloaded from CEPII
# Link: http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8
# country code: iso3_o, iso3_d
# contiguity (shared borders): contig (1 = contiguity)
# past colonial relationship between countries: col_dep_ever
# common official language between countries: comlang_off
CEPII_all <- readRDS("./temp/Gravity_V202211/Gravity_V202211.rds")
CEPII <- CEPII_all %>% 
  filter(year %in% seq(1984, 2019, 1)) %>% 
  rename(orig = iso3_o,
         dest = iso3_d,
         startyear = year,
         LB = contig,
         OL = comlang_off,
         COL = col_dep_ever) %>% 
  dplyr::select(orig, dest, startyear, LB, OL, COL) %>% 
  na.omit() %>% 
  distinct()

dataQM <- data_qm %>% left_join(WDIdata.o, by = c("orig", "startyear")) %>% 
  left_join(WDIdata.d, by = c("dest", "startyear")) %>% 
  left_join(CEPII) %>%  left_join(distance)

write.csv(dataQM, "./data/dataQM.csv", row.names = F)

## Asia Pacific data ##
# LINK to article: https://onlinelibrary.wiley.com/doi/10.1002/psp.2716
# data in supporting information (cannot be downloaded automatically)
ap_data <- read_csv("./temp/Final estimates - Online supplementary material 2.csv") 
ap_data <- ap_data %>% 
  rename(orig = Origin,
         dest = Destination,
         startyear = Year, # 2000 - 2019
         sex = Sex) %>% 
  group_by(orig, dest, startyear, sex) %>% 
  mutate(flow = sum(median),
         sex = if_else(sex == "Female", "female", "male")) %>% # sum all the age groups
  ungroup() %>% 
  filter(!orig %in% c("ALL", "ROA", "AFR", "EUR", "SCA"),
         !dest %in% c("ALL", "ROA", "AFR", "EUR", "SCA")) %>% 
  dplyr::select(-Age, -median, -lower95, -lower80, -upper80, -upper95) %>% 
  distinct()

# join
dataAP <- ap_data %>% left_join(WDIdata.o, by = c("orig", "startyear")) %>% 
  left_join(WDIdata.d, by = c("dest", "startyear")) %>% 
  left_join(CEPII) %>%  left_join(distance)

write.csv(dataAP, "./data/dataAP.csv", row.names = F)

## Latin America Data ##
# LINK to article: https://www.demogr.mpg.de/publications/files/7102_1634283430_1_Main%20manuscript.pdf
# LINK to data: https://celade.cepal.org/bdcelade/imila/ 
# collected manually
imila_total <- read.csv("./temp/data_recmig_imila_web_all.csv", sep = ";") %>% 
  filter(agegrp == "total",
         !c_birth == "ALL") %>% 
  mutate(sex = "total")
imila_fem <- read_csv("./temp/data_recmig_imila_web_females.csv") %>% 
  filter(agegrp == "total",
         !c_birth == "ALL") %>% 
  mutate(sex = "female")
imila_male <- read_csv("./temp/data_recmig_imila_web_males.csv") %>% 
  filter(agegrp == "total",
         !c_birth == "ALL") %>% 
  mutate(sex = "male")

imila <- rbind(imila_total, imila_fem, imila_male) %>% 
  dplyr::select(c_dest, c_birth, year, total, sex) %>% 
  rename(startyear = year,
         dest = c_dest,
         orig = c_birth,
         flow = total) %>% 
  filter(!sex == "total")

dataLA <- imila %>% left_join(WDIdata.o, by = c("orig", "startyear")) %>% 
  left_join(WDIdata.d, by = c("dest", "startyear")) %>% 
  left_join(CEPII) %>%  left_join(distance)

write.csv(dataLA, "./data/dataLA.csv", row.names = F)


### OLS gravity model ###

dataQM <- read_csv("./data/dataQM.csv")
dataAP <- read_csv("./data/dataAP.csv")
dataLA <- read_csv("./data/dataLA.csv")

# split data sets #
typedata <- dataQM %>% 
  group_by(orig, dest, startyear) %>% 
  mutate(totalflow = round(flow[sex == "male"] + flow[sex == "female"], 0),
         femshare = if_else(totalflow == 0, NA, round(flow[sex == "female"],0)/totalflow),
         type = case_when(
           femshare > 0.53 ~ "FPD",
           femshare < 0.47 ~ "MPD",
           femshare >= 0.47 & femshare <= 0.53 ~ "GB",
           totalflow == 0 ~ "ZERO")) %>% 
  ungroup()

data_trainQM <- typedata %>% 
  filter(!startyear == 2019,
         !totalflow == 0,
         !orig == dest) %>% 
  arrange(orig, dest, startyear) %>% 
  mutate(t = startyear - 2014,
         t2 = (startyear - 2014)^2)

data_testQM <- typedata %>% 
  filter(startyear == 2019,
         !totalflow == 0,
         !orig == dest) %>% 
  arrange(orig, dest, startyear) %>% 
  mutate(t = startyear - 2014,
         t2 = (startyear - 2014)^2)

typedata <- dataAP %>% 
  group_by(orig, dest, startyear) %>%
  mutate(totalflow = round(flow[sex == "male"] + flow[sex == "female"], 0),
         femshare = if_else(totalflow == 0, NA, round(flow[sex == "female"],0)/totalflow),
         type = case_when(
           femshare > 0.53 ~ "FPD",
           femshare < 0.47 ~ "MPD",
           femshare >= 0.47 & femshare <= 0.53 ~ "GB",
           totalflow == 0 ~ "ZERO")) %>% 
  ungroup()

data_trainAP <- typedata %>% 
  filter(!totalflow == 0,
         !startyear == 2019,
         !orig == dest) %>% 
  mutate(t = startyear - 2009,
         t2 = (startyear - 2009)^2)

data_testAP <- typedata %>% 
  filter(!totalflow == 0,
         startyear == 2019,
         !orig == dest) %>% 
  mutate(t = startyear - 2009,
         t2 = (startyear - 2009)^2)

typedata <- dataLA %>% 
  group_by(orig, dest, startyear) %>% 
  mutate(totalflow = round(flow[sex == "male"] + flow[sex == "female"], 0),
         femshare = if_else(totalflow == 0, NA, round(flow[sex == "female"],0)/totalflow),
         type = case_when(
           femshare > 0.53 ~ "FPD",
           femshare < 0.47 ~ "MPD",
           femshare >= 0.47 & femshare <= 0.53 ~ "GB",
           totalflow == 0 ~ "ZERO")) %>% 
  ungroup()

data_trainLA <- typedata %>%
  filter(totalflow != 0) %>%
  group_by(orig, dest, sex) %>%
  filter(startyear != max(startyear, na.rm = TRUE)) %>%
  ungroup()

data_testLA <- typedata %>% 
  filter(totalflow != 0) %>%
  group_by(orig, dest, sex) %>%
  filter(startyear == max(startyear, na.rm = TRUE)) %>%
  ungroup()

# models #

modelQM <- lm(log(flow + 1) ~ log(POP.o + 1) + log(POP.d + 1) + log(distance + 1) + log(PSR.o + 1) + log(PSR.d + 1) + log(IMR.o + 1) + log(IMR.d + 1) +
                URB.o + URB.d + log(LA.o + 1) + log(LA.d + 1) + LB + OL + COL + t + t2, data= data_trainQM)
data_testQM$pred <- exp(predict(modelQM, data_testQM, interval = "confidence"))

modelAP <- lm(log(flow + 1) ~ log(POP.o + 1) + log(POP.d + 1) + log(distance + 1) + log(PSR.o + 1) + log(PSR.d + 1) + log(IMR.o + 1) + log(IMR.d + 1) +
                URB.o + URB.d + log(LA.o + 1) + log(LA.d + 1) + LB + OL + COL + t + t2, data= data_trainAP)
data_testAP$pred <- exp(predict(modelAP, data_testAP, interval = "confidence"))

modelLA <- lm(log(flow + 1) ~ log(POP.o + 1) + log(POP.d + 1) + log(distance + 1) + log(PSR.o + 1) + log(PSR.d + 1) + log(IMR.o + 1) + log(IMR.d + 1) +
                URB.o + URB.d + log(LA.o + 1) + log(LA.d + 1) + LB + OL + COL, data= data_trainLA)
data_testLA$pred <- exp(predict(modelLA, data_testLA, interval = "confidence"))

# error metrics #
# QM
pers_values <- data_trainQM %>% filter(startyear==2018) %>% rename(pers = flow) %>% dplyr::select(pers, orig, dest, sex)
data <- data_testQM %>% left_join(pers_values) 
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
      pred <- subset_data$pred[,"fit"]
      pers <- subset_data$pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, type = t, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
errorQM <- as.data.frame(results)

# AP
pers_values <- data_trainAP %>% filter(startyear==2018) %>% rename(pers = flow) %>% dplyr::select(pers, orig, dest, sex)
data <- data_testAP %>% left_join(pers_values) 
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
      pred <- subset_data$pred[,"fit"]
      pers <- subset_data$pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, type = t, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
errorAP <- as.data.frame(results)

# LA
pers_values <- data_trainLA %>% filter(startyear == max(startyear, na.rm = TRUE)) %>% rename(pers = flow) %>% dplyr::select(pers, orig, dest, sex)
data <- data_testLA %>% left_join(pers_values) 
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
      pred <- subset_data$pred[,"fit"]
      pers <- subset_data$pers
      
      mape_val <- MAPE(pred, trueval, N)
      mae_val <- MAE(pred, trueval, N)
      mase_val <- MASE(pred, trueval, N, pers)
      
      results <- rbind(results, data.frame(sex = s, type = t, MAPE = mape_val, MAE = mae_val, MASE = mase_val))
    }
  }
}
errorLA <- as.data.frame(results)

# empty temp
unlink("./temp", recursive = TRUE)









library(tidyverse)
library(CoordinateCleaner)
library(countrycode)
library(raster)
library(WDI)
library(readxl)

##########################
### COLLECT COVARIATES ###
##########################

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
                                lonlat = T) # point distance in meters
}

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
# WDI indicators at the start of each period (1990, 1995, 2000, 2005, 2010)
# GDP per capita in USD 2015: NY.GDP.PCAP.KD
# population size: SP.POP.TOTL
# population female: SP.POP.TOTL.FE.IN
# population male: SP.POP.TOTL.MA.IN
# urban population in %: SP.URB.TOTL.IN.ZS
# infant mortality per 1,000 births: SP.DYN.IMRT.IN
# potential support ratio (100 * No. of people aged 15 to 64 / person aged 65+): 100*(SP.POP.1564.TO / SP.POP.65UP.TO) 
# land area (in sq. km): AG.LND.TOTL.K2
# Life expectancy at birth, total (years): SP.DYN.LE00.IN
# Life expectancy at birth, female (years): SP.DYN.LE00.FE.IN
# Life expectancy at birth, male (years): SP.DYN.LE00.MA.IN
# Unemployment rate, total in %: SL.UEM.TOTL.NE.ZS
# Unemployment rate, female in %: SL.UEM.TOTL.FE.NE.ZS
# Unemployment rate, male in %: SL.UEM.TOTL.MA.NE.ZS
# Educational attainment, at least Bachelor's or equivalent, population 25+, female (%) (cumulative): SE.TER.CUAT.BA.FE.ZS
# Educational attainment, at least Bachelor's or equivalent, population 25+, male (%) (cumulative): SE.TER.CUAT.BA.MA.ZS
# Political stability and absence of violence/terrorism estimate (-2.5 to 2.5): PV.EST

WDIdata <- WDI(indicator=c("NY.GDP.PCAP.KD", "SP.URB.TOTL.IN.ZS", "SP.DYN.IMRT.IN", "AG.LND.TOTL.K2", "SP.DYN.LE00.IN", "SP.DYN.LE00.FE.IN", "SP.DYN.LE00.MA.IN", "SL.UEM.TOTL.NE.ZS",
                           "SL.UEM.TOTL.FE.NE.ZS", "SL.UEM.TOTL.MA.NE.ZS", "SE.TER.CUAT.BA.ZS", "SE.TER.CUAT.BA.FE.ZS", "SE.TER.CUAT.BA.MA.ZS", "PV.EST"), 
               country="all", start = 1990, end = 2015, extra = T)
WDIdata <- WDIdata %>% 
  dplyr::select(-lending, -capital, -region, -iso2c, -lastupdated, -status, -longitude, -latitude) %>% 
  arrange(iso3c, desc(year)) %>% 
  filter(!iso3c == "")
WDIdata.o <- WDIdata %>% 
  filter(year %in% seq(1990, 2015, 5)) %>% 
  rename(orig = iso3c,
         INCOME.o = income,
         startyear = year,
         GDP.o = NY.GDP.PCAP.KD,
         URB.o = SP.URB.TOTL.IN.ZS,
         IMR.o = SP.DYN.IMRT.IN,
         LA.o = AG.LND.TOTL.K2,
         LEXtot.o = SP.DYN.LE00.IN,
         LEXfem.o = SP.DYN.LE00.FE.IN,
         LEXmal.o = SP.DYN.LE00.MA.IN,
         UEMtot.o = SL.UEM.TOTL.NE.ZS,
         UEMfem.o = SL.UEM.TOTL.FE.NE.ZS,
         UEMmal.o = SL.UEM.TOTL.MA.NE.ZS,
         EDUtot.o = SE.TER.CUAT.BA.ZS,
         EDUfem.o = SE.TER.CUAT.BA.FE.ZS,
         EDUmal.o = SE.TER.CUAT.BA.MA.ZS,
         POL.o = PV.EST)

WDIdata.d <- WDIdata %>% 
  filter(year %in% seq(1990, 2015, 5)) %>% 
  rename(dest = iso3c,
         INCOME.d = income,
         startyear = year,
         GDP.d = NY.GDP.PCAP.KD,
         URB.d = SP.URB.TOTL.IN.ZS,
         IMR.d = SP.DYN.IMRT.IN,
         LA.d = AG.LND.TOTL.K2,
         LEXtot.d = SP.DYN.LE00.IN,
         LEXfem.d = SP.DYN.LE00.FE.IN,
         LEXmal.d = SP.DYN.LE00.MA.IN,
         UEMtot.d = SL.UEM.TOTL.NE.ZS,
         UEMfem.d = SL.UEM.TOTL.FE.NE.ZS,
         UEMmal.d = SL.UEM.TOTL.MA.NE.ZS,
         EDUtot.d = SE.TER.CUAT.BA.ZS,
         EDUfem.d = SE.TER.CUAT.BA.FE.ZS,
         EDUmal.d = SE.TER.CUAT.BA.MA.ZS,
         POL.d = PV.EST)

## Population data from WPP ##
# LINK: https://population.un.org/wpp/ 
# population, total
# population, female
# population, male
# young population ratio (population aged 20-34 / population total)
# potential support ratio (100 * No. of people aged 15 to 64 / person aged 65+)

# make temp folder
dir.create("./temp")
download.file("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/2_Population/WPP2024_POP_F02_2_POPULATION_5-YEAR_AGE_GROUPS_MALE.xlsx", destfile = "./temp/pop_male.xlsx", mode = "wb")
download.file("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/2_Population/WPP2024_POP_F02_3_POPULATION_5-YEAR_AGE_GROUPS_FEMALE.xlsx", destfile = "./temp/pop_female.xlsx", mode = "wb")

POP_male <- read_excel("./temp/pop_male.xlsx")
POP_female <- read_excel("./temp/pop_female.xlsx")

names <- unique(WDIdata$country)
pop_male <- POP_male %>%
  filter(grepl(paste(names, collapse = "*|"), .[[3]]),
         .[[11]] %in% seq(1990, 2015, 5)) %>%
  dplyr::select(3, 11:32) %>%
  rename_with(.cols = 3:23, 
              .fn = ~ as.character(POP_male[12, 12:32])) %>%
  rename(iso3c = 1, startyear = 2) %>% 
  mutate(across(-1, as.numeric)) %>% 
  pivot_longer(cols = 3:23, names_to = "age_name", values_to = "pop_mal") %>% 
  mutate(pop_mal = pop_mal*1000, # covert thousands
         iso3c = countrycode(sourcevar = iso3c, origin = "country.name", destination = "iso3c")) %>% 
  na.omit()

pop_female <- POP_female %>%
  filter(grepl(paste(names, collapse = "*|"), .[[3]]),
         .[[11]] %in% seq(1990, 2015, 5)) %>%
  dplyr::select(3, 11:32) %>%
  rename_with(.cols = 3:23, 
              .fn = ~ as.character(POP_female[12, 12:32])) %>%
  rename(iso3c = 1, startyear = 2) %>% 
  mutate(across(-1, as.numeric)) %>% 
  pivot_longer(cols = 3:23, names_to = "age_name", values_to = "pop_fem") %>% 
  mutate(pop_fem = pop_fem*1000, # covert thousands
         iso3c = countrycode(sourcevar = iso3c, origin = "country.name", destination = "iso3c")) %>%
  na.omit()

# proportion of population 20-35 (mobile population)
pop_male.o <- pop_male %>% 
  group_by(iso3c, startyear) %>% 
  summarize(MOBmal.o = sum(pop_mal[age_name %in% c("20-24", "25-29", "30-34")])/sum(pop_mal),
            POPmal.o = sum(pop_mal)) %>% 
  ungroup() %>% 
  rename(orig = iso3c)
pop_male.d <- pop_male %>% 
  group_by(iso3c, startyear) %>% 
  summarize(MOBmal.d = sum(pop_mal[age_name %in% c("20-24", "25-29", "30-34")])/sum(pop_mal),
            POPmal.d = sum(pop_mal)) %>% 
  ungroup() %>% 
  rename(dest = iso3c)
pop_female.o <- pop_female %>% 
  group_by(iso3c, startyear) %>% 
  summarize(MOBfem.o = sum(pop_fem[age_name %in% c("20-24", "25-29", "30-34")])/sum(pop_fem),
            POPfem.o = sum(pop_fem)) %>% 
  ungroup() %>% 
  rename(orig = iso3c)
pop_female.d <- pop_female %>% 
  group_by(iso3c, startyear) %>% 
  summarize(MOBfem.d = sum(pop_fem[age_name %in% c("20-24", "25-29", "30-34")])/sum(pop_fem),
            POPfem.d = sum(pop_fem)) %>% 
  ungroup() %>% 
  rename(dest = iso3c)
pop_total <- pop_female %>% left_join(pop_male) %>% 
  mutate(pop_total = pop_fem + pop_mal) %>% 
  group_by(iso3c, startyear) %>% 
  summarize(POPtot = sum(pop_total),
            PSR = sum(pop_total[age_name %in% c("15-19", "20-24", "25-29", "30-34", "35-39",
                                            "40-44", "45-49", "50-54", "55-59", "60-64")])/
              sum(pop_total[age_name %in% c("65-69", "70-74", "75-79", "80-84", "85-89",
                                        "90-94", "95-99", "100+")])) %>% ungroup()
pop_total.o <- pop_total %>% 
  rename(orig = iso3c,
         POPtot.o = POPtot,
         PSR.o = PSR)
pop_total.d <- pop_total %>% 
  rename(dest = iso3c,
         POPtot.d = POPtot,
         PSR.d = PSR)

## UNDP data ##
# LINK: https://hdr.undp.org/data-center/documentation-and-downloads 
# expected years of schooling from HDI report for 2022
# gender index for 2022
# downloaded manually
genderindex <- read_excel("./temp/HDR23-24_Statistical_Annex_GII_Table.xlsx")
genderindex <- genderindex %>% 
  dplyr::select(2:3, 5) %>% 
  rename(country = 1,
         genderindex = 2,
         genderrank = 3) %>% 
  filter(!is.na(country),
         !grepl("develop*", country, ignore.case = TRUE),
         !is.na(genderindex)) %>% 
  mutate(genderindex = gsub("\\.\\.", NA, genderindex)) %>% 
  mutate(ISO3 = countrycode(sourcevar = country, origin = "country.name", destination = "iso3c"),
         genderindex = as.numeric(genderindex),
         genderrank = as.numeric(genderrank)) %>% 
  filter(!is.na(ISO3))
genderindex.o <- genderindex %>% rename(genderindex.o = genderindex, genderrank.o = genderrank, orig = ISO3) %>% dplyr::select(-country)
genderindex.d <- genderindex %>% rename(genderindex.d = genderindex, genderrank.d = genderrank, dest = ISO3) %>% dplyr::select(-country)

schooling <- read_excel("./temp/HDR23-24_Statistical_Annex_HDI_Table.xlsx")
schooling <- schooling %>% 
  dplyr::select(2,7) %>% 
  rename(country = 1, 
         schooling = 2) %>% 
  filter(!is.na(country),
         country %in% names) %>% 
  mutate(iso3c = countrycode(sourcevar = country, origin = "country.name", destination = "iso3c"),
         schooling = as.numeric(schooling))
schooling.o <- schooling %>% rename(schooling.o = schooling, orig = iso3c) %>% dplyr::select(-country)
schooling.d <- schooling %>% rename(schooling.d = schooling, dest = iso3c) %>% dplyr::select(-country)


## other covariates downloaded from CEPII manually
# Link: http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8
# country code: iso3_o, iso3_d
# contiguity (shared borders): contig (1 = contiguity)
# past colonial relationship between countries: col_dep_ever
# common official language between countries: comlang_off
# Landlocked country: can't find it in the gravity database

CEPII_all <- readRDS("./temp/Gravity_V202211/Gravity_V202211.rds")
CEPII <- CEPII_all %>% 
  filter(year %in% seq(1990, 2015, 5)) %>% 
  rename(orig = iso3_o,
         dest = iso3_d,
         startyear = year,
         LB = contig,
         OL = comlang_off,
         COL = col_dep_ever) %>% 
  dplyr::select(orig, dest, startyear, LB, OL, COL) %>% 
  na.omit() %>% 
  distinct()

#######################
### MIGRATION FLOWS ###
#######################
# bilateral migration flow estimates by sex for all countries (Abel & Cohen, 2022)
# LINK to article: https://www.nature.com/articles/s41597-022-01271-z
# LINK to data: https://figshare.com/collections/Bilateral_international_migration_flow_estimates_by_sex/5800838
flows <- read_csv("https://figshare.com/ndownloader/files/38016672", show_col_types = FALSE)
flows <- flows %>% 
  rename(startyear = year0,
         flow = da_pb_closed) %>% 
  arrange(orig, startyear) %>% 
  dplyr::select(-sd_drop_neg, -sd_rev_neg, -mig_rate, -da_min_open, -da_min_closed)

data_all <- flows %>% left_join(WDIdata.o, by = c("orig", "startyear")) %>% left_join(WDIdata.d, by = c("dest", "startyear")) %>% left_join(pop_total.o) %>% left_join(pop_total.d) %>% 
  left_join(pop_female.o) %>% left_join(pop_female.d) %>% left_join(pop_male.o) %>% left_join(pop_male.d) %>% 
  left_join(genderindex.o) %>% left_join(genderindex.d) %>% left_join(schooling.o) %>% left_join(schooling.d) %>% 
  left_join(CEPII) %>% left_join(distance) %>% 
  mutate(MOBtot.o = MOBfem.o + MOBmal.o,
         MOBtot.d = MOBfem.d + MOBmal.d)

## save data as csv
write.csv(data_all, "./data/dataclean_gravity_final.csv", row.names = F)

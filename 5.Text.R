###########################################################################
### "text.R"
## This script shows the analyses performed to create any statistics etc. referenced in the text of Trend_Analysis.tex
# 1. Load all packages required for the analyses
# 2. Load all functions etc. found in other scripts
# 3. Introduction
# 4. Methods
# 5. Results
# 6. Discussion
# 7. Conclussion
#############################################################################

#############################################################################
## 1. Load all packages required for the analyses
library(zoo)
library(lubridate)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(xtable)
library(doMC); doMC::registerDoMC(cores = 4)

#############################################################################
## 2. Load all functions etc. found in other scripts
source("func/metaTemp.R") # Afunction that calculates meta-deta/ stats of time series
source("func/seqSites.R") # A function that orders sites correctly along the coast of South Africa

#############################################################################
### 3. Introduction

# No stats are given in the intro

#############################################################################
### 4. Methods

## Data Sources

# Count of type of instrument used for time series
site_list <- read.csv("setupParams/site_list_v4.0.csv")
site_list$index <- paste(site_list$site, site_list$src, sep = "/ ")
length(site_list$type[site_list$type == "thermo"]) # 86
length(site_list$type[site_list$type == "old"]) # 13
length(site_list$type[site_list$type == "new"]) # 30

# Oldest time series
load("data/SACTNmonthly_v4.0.Rdata")
wide <- dcast(SACTNmonthly_v4.0, date ~ site+src, value.var = "temp", mean)
wide_zoo <- zoo(wide[,2:length(colnames(wide))], as.Date(as.character(as.POSIXct(wide$date))))
data_summary <- adply(wide_zoo, 2, metaTemp)
min(data_summary$date.start) # "1972-01-01"

# Beginning decade of time series
lngths <- data_summary$date.start[order(data_summary$date.start)]
lngths

## Data Management

# Number of time series in cleaned data
load("data/SACTN_sub2.Rdata")
length(levels(SACTN_sub2$index)) # 84

# Number of days in cleaned data
length(SACTN_sub2$temp)*365.25/12 # ~819,499

# Number of months in SACTN
length(SACTN_sub2$temp) # 26,924

## Systematic Analysis of Time Series

# Shortest and longest time series
load("data/SACTN_full_natural_no_interp.Rdata")
min(SACTN_full_natural_no_interp$length) # 72
max(SACTN_full_natural_no_interp$length) # 519

# Number of unique time series
load("data/SACTN_full_grown_no_interp.Rdata")
SACTN_full_grown_no_interp$index2 <- as.factor(paste(SACTN_full_grown_no_interp$index, 
                                                     SACTN_full_grown_no_interp$DT, 
                                                     SACTN_full_grown_no_interp$prec, sep = "/ "))
length(levels(SACTN_full_grown_no_interp$index2)) # 1680

#############################################################################
### 5. Results

## table01

## Intro

## Magnitude of decadal trend determined by GLS
# Load model results
load("data/gls_fitted_full_nointerp_natural.RData")
# Add "type" column
# Load the data that has percent NA and type columns to add them to the GLS data frame
load("data/SACTN_sub2.Rdata")
colnames(SACTN_sub2)[8] <- "na_perc"
SACTN_sub2 <- as.data.frame(SACTN_sub2)
# Add the type column
gls_df$index <- as.factor(paste(gls_df$site, gls_df$src, sep = "/ "))
gls_df$type <- NA
gls_df$type[gls_df$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "thermo"]))] <- "thermo"
gls_df$type[gls_df$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "old"]))] <- "old"
gls_df$type[gls_df$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "new"]))] <- "new"
# Add "DT_real" column as numeric values
gls_df$DT_real <- NA
gls_df$DT_real[gls_df$DT == "DT000"] <- 0.00
gls_df$DT_real[gls_df$DT == "DT005"] <- 0.05
gls_df$DT_real[gls_df$DT == "DT010"] <- 0.10
gls_df$DT_real[gls_df$DT == "DT015"] <- 0.15
gls_df$DT_real[gls_df$DT == "DT020"] <- 0.20
# Calculate "DT_perc", the percentage different between the real DT and the modelled DT
gls_df <- gls_df %>% 
  mutate(DT_perc = ((DT_real-DT_model)/DT_real)*100)
# Add NA%
gls_df <- gls_df%>%
  group_by(index) %>%
  mutate(na_perc = SACTN_sub2$na_perc[SACTN_sub2$index == index][1])
# Remove rows with missing data
gls_df_no_0 <- gls_df %>% 
  filter(DT_real != 0.00)

# Correlation between abs(DT_perc) vs. SD_initial
DT_vs_SD <- gls_df_no_0 %>%
  filter(prec == "prec0001") %>% 
  group_by(DT) %>% 
  mutate(r2 = cor(abs(DT_perc), sd_initial)) %>% 
  mutate(p = as.numeric(cor.test(abs(DT_perc), sd_initial)[3]))

DT_vs_SD <- DT_vs_SD[,c(3,20:21)] %>% 
  unique() # r = -0.05

## Significance p-value of the model fit

# Number of significant trends
length(gls_df$p_trend) # 1680 modelled fits
length(gls_df$p_trend[gls_df$p_trend <= 0.05])
gls_df[gls_df$p_trend <= 0.05,]

## Error associated with trend estimate

# Correlation between abs(DT_perc) vs. se_trend
DT_vs_SE <- gls_df_no_0 %>%
  filter(prec == "prec0001") %>% 
  group_by(DT) %>% 
  mutate(r2 = cor(abs(DT_perc), se_trend)) %>% 
  mutate(p = as.numeric(cor.test(abs(DT_perc), se_trend)[3]))

DT_vs_SE <- DT_vs_SE[,c(3,20:21)] %>% 
  unique() # r = 0.57, p < 0.001

## Effect of % missing values on trends estimation

# Count of time series missing X%
length(levels(droplevels(gls_df$index[gls_df$na_perc < 1]))) #25
length(levels(droplevels(gls_df$index[gls_df$na_perc < 5]))) #70-25=45
length(levels(droplevels(gls_df$index[gls_df$na_perc < 10]))) #80-70=10
length(levels(droplevels(gls_df$index[gls_df$na_perc < 15]))) #84
mean(gls_df$na_perc) # 2.65

# Correlation between p_trend vs. %NA
ptrend_vs_NA <- gls_df_no_0 %>%
  filter(prec == "prec0001") %>% 
  group_by(DT) %>% 
  mutate(r2 = cor(p_trend, na_perc)) %>% 
  mutate(p = as.numeric(p_trend, na_perc)[3])

ptrend_vs_NA <- ptrend_vs_NA[,c(3,20:21)] %>% 
  unique() # r = 0.57, p < 0.001

# Group 

# Correlation between abs(DT_perc) vs. se_trend
ptrend_vs_NA <- gls_df_no_0 %>%
  filter(prec == "prec0001") %>% 
  group_by(na_group)
  group_by(DT) %>% 
  mutate(r2 = cor(p_trend, na_perc)) %>% 
  mutate(p = as.numeric(p_trend, na_perc)[3])

ptrend_vs_NA <- ptrend_vs_NA[,c(3,20:21)] %>% 
  unique() # r = 0.57, p < 0.001

## Effect of initial SD on trend estimate

## Effect of time series length on trend estimation

## Effect of decadal trend steepness on trend estimation

## Effect of measurement precision on trend estimation

## Effect instrument type on trend detection

#############################################################################
### 6. Discussion

#############################################################################
### 7. Conclussion

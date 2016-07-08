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
# library(tseries)
# library(zoo)
# library(vegan)
library(ggplot2)
# library(xtable)
# library(reshape2)
# library(FNN)
# library(gridExtra)
# library(grid)
# library(lubridate)
# library(pwr)
# library(nlme)
# library(mgcv)
# library(dplyr)
# library(tidyr)
# library(purrr)
# library(broom)
library(doMC); doMC::registerDoMC(cores = 8)

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
siteList <- read.csv("setupParams/site_list_v4.0.csv")
siteList$index <- paste(siteList$site, siteList$src, sep = "/ ")
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
load("data/SACTN_flat.Rdata")
length(levels(SACTN_full$index))

# Number of days in cleaned data
length(SACTN_flat$temp)*365.25/12 # ~797,736

# Number of months in SACTN
length(SACTN_flat$temp) # 26,209

## Systematic Analysis of Time Series

# Shortest and longest time series
load("data/SACTN_full.Rdata")
min(SACTN_full$year_index) # 5
max(SACTN_full$year_index) # 43

# Number of unique time series
SACTN_full$index2 <- as.factor(paste(SACTN_full$index, SACTN_full$DT, SACTN_full$prec, sep = "/ "))
length(levels(SACTN_full$index2)) # 1680

#############################################################################
### 5. Results

## Systematic Analysis of Time Series

# Table of statistics - Must be something better shown with a table than a figure

# Range of p_trend detected in models
p_ranges <- gls_df %>%
  summarise(min(p_trend)) %>%
  summarise(min(p_trend)) %>%
  summarise(min(p_trend))


# Calculate correlation between different precisions

#############################################################################
### 6. Discussion

#############################################################################
### 7. Conclussion

###########################################################################
### "tables.R"
## This script shows the code used to generate the tables for the paper
# 1. Load all packages required for the analyses
# 2. Load all functions etc. found in other scripts
# 3. Create table01
#############################################################################

#############################################################################
## 1. Load all packages required for the analyses
# library(tseries)
# library(zoo)
# library(vegan)
library(ggplot2)
library(xtable)
# library(reshape2)
# library(FNN)
# library(gridExtra)
# library(grid)
# library(lubridate)
# library(pwr)
# library(nlme)
# library(mgcv)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(doMC); doMC::registerDoMC(cores = 8)

#############################################################################
## 2. Load all functions etc. found in other scripts
source("func/metaTemp.R") # Afunction that calculates meta-deta/ stats of time series
source("func/seqSites.R") # A function that orders sites correctly along the coast of South Africa

#############################################################################
### 3. Create table01

# Rows
  # type min-mean-max
# Columns
  # Length
  # p
  # DT/ DT_model

## Load the modelled data and add the type column
load("data/gls_df.RData")
gls_df$index <- as.factor(paste(gls_df$site, gls_df$src, sep = "/ "))
gls_df$type <- NA
gls_df$type[gls_df$index %in% levels(droplevels(site_list$index[site_list$type == "thermo"]))] <- "thermo"
gls_df$type[gls_df$index %in% levels(droplevels(site_list$index[site_list$type == "old"]))] <- "old"
gls_df$type[gls_df$index %in% levels(droplevels(site_list$index[site_list$type == "new"]))] <- "new"



results_table <- gls_df %>%
  group_by(type)
###########################################################################
### "tables.R"
## This script shows the code used to generate the tables for the paper
# 1. Load all packages required for the analyses
# 2. Load all functions etc. found in other scripts
# 3. Create table01
#############################################################################

#############################################################################
## 1. Load all packages required for the analyses
# library(zoo)
library(ggplot2)
library(xtable)
library(reshape2)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(doMC); doMC::registerDoMC(cores = 4)

#############################################################################
## 2. Load all functions etc. found in other scripts
source("func/metaTemp.R") # Afunction that calculates meta-deta/ stats of time series
source("func/seqSites.R") # A function that orders sites correctly along the coast of South Africa

#############################################################################
### 3. Create table01

# Rows
  # type? 
  # sd-mean-sd
# Columns
  # Length
  # p_trend
  # rmse(DTdata-DT_model)
  # se_trend

## Load the modelled data
load("data/gls_fitted_full_nointerp_natural.RData")
# head(gls_df)

## Add "DT_real" column as numeric values
gls_df$DT_real <- NA
gls_df$DT_real[gls_df$DT == "DT000"] <- 0.00
gls_df$DT_real[gls_df$DT == "DT005"] <- 0.05
gls_df$DT_real[gls_df$DT == "DT010"] <- 0.10
gls_df$DT_real[gls_df$DT == "DT015"] <- 0.15
gls_df$DT_real[gls_df$DT == "DT020"] <- 0.20

# Add same small value to "DT_model" for DT 0.00
# gls_df0 <- gls_df %>%
#   filter(DT == "DT000") %>% 
#   mutate(DT_model = DT_model + 0.001)
# 
# gls_df1 <- gls_df %>%
#   filter(DT != "DT000")
# 
# gls_df <- rbind(gls_df0, gls_df1)

## Calculate "DT_perc", the percentage different between the real DT and the modelled DT
gls_df <- gls_df %>% 
  mutate(DT_perc = ((DT_real-DT_model)/DT_real)*100)

# Ply the data to get the required statistics
  # Only use prec0001
results_table <- gls_df[,c(3:8,11,13,16)] %>%
  group_by(DT) %>%
  filter(prec == "prec0001") %>%
  mutate(length_mean = mean(length)) %>% 
  mutate(length_sd = sd(length)) %>%
  mutate(sd_mean = mean(sd_initial)) %>% 
  mutate(sd_sd = sd(sd_initial)) %>%
  mutate(p_trend_mean = mean(p_trend)) %>% 
  mutate(p_trend_sd = sd(p_trend)) %>%
  mutate(DT_perc_mean = mean(DT_perc)) %>% 
  mutate(DT_perc_sd = sd(DT_perc)) %>% 
  mutate(se_trend_mean = mean(se_trend)) %>% 
  mutate(se_trend_sd = sd(se_trend))

  
results_table <- results_table[c(1,10:19)] %>% 
  unique()
save(results_table, file = "data/results_table.Rdata")
xtable(results_table)

results_table_long <- results_table %>% 
  melt(id = c("DT"))


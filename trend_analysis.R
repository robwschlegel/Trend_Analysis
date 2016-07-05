###########################################################################
### "trend_analysis.R"
## This script shows all of the analyses performed for the paper
# 1. Load all packages required for the analyses
# 2. Load all functions etc. found in other scripts
# 3. Load the raw data and site list information
# 4. Manually screen and remove eroneous data/ large gaps
# 5. Calculate relevant statistics and meta-data per site and screen unwanted time series
# 6. Remove trends from time series to create "flat" time series and prep for modelling
# 7. "Grow" the time series
# 8. Expand the grown data frame to also show different DT and precision
# 9. Setup the models to be used

#############################################################################

#############################################################################
### 1. Load all packages required for the analyses
library(ggplot2)
library(grid)
library(zoo)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(nlme)
library(mgcv)
library(doMC); doMC::registerDoMC(cores = 8)

# library(tseries)
# library(xtable)
# library(FNN)
# library(gridExtra)
# library(pwr)

#############################################################################
### 2. Load all functions etc. found in other scripts
source("func/metaTemp.R") # Afunction that calculates meta-deta/ stats of time series
source("func/seqSites.R") # A function that orders sites correctly along the coast of South Africa
source("func/detrendFunc.R") # A function that removes any linear trend from a time series

#############################################################################
### 3. Load the raw data and site list information

# Monthly data
load("data/SACTNmonthly_v4.0.Rdata")
SACTNmonthly_v4.0$index <- as.factor(paste(SACTNmonthly_v4.0$site, SACTNmonthly_v4.0$src, sep = "/ "))

# Site list
site_list <- read.csv("setupParams/site_list_v4.0.csv")
site_list$index <- as.factor(paste(site_list$site, site_list$src, sep = "/ "))

# Number of time series belonging to each instrument type
length(site_list$type[site_list$type == "thermo"])
length(site_list$type[site_list$type == "old"])
length(site_list$type[site_list$type == "new"])

#############################################################################
### 4. Manually screen and remove eroneous data/ large gaps

# Visualise all time series
# ggplot(SACTNmonthly_v4.0, aes(x = date, y = temp)) +
#   geom_line() +
#   facet_wrap(~index)

# Manually remove unwanted rows of data
  # "Saldanha Bay/ SAWS", "Hout Bay/ DEA", "Kalk Bay/ SAWS", "Gansbaai/ SAWS", "Tinley Manor/ KZNSB"
SACTN_sub <- SACTNmonthly_v4.0[-c(2291:2401, 4132:4163, 5393:5442, 7358:7368, 28773:28775),] 
    # 207 rows (months) of data removed

# Manually remove unwanted time series
gaps <- c("Aliwal Shoal/ EKZNW", "Durban/ SAWS", "Umgababa/ KZNSB")
SACTN_sub <- droplevels(subset(SACTN_sub, !(index %in% gaps)))

# Check that corrections have been implemented
# ggplot(SACTN_sub, aes(x = date, y = temp)) +
#   geom_line() +
#   facet_wrap(~index)

# Match siteList to reduced time series
site_list_sub <- droplevels(subset(site_list, index %in% SACTN_sub$index))

#############################################################################
### 5. Calculate relevant statistics and meta-data per site and screen unwanted time series

# Calculate statistics and meta-data
wide <- dcast(SACTN_sub, date ~ site+src, value.var = "temp", mean)
wide_zoo <- zoo(wide[,2:length(colnames(wide))], as.Date(as.POSIXct(wide$date)))
data_summary <- adply(wide_zoo, 2, metaTemp)
names(data_summary)[c(1:7)] <- c("index", "start date", "end date", "length", "temp months", "NA months", "NA%")
data_summary$site <- sapply(strsplit(as.character(data_summary$index), "[_]"), "[[", 1)
data_summary$src <- sapply(strsplit(as.character(data_summary$index), "[_]"), "[[", 2)

# Reorder for project wide cohesion
data_summary <- seqSites(data_summary)

# Create reduced data.frame to remove more unwanted time series
data_summary2 <- cbind(site_list_sub, data_summary)
data_summary2 <- data_summary2[data_summary2$depth < 11, ]
# Set the minimum length to 62, rather than 60, to catch one time series that has less than 5 full calendar years of data
data_summary2 <- data_summary2[data_summary2$`temp months` >= 62, ]
data_summary2 <- data_summary2[data_summary2$`NA%` <= 15, ]
data_summary2 <- data_summary2[c(1:8, 10:19 ,9)]

# Remove them from the dataset
SACTN_sub2 <- droplevels(subset(SACTN_sub, index %in% data_summary2$index))

# Remove them from the site list
site_list_sub2 <- droplevels(subset(site_list_sub, index %in% data_summary2$index))

# Check what the final dataset looks like
ggplot(SACTN_sub2, aes(x = date, y = temp)) +
  geom_line() +
  facet_wrap(~index)

#############################################################################
### 6. Remove trends from time series to create "flat" time series and prep for modelling

SACTN_flat <- detrend(SACTN_sub2)

# Add necessary info for models
SACTN_flat <- SACTN_flat %>%
  group_by(site, src) %>%
  mutate(time = as.numeric(date)) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(num = 1:length(date))

save(SACTN_flat, file = "data/SACTN_flat.Rdata")
load("data/SACTN_flat.Rdata")

#############################################################################
### 7. "Grow" the time series

SACTN_grow <- data.frame()
for(i in 1:length(levels(SACTN_flat$index))){
  data1 <- data.frame(droplevels(subset(SACTN_flat, index == levels(SACTN_flat$index)[i])))
  if(length(data1$year[data1$year == levels(as.factor(data1$year))[1]]) < 12){
    data1 <- droplevels(subset(data1, year != levels(as.factor(data1$year))[1]))
  }
  if(length(data1$year[data1$year == levels(as.factor(data1$year))[length(levels(as.factor(data1$year)))]]) < 12){
    data1 <- droplevels(subset(data1, year != levels(as.factor(data1$year))[length(levels(as.factor(data1$year)))]))
  }
  if(length(levels(as.factor(data1$year))) < 5){
  } else {
    for(j in 5:length(levels(as.factor(data1$year)))){
      data2 <- droplevels(subset(data1, year %in% levels(as.factor(data1$year))[1:j]))
      data2$year_index <- j
      SACTN_grow <- rbind(SACTN_grow, data2)
    }
  }
}

save(SACTN_grow, file = "data/SACTN_grow.Rdata")
load("data/SACTN_grow.Rdata")

#############################################################################
### 8. Expand the grown data frame to also show different DT and precision

SACTN_full <- SACTN_grow %>%
  group_by(site, src, type, year_index) %>%
  mutate(time = as.numeric(date)) %>%
  ddply(.(site, src, year_index), mutate, DT000 = (seq(0, by = (0.00 / 120), length.out = length(date))) + temp) %>%
  ddply(.(site, src, year_index), mutate, DT005 = (seq(0, by = (0.05 / 120), length.out = length(date))) + temp) %>%
  ddply(.(site, src, year_index), mutate, DT010 = (seq(0, by = (0.10 / 120), length.out = length(date))) + temp) %>%
  ddply(.(site, src, year_index), mutate, DT015 = (seq(0, by = (0.15 / 120), length.out = length(date))) + temp) %>%
  ddply(.(site, src, year_index), mutate, DT020 = (seq(0, by = (0.20 / 120), length.out = length(date))) + temp) %>%
  select(-temp) %>%
  gather(DT, value = temp, DT000, DT005, DT010, DT015, DT020) %>%
  ddply(.(site, src, year_index), mutate, prec0001 = round_any(temp, 0.001)) %>%
  ddply(.(site, src, year_index), mutate, prec001 = round_any(temp, 0.01)) %>%
  ddply(.(site, src, year_index), mutate, prec01 = round_any(temp, 0.1)) %>%
  ddply(.(site, src, year_index), mutate, prec05 = round_any(temp, 0.5)) %>%
  select(-temp) %>%
  gather(prec, value = temp, prec0001, prec001, prec01, prec05) %>%
  tbl_df()

save(SACTN_full, file = "data/SACTN_full.Rdata")
load("data/SACTN_full.Rdata")

#############################################################################
### 9. Setup the models to be used

## GLS
gls_fun <- function(df) {
  out <- tryCatch(
    {
      model <- gls(temp ~ time,
                   correlation = corARMA(form = ~ time, p = 2), na.action = na.omit,
                   method = "ML", data = df)
      stats <- data.frame(DT_model = round(as.numeric(coef(model)[2])*3652.5,3),
                          se_trend = round(summary(model)[["tTable"]][[2,2]],10),
                          sd_initial = round(sd(df$temp),2),
                          sd_residual = round(sd(model$residuals),2),
                          r = NA,
                          R2 = NA,
                          p_trend = summary(model)[["tTable"]][[2,4]],
                          p_seas = NA,
                          length = length(df$temp),
                          model = "gls")
      return(stats)
    },
    error = function(cond) {
      stats <- data.frame(DT_model = NA,
                          se_trend = NA,
                          sd_initial = round(sd(df$temp),2),
                          sd_residual = NA,
                          r = NA,
                          R2 = NA,
                          p_trend = NA,
                          p_seas = NA,
                          length = length(df$temp),
                          model = "gls")
      return(stats)
    }
  )
  rm(model)
  return(out)
}

# Linear GAMM
gamm_lin_fun <- function(df) {
  out <- tryCatch(
    {
      model <- gamm(temp ~ s(month, bs = "cc") + time,
                    correlation = corARMA(form = ~ 1 | year, p = 2),
                    method = "REML", data = df, control = ctrl)
      stats <- data.frame(DT_model = round(as.numeric(summary(model$gam)$p.coeff[2]) * 3652.5, 3),
                          se_trend = round(as.numeric(summary(model$gam)$se[2]), 10),
                          sd_initial = round(sd(df$temp), 2),
                          sd_residual = round(sd(model$gam$residuals), 2),
                          r = round(sqrt(summary(model$gam)$r.sq), 2),
                          R2 = round(summary(model$gam)$r.sq, 2),
                          p_trend = as.numeric(summary(model$gam)$p.pv[2]),
                          p_seas = as.numeric(summary(model$gam)$s.pv),
                          length = length(df$temp),
                          model = "gamm_lin")
      return(stats)
    },
    error = function(cond) {
      stats <- data.frame(DT_model = NA,
                          se_trend = NA,
                          sd_initial = round(sd(df$temp), 2),
                          sd_residual = NA,
                          r = NA,
                          R2 = NA,
                          p_trend = NA,
                          p_seas = NA,
                          length = length(df$temp),
                          model = "gamm_lin")
      return(stats)
    }
  )
  rm(model)
  return(out)
}

# Non-linear GAMM

#############################################################################
### 10. Fit the models to the fully grown and expanded data and extract the results

## GLS
system.time(mod_gls <- dlply(SACTN_full, .(site, src, DT, prec, year_index), .progress = "text", .parallel = TRUE, gls_fun))
gls_df <- ldply(mod_gls, data.frame)

save(gls_df, file = "data/gls_df.RData")
load("data/gls_df.RData")

## Linear GAMM
system.time(mod_gamm_lin <- dlply(SACTN_full, .(site, src, DT, prec, year_index), .progress = "text", .parallel = TRUE, gamm_lin_fun))
gamm_lin_df <- ldply(mod_gamm_lin, data.frame)

save(gamm_lin_df , file = "data/gamm_lin_df.RData")
load("data/gamm_lin_df.RData")

## Non-linear GAMM
system.time(mod_gamm_non <- dlply(SACTN_full, .(site, src, DT, prec, year_index), .progress = "text", .parallel = TRUE, gamm_non_fun))
gamm_non_df <- ldply(mod_gamm_non, data.frame)

save(gamm_non_df, file = "data/gamm_non_df.RData")
load("data/gamm_non_df.RData")

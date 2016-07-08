# This script takes the SACTN data and assembles two expanded data files:
# 1. First the data are detrended.
# 2. No interpolation takes place to fill missing temperatures.
# 3. It grows the dataset to four levels of precision and five decadal trends.
# 4. Produces one set of only the 'natural' lengths (the lengths of the time series
# in the SACTN).
# 5. Produces another set fully 'grown' (each series greater than/equal to 5 years
# in length is replicated at lengths from 5 yrs to the natural length, at one
# year increments starting from 5 yrs).

# load all packages required for the analyses -----------------------------
# probably some are superfluous
library(zoo)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(tibble)

# load the 84 time series -------------------------------------------------
load("data/SACTN_sub2.Rdata")
colnames(SACTN_sub2)[8] <- "na_perc" # your woes started because you use "NA%" as a column name
SACTN_sub2 <- as.data.frame(SACTN_sub2)

# add necessary info for models -------------------------------------------
SACTN_flat_no_interp <- SACTN_sub2 %>%
  filter(complete.cases(temp)) %>%
  group_by(index) %>%
  mutate(time = as.numeric(date)) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(num = 1:length(date)) # to use as regressor

# detrend -----------------------------------------------------------------
resids <- ldply(dlply(SACTN_flat_no_interp, .(site, src, index),
                      function(df) as.numeric(residuals(lm(temp ~ time, data = df, na.action = na.omit)))), data.frame)
colnames(resids)[4] <- "residuals"
SACTN_flat_no_interp$temp <- resids$residuals

save(SACTN_flat_no_interp, file = "data/SACTN_flat_no_interp.Rdata")
# load("data/SACTN_flat_no_interp.Rdata")

# "grow" the time series --------------------------------------------------
SACTN_grown_no_interp <- data.frame()
for(i in 1:length(levels(SACTN_flat_no_interp$index))){
  data1 <- data.frame(droplevels(subset(SACTN_flat_no_interp, index == levels(SACTN_flat_no_interp$index)[i])))
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
      SACTN_grown_no_interp <- rbind(SACTN_grown_no_interp, data2)
    }
  }
}

save(SACTN_grown_no_interp, file = "data/SACTN_grown_no_interp.Rdata")
# load("data/SACTN_grown_no_interp.Rdata")
# load("data/SACTN_grow_interp.Rdata")

# Expand data to show different DT and precision --------------------------
# Uncomment as follows:
# 1. SACTN_flat_no_interp to expand the flat, uninterpolated, natural data; or
# 2. SACTN_grown_no_interp to expand the flat, uninterpolated, grown data

SACTN_full_natural_no_interp <- SACTN_flat_no_interp %>% # 1.
# SACTN_full_grown_no_interp <- SACTN_grown_no_interp %>% # 2.
  group_by(site, src, type) %>%
  mutate(time = as.numeric(date)) %>%
  ddply(.(site, src), mutate, DT000 = (seq(0, by = (0.00 / 120), length.out = length(date))) + temp) %>%
  ddply(.(site, src), mutate, DT005 = (seq(0, by = (0.05 / 120), length.out = length(date))) + temp) %>%
  ddply(.(site, src), mutate, DT010 = (seq(0, by = (0.10 / 120), length.out = length(date))) + temp) %>%
  ddply(.(site, src), mutate, DT015 = (seq(0, by = (0.15 / 120), length.out = length(date))) + temp) %>%
  ddply(.(site, src), mutate, DT020 = (seq(0, by = (0.20 / 120), length.out = length(date))) + temp) %>%
  select(-temp) %>%
  gather(DT, value = temp, DT000, DT005, DT010, DT015, DT020) %>%
  ddply(.(site, src), mutate, prec0001 = round_any(temp, 0.001)) %>%
  ddply(.(site, src), mutate, prec001 = round_any(temp, 0.01)) %>%
  ddply(.(site, src), mutate, prec01 = round_any(temp, 0.1)) %>%
  ddply(.(site, src), mutate, prec05 = round_any(temp, 0.5)) %>%
  select(-temp) %>%
  gather(prec, value = temp, prec0001, prec001, prec01, prec05) %>%
  tbl_df()

save(SACTN_full_natural_no_interp, file = "data/SACTN_full_natural_no_interp.Rdata")
save(SACTN_full_grown_no_interp, file = "data/SACTN_full_grown_no_interp.Rdata")
# load("data/SACTN_full_natural_no_interp.Rdata")
# load("data/SACTN_full_grown_no_interp.Rdata")

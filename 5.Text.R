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
library(tibble)
library(doMC); doMC::registerDoMC(cores = 4)

#############################################################################
## 2. Load all functions etc. found in other scripts
source("func/metaTemp.R") # Afunction that calculates meta-deta/ stats of time series
source("func/seqSites.R") # A function that orders sites correctly along the coast of South Africa
source("setupParams/themes.R") # The ggplot theme used for all figures

#############################################################################
### 3. Introduction

# No stats are given in the intro

#############################################################################
### 4. Methods

## Data Sources

# Count of type of instrument used for time series
site_list <- read.csv("setupParams/site_list_v4.0.csv")
site_list$index <- as.factor(paste(site_list$site, site_list$src, sep = "/ "))
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

# Load model results
load("data/gls_fitted_full_nointerp_natural.RData")
gls_df_natural <- gls_df; rm(gls_df)
gls_df_natural$index <- as.factor(paste(gls_df_natural$site, gls_df_natural$src, sep = "/ "))

# Load the data that has percent NA and type columns to add them to the GLS data frame
load("data/SACTN_sub2.Rdata")
colnames(SACTN_sub2)[8] <- "na_perc"
SACTN_sub2 <- as.data.frame(SACTN_sub2)

# Add "DT_real" column as numeric values
gls_df_natural$DT_real <- NA
gls_df_natural$DT_real[gls_df_natural$DT == "DT000"] <- 0.00
gls_df_natural$DT_real[gls_df_natural$DT == "DT005"] <- 0.05
gls_df_natural$DT_real[gls_df_natural$DT == "DT010"] <- 0.10
gls_df_natural$DT_real[gls_df_natural$DT == "DT015"] <- 0.15
gls_df_natural$DT_real[gls_df_natural$DT == "DT020"] <- 0.20

# Add "prec_real" column
gls_df_natural$prec_real <- NA
gls_df_natural$prec_real[gls_df_natural$prec == "prec0001"] <- 0.001
gls_df_natural$prec_real[gls_df_natural$prec == "prec001"] <- 0.01
gls_df_natural$prec_real[gls_df_natural$prec == "prec01"] <- 0.1
gls_df_natural$prec_real[gls_df_natural$prec == "prec05"] <- 0.5

# Add coast column
gls_df_natural$coast <- NA
gls_df_natural$coast[gls_df_natural$index %in% levels(droplevels(site_list$index[site_list$coast == "wc"]))] <- "wc"
gls_df_natural$coast[gls_df_natural$index %in% levels(droplevels(site_list$index[site_list$coast == "sc"]))] <- "sc"
gls_df_natural$coast[gls_df_natural$index %in% levels(droplevels(site_list$index[site_list$coast == "ec"]))] <- "ec"

# Calculate "DT_perc", the percentage different between the real DT and the modelled DT
gls_df_natural <- gls_df_natural %>% 
  mutate(DT_perc = ((DT_real-DT_model)/DT_real)*100)

# Add NA%
gls_df_natural <- gls_df_natural%>%
  group_by(index) %>%
  mutate(na_perc = SACTN_sub2$na_perc[SACTN_sub2$index == index][1])
gls_df_natural <- data.frame(gls_df_natural)

# Remove rows with missing data
gls_df_natural_no_0 <- gls_df_natural %>% 
  filter(DT_real != 0.00)

## Intro
# Quantify the effect on DT_perc AND p_trend of: length, DT, SD_initial, NA, precision
  # Use linear models as this more broadly measures the relationship between the variables
  # Not using min and max values as these are only two data points and could be misrepresentative

## Histograms of residuals after fitting GLM to the transformed and untransformed data to find best model
# hist(glm(abs(DT_perc)~length, data = gls_df_natural_no_0, family = gaussian)$residuals) # Gaussian appears better...
# hist(glm(abs(DT_perc)~sin(length), data = gls_df_natural_no_0, family = gaussian)$residuals) # Not as good...
# hist(glm(abs(DT_perc)~log(length), data = gls_df_natural_no_0, family = gaussian)$residuals)
# 
# hist(glm(abs(DT_perc)~length, data = gls_df_natural_no_0, family = gaussian)$residuals) # Gaussian appears better...
# hist(glm(sin(abs(DT_perc))~length, data = gls_df_natural_no_0, family = gaussian)$residuals)
# hist(glm(log(abs(DT_perc))~length, data = gls_df_natural_no_0, family = gaussian)$residuals)
# 
# hist(glm(abs(DT_perc)~length, data = gls_df_natural_no_0, family = poisson)$residuals) # Poisson gives less normal distribution

## Create model to be used on the data

# Testing...
# df <- gls_df_natural_no_0 %>% 
#   filter(DT == "DT010", prec == "prec01", coast == "sc")

# Set model for use with: "length", "DT_real", "sd_initial", "na_perc", "prec_real"
variable.effects <- function(df) {
  # Effect of length
  model_DT <- glm(abs(DT_perc)~length, family = gaussian, data = df)
  model_p <- glm(p_trend~length, family = gaussian, data = df)
  stats_len <- data.frame(
    var = rep("length", 2),
    stat = c("DT_perc", "p_trend"),
    intercept = c(round(as.numeric(coef(model_DT)[1]), 4),
                  round(as.numeric(coef(model_p)[1]), 4)),
    slope = c(round(as.numeric(coef(model_DT)[2]), 4),
              round(as.numeric(coef(model_p)[2]), 4))
  )
  # Effect of DT_real
    ## NB: These results will be NA when "DT" is used as a grouping variable in plyr!
  model_DT <- glm(abs(DT_perc)~DT_real, family = gaussian, data = df)
  model_p <- glm(p_trend~DT_real, family = gaussian, data = df)
  stats_DT <- data.frame(
    var = rep("DT_real", 2),
    stat = c("DT_perc", "p_trend"),
    intercept = c(round(as.numeric(coef(model_DT)[1]), 4),
                  round(as.numeric(coef(model_p)[1]), 4)),
    slope = c(round(as.numeric(coef(model_DT)[2]), 4),
              round(as.numeric(coef(model_p)[2]), 4))
  )
  # Effect of sd_initial
  model_DT <- glm(abs(DT_perc)~sd_initial, family = gaussian, data = df)
  model_p <- glm(p_trend~sd_initial, family = gaussian, data = df)
  stats_sd <- data.frame(
    var = rep("sd_initial", 2),
    stat = c("DT_perc", "p_trend"),
    intercept = c(round(as.numeric(coef(model_DT)[1]), 4),
                  round(as.numeric(coef(model_p)[1]), 4)),
    slope = c(round(as.numeric(coef(model_DT)[2]), 4),
              round(as.numeric(coef(model_p)[2]), 4))
  )
  # Effect of na_perc
  model_DT <- glm(abs(DT_perc)~na_perc, family = gaussian, data = df)
  model_p <- glm(p_trend~na_perc, family = gaussian, data = df)
  stats_na <- data.frame(
    var = rep("na_perc", 2),
    stat = c("DT_perc", "p_trend"),
    intercept = c(round(as.numeric(coef(model_DT)[1]), 4),
                  round(as.numeric(coef(model_p)[1]), 4)),
    slope = c(round(as.numeric(coef(model_DT)[2]), 4),
              round(as.numeric(coef(model_p)[2]), 4))
  )
  # Effect of prec_real
    ## NB: These results will be NA when "prec" is used as a grouping variable in plyr!
  model_DT <- glm(abs(DT_perc)~prec_real, family = gaussian, data = df)
  model_p <- glm(p_trend~prec_real, family = gaussian, data = df)
  stats_prec <- data.frame(
    var = rep("prec_real", 2),
    stat = c("DT_perc", "p_trend"),
    intercept = c(round(as.numeric(coef(model_DT)[1]), 4),
                  round(as.numeric(coef(model_p)[1]), 4)),
    slope = c(round(as.numeric(coef(model_DT)[2]), 4),
              round(as.numeric(coef(model_p)[2]), 4))
  )
  # Wrap it up!
  stats <- rbind(stats_len, stats_DT, stats_sd, stats_na, stats_prec)
  stats$accuracy <- NA
  stats$accuracy[stats$stat == "DT_perc"] <- stats$intercept[stats$stat == "DT_perc"]/-stats$slope[stats$stat == "DT_perc"]
  stats$accuracy[stats$stat == "p_trend"] <- (0.05-stats$intercept)[stats$stat == "p_trend"]/stats$slope[stats$stat == "p_trend"]
  return(stats)
}

# Calculate relationship of DT_perc with length for all DTs and precisions
  # Change the grouping variables in the following line to change how the interactions of variables are viewed
mod_vars <- dlply(gls_df_natural_no_0[gls_df_natural_no_0$DT == "DT010",], .(DT, round_any(sd_initial,0.5), prec), .progress = "text", .parallel = FALSE, variable.effects)
vars_df <- ldply(mod_vars, data.frame, .progress = "text")
vars_df_sub <- vars_df %>% 
  filter(var == "length")

save(vars_df, file = "data/vars_df_all.Rdata")

# Visualise the trend lines
ggplot(data=gls_df_natural_no_0, aes(x = sd_initial, y=abs(DT_perc)))+ # Visualise test
  geom_point(aes(colour = length)) +
  geom_smooth(method = "glm")


# Length #
ggplot(data=gls_df_natural_no_0, aes(x = DT_real, y=abs(DT_perc)))+ # Visualise test
  geom_point() +
  geom_smooth(method = "glm")
(max(gls_df_natural_no_0$length) * coef(lm(abs(abs(DT_perc))~length, data = gls_df_natural_no_0))[2] + coef(lm(abs(DT_perc)~length, data = gls_df_natural_no_0))[1]) - (min(gls_df_natural_no_0$length) * coef(lm(abs(DT_perc)~length, data = gls_df_natural_no_0))[2] + coef(lm(abs(DT_perc)~length, data = gls_df_natural_no_0))[1])
  # -112.14% is the spread of DT accuracy imposed by the natural length range in the dataset

# predict(gls_df_natural_no_0, lm(abs(abs(DT_perc))~length))

# DT #
(max(gls_df_natural_no_0$DT_real) * coef(lm(abs(DT_perc)~DT_real, data = gls_df_natural_no_0))[2] + coef(lm(abs(DT_perc)~DT_real, data = gls_df_natural_no_0))[1]) - (min(gls_df_natural_no_0$DT_real) * coef(lm(abs(DT_perc)~DT_real, data = gls_df_natural_no_0))[2] + coef(lm(abs(DT_perc)~DT_real, data = gls_df_natural_no_0))[1])
  # -71.66% range

# sd_initial #
# ggplot(data=gls_df_natural_no_0[,c(7,17)], aes(x = sd_initial, y=abs(DT_perc)))+ # Visualise test
#   geom_point() +
#   geom_smooth(method = "lm")
(max(gls_df_natural_no_0$sd_initial) * coef(lm(abs(DT_perc)~sd_initial, data = gls_df_natural_no_0))[2] + coef(lm(abs(DT_perc)~sd_initial, data = gls_df_natural_no_0))[1]) - (min(gls_df_natural_no_0$sd_initial) * coef(lm(abs(DT_perc)~sd_initial, data = gls_df_natural_no_0))[2] + coef(lm(abs(DT_perc)~sd_initial, data = gls_df_natural_no_0))[1])
  # -14.84% range
    # Admittedly, a linear relationship isn't the best description

# NA% #
(max(gls_df_natural_no_0$na_perc) * coef(lm(abs(DT_perc)~na_perc, data = gls_df_natural_no_0))[2] + coef(lm(abs(DT_perc)~na_perc, data = gls_df_natural_no_0))[1]) - (min(gls_df_natural_no_0$na_perc) * coef(lm(abs(DT_perc)~na_perc, data = gls_df_natural_no_0))[2] + coef(lm(abs(DT_perc)~na_perc, data = gls_df_natural_no_0))[1])
  # 12.84% range

# Precision #
  # Run analysis
(max(gls_df_natural_no_0_precision$prec_real) * coef(lm(abs(DT_perc)~prec_real, data = gls_df_natural_no_0_precision))[2] + coef(lm(abs(DT_perc)~prec_real, data = gls_df_natural_no_0_precision))[1]) - (min(gls_df_natural_no_0_precision$prec_real) * coef(lm(abs(DT_perc)~prec_real, data = gls_df_natural_no_0_precision))[2] + coef(lm(abs(DT_perc)~prec_real, data = gls_df_natural_no_0_precision))[1])
  # -0.20% range
ggplot(data = gls_df_natural_no_0_precision[,c(17,19)],
       aes(x = prec_real, y=abs(DT_perc)))+ # Visualise test
  geom_point() +
  geom_smooth(method = "lm")

## Magnitude of decadal trend determined by GLS
# Correlation between abs(DT_perc) vs. SD_initial
DT_vs_SD <- gls_df_natural_no_0 %>%
  filter(prec == "prec0001") %>% 
  group_by(DT) %>% 
  mutate(r2 = cor(abs(DT_perc), sd_initial)) %>% 
  mutate(p = as.numeric(cor.test(abs(DT_perc), sd_initial)[3]))

DT_vs_SD <- DT_vs_SD[,c(3,20:21)] %>% 
  unique() # r = -0.05

## Significance p-value of the model fit

# Number of significant trends
length(gls_df_natural$p_trend) # 1680 modelled fits
length(gls_df_natural$p_trend[gls_df_natural$p_trend <= 0.05])
gls_df_natural[gls_df_natural$p_trend <= 0.05,]

## Error associated with trend estimate

# Correlation between abs(DT_perc) vs. se_trend
DT_vs_SE <- gls_df_natural_no_0 %>%
  filter(prec == "prec0001") %>% 
  group_by(DT) %>% 
  mutate(r2 = cor(abs(DT_perc), se_trend)) %>% 
  mutate(p = as.numeric(cor.test(abs(DT_perc), se_trend)[3]))

DT_vs_SE <- DT_vs_SE[,c(3,20:21)] %>% 
  unique() # r = 0.57, p < 0.001

## Effect of % missing values on trends estimation

# Count of time series missing X%
length(levels(droplevels(gls_df_natural$index[gls_df_natural$na_perc < 1]))) #25
length(levels(droplevels(gls_df_natural$index[gls_df_natural$na_perc < 5]))) #70-25=45
length(levels(droplevels(gls_df_natural$index[gls_df_natural$na_perc < 10]))) #80-70=10
length(levels(droplevels(gls_df_natural$index[gls_df_natural$na_perc < 15]))) #84
mean(gls_df_natural$na_perc) # 2.65

# Correlation between p_trend vs. %NA
ptrend_vs_NA <- gls_df_natural_no_0 %>%
  filter(prec == "prec0001") %>% 
  group_by(DT) %>% 
  mutate(r2 = cor(p_trend, na_perc)) %>% 
  mutate(p = as.numeric(p_trend, na_perc)[3])

ptrend_vs_NA <- ptrend_vs_NA[,c(3,20:21)] %>% 
  unique() # r = 0.57, p < 0.001

## Create a grouping column so that the correlation by missing percent can be calculated
# Correlation between p_trend vs. %NA controlling for %NA
ptrend_vs_NA <- gls_df_natural_no_0 %>%
  filter(prec == "prec0001") %>% 
  # group_by(na_group) %>% 
  group_by(DT) %>% 
  mutate(r2 = cor(p_trend, na_perc)) %>% 
  mutate(p = as.numeric(p_trend, na_perc)[3])

ptrend_vs_NA <- ptrend_vs_NA[,c(3,20:21)] %>% 
  unique() # r = 0.57, p < 0.001


## Effect of measurement precision on trend estimation
prec_results_table <- gls_df_natural[,c(3:8,11,13,17:19)] %>%
  group_by(DT, prec) %>%
  mutate(sd_mean = mean(sd_initial)) %>% 
  mutate(sd_sd = sd(sd_initial)) %>%
  mutate(p_trend_mean = mean(p_trend)) %>% 
  mutate(p_trend_sd = sd(p_trend)) %>%
  mutate(DT_perc_mean = mean(DT_perc)) %>% 
  mutate(DT_perc_sd = sd(DT_perc)) %>% 
  mutate(se_trend_mean = mean(se_trend)) %>% 
  mutate(se_trend_sd = sd(se_trend))
prec_results_table <- prec_results_table[c(1:2,12:19)] %>% 
  unique()
save(prec_results_table, file = "data/prec_results_table.Rdata")

prec_range_results_table <- prec_results_table %>% 
  group_by(DT) %>% 
  mutate(sd_mean = range(sd_mean)[2]-range(sd_mean)[1]) %>% 
  mutate(sd_sd = range(sd_sd)[2]-range(sd_sd)[1]) %>%
  mutate(p_trend_mean = range(p_trend_mean)[2]-range(p_trend_mean)[1]) %>% 
  mutate(p_trend_sd = range(p_trend_sd)[2]-range(p_trend_sd)[1]) %>%
  mutate(DT_perc_mean = range(DT_perc_mean)[2]-range(DT_perc_mean)[1]) %>% 
  mutate(DT_perc_sd = range(DT_perc_sd)[2]-range(DT_perc_sd)[1]) %>% 
  mutate(se_trend_mean = range(se_trend_mean)[2]-range(se_trend_mean)[1]) %>% 
  mutate(se_trend_sd = range(se_trend_sd)[2]-range(se_trend_sd)[1])
  # No one "prec" provides the best fit, but "prec05" is always the worst

prec_range_results_table <- prec_range_results_table[c(1,3:10)] %>% 
  unique()

# Manually assign best fitting "prec" as piping isn't working
prec_range_results_table$best <- c(NA, "prec01", "prec001", "prec01", "prec0001")
save(prec_range_results_table, file = "data/prec_range_results_table.Rdata")

## Pay-off between length and prec05
# First visualise data
gls_df_natural_no_0 %>% 
  group_by(DT, prec) %>% 
  ggplot(aes(x = length, y = abs(DT_perc))) +
  geom_point() +
  geom_line() +
  geom_smooth(se = FALSE, method = "lm") +
  facet_grid(prec~DT)

# Set model for use
lm_fun<- function(df) {
    model <- glm(abs(DT_perc)~length, data = df)
    stats <- data.frame(
        intercept = round(as.numeric(coef(model)[1]), 4),
        slope = round(as.numeric(coef(model)[2]), 4)
      )
    return(stats)
}

# Calculate relationship of DT_perc with length for all DTs and precisions
mod_lm <- dlply(gls_df_natural_no_0, .(DT, prec), .progress = "text", .parallel = FALSE, lm_fun)
DT_perc_length_df <- ldply(mod_lm, data.frame, .progress = "text")
  ## The results are very interesting but I will need to take a look at them with fresh eyes
ggplot(lm_df, aes(x = intercept, y = slope)) +
  geom_point(aes(colour = DT, shape = prec)) #+
  # geom_line(aes(colour = DT))
  ## The effect of DT on the relationship between length and DT_perc appears very significant
  ## The effect of prec05 also appears significantly different from the other precisions
  ## Also, the effect of DT on the aforementioned relationship appears oddly linear
    ## What could that mean?

# Calculate when each pairing would allow the modelled slope to become the same as the real slope
DT_perc_length_df <- DT_perc_length_df %>% 
  mutate(singularity = intercept/slope)
  # This may not be the best way to measure this...
    # Perhaps calculating when the slope first enters a 90% CI range would be better...

## Is it easier to detect a trend on which coast?


# Number of sites per coast
length(levels(droplevels(gls_nat$index[gls_nat$coast == "wc"]))) # 16
length(levels(droplevels(gls_nat$index[gls_nat$coast == "sc"]))) # 21
length(levels(droplevels(gls_nat$index[gls_nat$coast == "ec"]))) # 47

# Shape data for plotting
dat <- gls_nat %>%
  filter(prec == "prec001") %>%
  select(site, src, DT, DT_model, se_trend, sd_initial, sd_residual,
         p_trend, length, coast) %>%
  unite(fac, site, src, remove = FALSE)
dat$DT[dat$DT == "DT000"] <- 0
dat$DT[dat$DT == "DT005"] <- 0.05
dat$DT[dat$DT == "DT010"] <- 0.10
dat$DT[dat$DT == "DT015"] <- 0.15
dat$DT[dat$DT == "DT020"] <- 0.20
dat$DT <- as.numeric(dat$DT)

dat %>%
  ggplot(aes(x = sd_initial, y = p_trend)) + bw_update +
  geom_hline(yintercept = 0.05, col = "red") +
  geom_point(aes(size = length, colour = coast), shape = 21, stroke = 0.2) +
  geom_smooth(aes(colour = coast), method = "glm") +
  scale_y_continuous(name = "p-value", limits = c(0, 1)) +
  scale_x_continuous(name = expression(paste("Initial SD (", degree, "C)"))) +
  scale_size_continuous(name = "Length (months)") +
  facet_wrap("DT", ncol = 1)
ggsave("graph/all_plt4_no_interp_natural_coast.pdf", plot = last_plot(), width = 5, height = 7,
       units = "in")

#############################################################################
### 6. Discussion

# Mean length of themo vs UTR time series
gls_df_natural$type <- NA
gls_df_natural$type[gls_df_natural$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "thermo"]))] <- "thermo"
gls_df_natural$type[gls_df_natural$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "old"]))] <- "UTR"
gls_df_natural$type[gls_df_natural$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "new"]))] <- "UTR"

mean(gls_df_natural$length[gls_df_natural$type == "thermo"]) # 349
mean(gls_df_natural$length[gls_df_natural$type == "UTR"]) # 167


#############################################################################
### 7. Conclussion

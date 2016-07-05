## This script shows the analyses performed to create any statistics etc. referenced in the text

# Functions
source("func/tsaFunc.R"); source("func/coastlineStatsFunc.R"); source("func/metaTemp.R"); source("func/seqSites.R"); source("setupParams/theme.R"); source("func/scaleBarFunc.R")

# Packages
require(tseries); require(zoo); require(vegan); require(ggplot2); require(xtable); require(reshape2); require(FNN); require(gridExtra); require(grid); require(lubridate); require(pwr); require(nlme); require(mgcv); require(dplyr); require(tidyr); require(purrr); require(broom); require(doMC)
doMC::registerDoMC(cores = 8)

### Introduction


### Methods

## Data Sources

# Oldest time series

# Lengths of time series

# COunt of 'old', 'new' and 'thermo' time series

## Data Management

# Number of days and months in SACTN
load("~/SACTNraw/data/SACTNdaily_v4.0.Rdata")
length(na.omit(SACTNdaily_v4.0$temp)) # 637,623

load("~/SACTNraw/data/SACTNmonthly_v4.0.Rdata")
SACTNmonthly_v4.0$index <- paste(SACTNmonthly_v4.0$site, SACTNmonthly_v4.0$src, sep = "/ ")
length(na.omit(SACTNmonthly_v4.0$temp)) # 28,569

# Visualisation of data to see which time series have large gaps etc.
ggplot(SACTNmonthly_v4.0, aes(x = date, y = temp)) +
  geom_line() +
  facet_wrap(~index)

# Manually remove unwanted rows of data
  # "Saldanha Bay/ SAWS", "Hout Bay/ DEA", "Kalk Bay/ SAWS", "Gansbaai/ SAWS", "Tinley Manor/ KZNSB" # 5 time series
SACTNmonthly_v4.0 <- SACTNmonthly_v4.0[-c(2291:2401, 4132:4163, 5393:5442, 7358:7368, 28773:28775),] 
    # 207 rows (months) of data removed

# Manually remove unwanted time series
gaps <- c("Aliwal Shoal/ EKZNW", "Durban/ SAWS", "Umgababa/ KZNSB") # 3 time series
SACTNmonthly_v4.0 <- droplevels(subset(SACTNmonthly_v4.0, !(index %in% gaps)))

# Calculate meta-data
wide <- dcast(SACTNmonthly_v4.0, date ~ site+src, value.var = "temp", mean)
wide_zoo <- zoo(wide[,2:length(colnames(wide))], as.Date(as.POSIXct(wide$date)))
source("func/metaTemp.R")
data.summary <- adply(wide_zoo, 2, metaTemp)
names(data.summary)[c(1:7)] <- c("index", "start date", "end date", "length", "temp months", "NA months", "NA%")
data.summary$site <- sapply(strsplit(as.character(data.summary$index), "[_]"), "[[", 1)
data.summary$src <- sapply(strsplit(as.character(data.summary$index), "[_]"), "[[", 2)

# Reorder for project wide cohesion
data.summary <- seqSites(data.summary)

# Load site list
siteList <- read.csv("setupParams/site_list_v4.0.csv")
siteList$index <- paste(siteList$site, siteList$src, sep = "/ ")

# Change UTR labels for plotting
siteList2 <- siteList
levels(siteList2$type)[levels(siteList2$type) == "UTR"] <- "new"
siteList2$type <- as.character(siteList2$type)
siteList2$src <- as.character(siteList2$src)
siteList2$type[siteList2$src == "DEA"] <- "old"
siteList2$type[c(99,128)] <- "new" # "Aliwal Shoal/ DEA", "Sodwana/ DEA"
siteList2$type <- as.factor(siteList2$type)
siteList2$src <- as.factor(siteList2$src)

# Match siteList to reduced time series
siteList3 <- droplevels(subset(siteList2, index %in% SACTNmonthly_v4.0$index))

# Create reduced data.frame to remove more unwanted time series
data.summary2 <- cbind(siteList3, data.summary)
data.summary2 <- data.summary2[data.summary2$depth < 11, ] # Removes 5 time series
data.summary2 <- data.summary2[data.summary2$`temp months` >= 62, ] # Removes 33 time series
    # NB: Minimum length set to 62 months, rather than 60 as in the text, as further length restrictions imposed later in the analysis remove one additional time series
data.summary2 <- data.summary2[data.summary2$`NA%` <= 15, ] # Remove 4 time series
data.summary2$index <- paste(data.summary2$site, data.summary2$src, sep = "/ ")

# Remove them from the dataset
SACTNmonthly_v4.0 <- droplevels(subset(SACTNmonthly_v4.0, index %in% data.summary2$index))
length(levels(as.factor(SACTNmonthly_v4.0$index))) # 84
length(na.omit(SACTNmonthly_v4.0$temp)) # 25,636

# Check against the modelled data
load("data/gls_df_2016-07-04.RData")
gls_df$index <- as.factor(paste(gls_df$site, gls_df$src, sep = "/ "))
length(levels(gls_df$index))

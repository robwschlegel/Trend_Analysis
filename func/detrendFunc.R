###############################################################################
### "func/detrendFunc.R"
## DESCRIPTION: This function uses a linear model to identify the overall trend of a time series and removes it to create a "flat" time series
## USAGE:
## ARGUMENTS:
## DETAILS:
## VALUE:
## AUTHORS(S): Robert Schlegel
## REFERENCE(S):
## EXAMPLE(S):
## DEPENDS: 
library(zoo)
## USED BY: "trend_analysis.R"
##############################################################################

detrend <- function(data){
  data5 <- data.frame()
  for(i in 1:length(levels(as.factor(data$index)))){
    data2 <- droplevels(subset(data, index == levels(as.factor(data$index))[i]))
    zoodata2 <- zoo(data2$temp, data2$date)
    zoodata2 <- na.trim(zoodata2)
    zoodata2 <- na.approx(zoodata2) # This reduncy allows the row names to show the dates
    data3 <- data.frame(temp = zoodata2)
    data3$date <- as.Date(row.names(data3))
    lm2 <- lm(temp~date, data3)
    data3$temp <- lm2$residuals
    data4 <- data.frame(site = data2$site[1:length(data3$temp)], src = data2$src[1:length(data3$temp)],
                        date = data3$date, temp = data3$temp, depth = data2$depth[1:length(data3$temp)],
                        type = data2$type[1:length(data3$temp)], index = data2$index[1:length(data3$temp)])
    data5 <- rbind(data5, data4)
  }
  return(data5)
}
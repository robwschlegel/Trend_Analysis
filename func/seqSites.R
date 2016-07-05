###############################################################################
### "func/seqSites.R"
## DESCRIPTION: This function subsets a data.frame (data) with a premade list of sites (sites) extracting the sites of interest and rearranging them in the correct west to east order.
## USAGE: You must be in the project root directory to use this function. 
## ARGUMENTS: The (data) must have columns labeled "site" and "src"
## DETAILS:
## VALUE:
## AUTHORS(S):
## REFERENCE(S):
## EXAMPLE(S):
## DEPENDS:
# "setupParams/site_list_v4.0.csv"
## USED BY: "trend_analysis.R", "text.R"
##############################################################################

seqSites <- function(data, sites = data$site, src = data$src, ...){
  seqSite <- read.csv("setupParams/site_list_v4.0.csv")
  seqSite$index <- 1:length(seqSite$site)
  seqSite$index2 <- paste(seqSite$site, seqSite$src, sep = "/ ")
  data$index2 <- paste(data$site, data$src, sep = "/ ")
  seqSite$index2 <- reorder(seqSite$index2, seqSite$index) # Convert character vector into ordered factor vector
  seqSite2 <- droplevels(subset(seqSite, (index2 %in% data$index2)))
  data2 <- droplevels(subset(data, (index2 %in% seqSite2$index2)))
  index <- data.frame()
  for(i in 1:length(levels(seqSite2$index2))){
    data3 <- droplevels(subset(data2, index2 == levels(seqSite2$index2)[i]))
    seqSite3 <- droplevels(subset(seqSite2, index2 == levels(seqSite2$index2)[i]))
    data3$index <- seqSite3$index
    index <- rbind(index, data3)
  }
  index <- index[order(index$index),]
  index$index <- NULL; index$index2 <- NULL # Removes both index column
  rownames(index) <- NULL
  index$index <- paste(index$site, index$src, sep = "/ ")
  return(index)
}
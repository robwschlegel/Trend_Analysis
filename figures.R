###########################################################################
### "figures.R"
## This script shows the code used to generate the figures for the paper
# 1. Load all packages required for the analyses
# 2. Load all functions etc. found in other scripts
# 3. Load the site list and spatial data used for the map
# 4. Create the map of Africa to be windowed
# 5. Create the map of southern Africa
# 6. Add site list information
# 7. Create figure 1
# 8. Load analysis results
# 9. 
#############################################################################

#############################################################################
### 1. Load all packages required for the analyses
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
### 2. Load all functions etc. found in other scripts
source("func/scaleBarFunc.R") # A custom ggplot function that creates a very snazy scale bar
source("graph/theme.R") # The ggplot theme used for all figures

#############################################################################
### 3. Load the site list and spatial data used for the map

## The site list
sites <- read.csv("setupParams/site_list_v4.0.csv")

## Coastline of African Continent
load("graph/africa_coast.RData")

## Borders of African countries
load("graph/africa_borders.Rdata")

## Coastline of Southern Africa
load("graph/south_africa_coast.RData")

## Province borders
load("graph/sa_provinces_new.RData")
# Reduce prvonice border resolution
# sa_provinces_new$index <- 1:12 # Reduce it by 92%
# sa_provinces_new <- droplevels(subset(sa_provinces_new, index == 1))

##########################################################################
### 4. Create the map of Africa to be windowed

## The map + SA filled in
africa <- ggplot(africa_coast, aes(x = lon, y = lat)) + # Select the main dataset with which to graph
  theme_bw() + # Set the theme to black and white
  coord_equal() + # Forces lon/ lat to be equitably displayed so that the map isn't squished
  geom_polygon(aes(group = group), colour = "black", fill = "grey80") + # Draw the coast
  geom_polygon(data = sa_provinces_new, (aes(group = group))) +
  annotate("text", label = "Africa", x = 16.0, y = 15.0, size = 3) + # Change Africa label size and position
  theme(panel.border = element_rect(colour = "black", size = 0.4), # Creates border
        plot.background = element_blank(), # Makes background transparent
        axis.ticks = element_blank(), # Remove tick marks
        axis.text = element_blank(), # Remove lat/ lon numbers
        axis.title = element_blank(), # Remove lat/ lon labels
        panel.grid.major = element_blank(), # Removes major grid lines
        panel.grid.minor = element_blank()) +# Removes minor grid lines
  coord_map(xlim = c(-20, 53), ylim = c(-36, 38), projection = "mercator") # Constricts view to Africa
africa

##########################################################################
### The map of southern Africa

## Create the base figure
SA <- ggplot() + coord_equal() + theme_bw() +
  # Landmass
  geom_polygon(data = south_africa_coast, aes(x = lon, y = lat, group = group), 
               colour = NA, fill = "grey80") +
  # International borders
  geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), 
            size = 1.0, colour = "black") +
  # Thick coastal border
  geom_polygon(data = south_africa_coast, aes(x = lon, y = lat, group = group),
               size = 1.0, colour = "black", fill = NA) +
  # Scale bar
  scaleBar(lon = 29, lat = -35.8, distanceLon = 200, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
           arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
  # Map plotting limits
  coord_cartesian(xlim = c(14.5, 33.5), ylim = c(-27, -36)) +
  theme(axis.title = element_blank()) # Remove lat/ lon labels)
SA

##########################################################################
### # 6. Add site list information

map <- SA + 
  # Oceans
  annotate("text", label = "Indian\nOcean", x = 32.60, y = -32.9, size = 4.0, angle = 0) +
  annotate("text", label = "Atlantic\nOcean", x = 15.50, y = -32.9, size = 4.0, angle = 0) +
  # Benguela
  geom_segment(aes(x = 17.2, y = -32.6, xend = 15.2, yend = -29.5),
               arrow = arrow(length = unit(0.4, "cm")), size = 1.0, colour = "grey50") +
  annotate("text", label = "Benguela", x = 16.1, y = -31.5, size = 3.0, angle = 300) +
  # Agulhas
  geom_segment(aes(x = 33, y = -29.5, xend = 29.8, yend = -33.0),
               arrow = arrow(length = unit(0.4, "cm")), size = 1.0, colour = "grey50") +
  annotate("text", label = "Agulhas", x = 31.6, y = -31.6, size = 3.0, angle = 53) +
  # Landmass
  annotate("text", label = "South\nAfrica", x = 24.00, y = -31.00, size = 7, angle = 0) +
  geom_point(data = sites, aes(lon, lat, shape = type), colour = "black", size = 3.5) +
  geom_point(data = sites, aes(lon, lat, shape = type), colour = "white", size = 2.0) +
  # scale_colour_grey(breaks = c("new", "old", "thermo"),
  #                   label = c("new", "old", "thermo")) +
  guides(shape = guide_legend("Type", override.aes = list(size = 2.5, colour = "black"))) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme(legend.key = element_blank())
map

##########################################################################
### 7. Create figure01

## Combine the two maps to create one inset map
pdf("graph/figure01.pdf", width = 8, height = 4, pointsize = 6) # Set PDF dimensions
vp1 <- viewport(x = -0.01, y = 0.05, w = 0.25, h = 0.25, just = c("left", "bottom")) # Africa
vp2 <- viewport(x = 1.0, y = 1.0, w = 1.00, h = 1.00, just = c("right", "top"))  # South Africa
print(map, vp = vp2)
print(africa, vp = vp1)
dev.off()

##########################################################################
### 8. Load analysis results

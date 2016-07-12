###########################################################################
### "figures.R"
## This script shows the code used to generate the figures for the paper
# 1. Load all packages required for the analyses
# 2. Load all functions etc. found in other scripts
# 3. Load the site list and spatial data used for the map
# 4. Create the map of Africa to be windowed
# 5. Create the map of southern Africa
# 6. Add site list information
# 7. Create figure01
# 8. Load analysis results
# 9. 
#############################################################################

#############################################################################
### 1. Load all packages required for the analyses
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(tibble)
library(doMC); doMC::registerDoMC(cores = 4)

#############################################################################
### 2. Load all functions etc. found in other scripts
source("func/scaleBarFunc.R") # A custom ggplot function that creates a very snazy scale bar
source("setupParams/themes.R") # The ggplot theme used for all figures

#############################################################################
### 3. Load the site list and spatial data used for the map

## The site data
load("data/SACTN_sub2.Rdata")
## The site list
sites <- read.csv("setupParams/site_list_v4.0.csv")
sites$index <- as.factor(paste(sites$site, sites$src, sep = "/ "))
# Subset to the 84 time series used
sites_sub <- sites[sites$index %in% levels(SACTN_sub2$index),]

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
  # The unused stations
  geom_point(data = sites, aes(lon, lat), colour = "black", size = 3.5, alpha = 0.3) +
  geom_point(data = sites, aes(lon, lat), colour = "white", size = 2.0, alpha = 0.3) +
  # The used stations
  geom_point(data = sites_sub, aes(lon, lat), colour = "black", size = 3.5) +
  geom_point(data = sites_sub, aes(lon, lat), colour = "white", size = 2.0) +
  # scale_colour_grey(breaks = c("new", "old", "thermo"),
  #                   label = c("new", "old", "thermo")) +
  # guides(shape = guide_legend("Type", override.aes = list(size = 2.5, colour = "black"))) +
  labs(title = NULL, x = NULL, y = NULL) #+
  # theme(legend.key = element_blank())
map

##########################################################################
### 7. Create figure01

## Combine the two maps to create one inset map
pdf("graph/SA_sites.pdf", width = 8, height = 5, pointsize = 6) # Set PDF dimensions
vp1 <- viewport(x = -0.00, y = 0.05, w = 0.25, h = 0.25, just = c("left", "bottom")) # Africa
vp2 <- viewport(x = 1.0, y = 1.0, w = 1.00, h = 1.00, just = c("right", "top"))  # South Africa
print(map, vp = vp2)
print(africa, vp = vp1)
dev.off()

##########################################################################
### 8. Load analysis results
load(file = "data/gls_fitted_full_nointerp_natural.RData") # for non-interpolated, full, natural
gls_nat <- as_tibble(gls_df)
glimpse(gls_nat)
rm(gls_df)
# the interpolated, grown data need to be replaced with non-interpolated, grown data...
load(file = "data/gls_fitted_full_nointerp_grown.RData") # for interpolated, full, grown
gls_gro <- as_tibble(gls_df)
glimpse(gls_gro)
rm(gls_df)
load("data/SACTN_flat_interp.Rdata")
SACTN_flat <- as_tibble(SACTN_flat)
SACTN_flat

# boxplots ----------------------------------------------------------------
SACTN_flat %>% # remains unchanged
  ggplot(aes(x = index, y = temp, group = index)) +
  geom_hline(yintercept = 0, size = 0.4, col = "red") +
  geom_boxplot(size = 0.3, outlier.size = 0.5, show.legend = FALSE,
               outlier.shape = 21, notch = TRUE, fill = "grey80", varwidth = TRUE) +
  scale_x_discrete(name = "Time series no.", labels = 1:length(levels(SACTN_flat$index))) +
  scale_y_continuous(name = expression(paste("Detrended temperature anomaly (", degree, "C)"))) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 8))
ggsave("graph/all_plt1.pdf", plot = last_plot(), width = 8.0, height = 3.25, units = "in")

# data prep for correlation -----------------------------------------------
dat_w <- gls_nat %>%
  unite(fac, site, src, remove = TRUE) %>%
  select(fac, DT, length, DT_model, prec)
x1 <- filter(dat_w, prec == "prec0001")
x2 <- filter(dat_w, prec == "prec001")
x3 <- filter(dat_w, prec == "prec01")
x4 <- filter(dat_w, prec == "prec05")
length(x1) == length(x2) # they are all of the same length; proceed...

# t-tests and correlations ------------------------------------------------
t.test(x1$DT_model, x2$DT_model, paired = TRUE) # different?!
t.test(x1$DT_model, x3$DT_model, paired = TRUE) # not different!!
t.test(x1$DT_model, x4$DT_model, paired = TRUE) # different!
cor1 <- cor.test(x = x1$DT_model, y = x2$DT_model)
cor2 <- cor.test(x = x1$DT_model, y = x4$DT_model)

# correlation plots -------------------------------------------------------
pdf(file = "graph/correlations_new.pdf", width = 6, height = 3)
par(mfrow=c(1, 2))
plot(x1$DT_model, x2$DT_model, pch = ".", col = "red", type = "p",
     xlab = "Precision: 0.001", ylab = "Precision: 0.01")
plot(x1$DT_model, x4$DT_model, pch = ".", col = "red", type = "p",
     xlab = "Precision: 0.001", ylab = "Precision: 0.5")
par(mfrow=c(1, 1))
dev.off()

# RMSE --------------------------------------------------------------------
# where vec1 is the reference; vec1 and vec2 of equal length
rmse <- function(vec1, vec2) {
  sqrt(mean((vec1 - vec2)^2))
}
# assume pred0001 is best, i.e. reference, then...
rmse(x1$DT_model, x2$DT_model) # smaller is better
rmse(x1$DT_model, x3$DT_model)
rmse(x1$DT_model, x4$DT_model)

# data prep for plotting --------------------------------------------------
dat <- gls_nat %>%
  filter(prec == "prec001") %>%
  select(site, src, DT, DT_model, se_trend, sd_initial, sd_residual,
         p_trend, length) %>%
  unite(fac, site, src, remove = FALSE)
dat$DT[dat$DT == "DT000"] <- 0
dat$DT[dat$DT == "DT005"] <- 0.05
dat$DT[dat$DT == "DT010"] <- 0.10
dat$DT[dat$DT == "DT015"] <- 0.15
dat$DT[dat$DT == "DT020"] <- 0.20
dat$DT <- as.numeric(dat$DT)

dat_gro <- gls_gro %>%
  select(site, src, DT, DT_model, se_trend, sd_initial, sd_residual,
         p_trend, length, year_index) %>%
  unite(fac, site, src, remove = FALSE)
dat_gro$DT[dat_gro$DT == "DT000"] <- 0
dat_gro$DT[dat_gro$DT == "DT005"] <- 0.05
dat_gro$DT[dat_gro$DT == "DT010"] <- 0.10
dat_gro$DT[dat_gro$DT == "DT015"] <- 0.15
dat_gro$DT[dat_gro$DT == "DT020"] <- 0.20
dat_gro$DT <- as.numeric(dat_gro$DT)

# other questions ---------------------------------------------------------
# the relationship between precision and regression (slope) SE?
# the relationship between sd_initial and regression (slope) SE?

dat %>%
  ggplot(aes(x = DT, y = DT_model, group = as.factor(DT))) +
  geom_jitter(aes(col = sd_initial), show.legend = TRUE, width = 0.015, shape = 1) +
  geom_boxplot(fill = "grey20", alpha = 0.35, outlier.colour = NA, size = 0.3) +
  scale_x_continuous(name = expression(paste("Actual trend (", degree, "C/dec)"))) +
  scale_y_continuous(name = expression(paste("Model trend (", degree, "C/dec)")),
                     breaks = c(-0.2, 0, 0.05, 0.1, 0.15, 0.2, 0.4)) +
  scale_colour_distiller(name = "Initial SD", palette = "Spectral") +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5))
ggsave("graph/all_plt0_no_interp_natural.pdf", plot = last_plot(), width = 7, height = 4.5, units = "in")

dat %>%
  ggplot(aes(x = DT, y = DT_model, group = as.factor(DT))) +
  geom_jitter(aes(col = length), show.legend = TRUE, width = 0.015, shape = 1) +
  geom_boxplot(fill = "grey20", alpha = 0.35, outlier.colour = NA, size = 0.3) +
  scale_x_continuous(name = expression(paste("Actual trend (", degree, "C/dec)"))) +
  scale_y_continuous(name = expression(paste("Model trend (", degree, "C/dec)")),
                     breaks = c(-0.2, 0, 0.05, 0.1, 0.15, 0.2, 0.4)) +
  scale_colour_distiller(name = "Time series length (months)", palette = "Spectral") +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5))
ggsave("graph/all_plt1_no_interp_natural.pdf", plot = last_plot(), width = 7, height = 4.5, units = "in")

# plotting modelled trend vs. length (natural, no-interp) -----------------
dat %>%
  ggplot(aes(x = length, y = DT_model)) +
  geom_line(col = "black", show.legend = TRUE) +
  scale_x_continuous(name = "Time series length (months)") +
  scale_y_continuous(name = expression(paste("Model trend (", degree, "C)")),
                     limits = c(-0.5, 0.5)) +
  facet_wrap("DT", ncol = 5) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5))
ggsave("graph/all_plt2_no_interp_natural.pdf", plot = last_plot(), width = 8, height = 2, units = "in")

# plotting modelled trend vs. length (grown, no-interp) -------------------
dat_gro %>%
  ggplot(aes(x = year_index, y = DT_model, group = fac)) +
  geom_line(aes(col = se_trend), show.legend = TRUE, alpha = 0.85, size = 0.3) +
  scale_x_continuous(name = "Time series length (years)") +
  scale_y_continuous(name = expression(paste("Model trend (", degree, "C)")),
                     limits = c(-2, 2)) +
  scale_colour_distiller(name = "SE of trend", palette = "Spectral") +
  facet_wrap("DT", ncol = 5) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5),
        legend.position = "right",
        legend.direction = "vertical")
ggsave("graph/all_plt2_no_interp_gro.pdf", plot = last_plot(), width = 8, height = 2, units = "in")

# plotting p-value vs. SD (initial) (natural, no-interp) ------------------
dat %>%
  ggplot(aes(x = sd_initial, y = p_trend)) +
  geom_hline(yintercept = 0.05, col = "red") +
  geom_point(aes(size = length), col = "black", shape = 21, stroke = 0.2) +
  scale_y_continuous(name = "p-value", limits = c(0, 1)) +
  scale_x_continuous(name = expression(paste("Initial SD (", degree, "C)"))) +
  scale_size_continuous(name = "Length (months)") +
  facet_wrap("DT", ncol = 1)
ggsave("graph/all_plt4_no_interp_natural.pdf", plot = last_plot(), width = 5, height = 7,
       units = "in")

# plotting p-value vs. SD (initial) (grown, no-interp) --------------------
dat_gro %>%
  ggplot(aes(x = sd_initial, y = p_trend)) +
  geom_hline(yintercept = 0.05, col = "red") +
  geom_point(aes(size = length), col = "black", shape = 21, stroke = 0.2) +
  scale_y_continuous(name = "p-value", limits = c(0, 1)) +
  scale_x_continuous(name = expression(paste("Initial SD (", degree, "C)"))) +
  scale_size_continuous(name = "Length (months)") +
  facet_wrap("DT", ncol = 1)
ggsave("graph/all_plt4_no_interp_gro.pdf", plot = last_plot(), width = 5, height = 7,
       units = "in")

# plotting DT/DT_modeled vs. length (natural, no-interp) ------------------
dat %>%
  ggplot(aes(x = length, y = abs(DT/DT_model))) +
  geom_point(aes(size = se_trend/20, alpha = ((1/se_trend) * 2)), col = "black",
             shape = 21, show.legend = TRUE) +
  scale_x_continuous(name = "Time series length (months)") +
  scale_y_continuous(name = "Actual trend / Model trend", limits = c(-0.1, 1)) +
  scale_alpha_continuous(guide = FALSE) +
  scale_size_continuous(name = "SE of trend") +
  facet_wrap("DT", ncol = 5)
ggsave("graph/all_plt6_no_interp_natural.pdf", plot = last_plot(), width = 8, height = 2.45, units = "in")

# plotting DT/DT_modeled vs. length (grown, no-interp) --------------------
dat_gro %>%
  ggplot(aes(x = year_index, y = abs(DT/DT_model))) +
  geom_point(aes(size = se_trend/20, alpha = ((1/se_trend) * 2)), col = "black",
             shape = 21, show.legend = TRUE) +
  scale_x_continuous(name = "Time series length (months)") +
  scale_y_continuous(name = "Actual trend / Model trend", limits = c(-0.1, 1)) +
  scale_alpha_continuous(guide = FALSE) +
  scale_size_continuous(name = "SE of trend") +
  facet_wrap("DT", ncol = 5)
ggsave("graph/all_plt6_no_interp_gro.pdf", plot = last_plot(), width = 8, height = 2.45, units = "in")

# plotting ts length vs. se_trend (natural, no-interp) --------------------
dat %>%
  ggplot(aes(x = length, y = se_trend)) +
  geom_line(col = "black", show.legend = TRUE) +
  scale_x_continuous(name = "Time series length (months)") +
  scale_y_continuous(name = "SE of trend") +
  facet_wrap("DT", ncol = 5) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5))
ggsave("graph/all_plt7_no_interp_natural.pdf", plot = last_plot(), width = 8, height = 2, units = "in")

# plotting ts length vs. se_trend (grown, no-interp) ----------------------
dat_gro %>%
  ggplot(aes(x = year_index, y = se_trend, group = fac)) +
  geom_line(aes(col = sd_initial), alpha = 0.35, show.legend = TRUE) +
  scale_x_continuous(name = "Time series length (years)") +
  scale_y_continuous(name = "SE of trend") +
  scale_colour_distiller(name = expression(paste("Initial SD (", degree, "C)")),
                         palette = "Spectral") +
  facet_wrap("DT", ncol = 1) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5))
ggsave("graph/all_plt7_no_interp_grown.pdf", plot = last_plot(), width = 4, height = 7, units = "in")

#############################################################################
### 11. Relationship between NA% and significance of detected trend

## Load the data that has percent NA and type columns to add them to the GLS data frames
load("data/SACTN_sub2.Rdata")
colnames(SACTN_sub2)[8] <- "na_perc"
SACTN_sub2 <- as.data.frame(SACTN_sub2)

## Load the modelled data and add the index and type columns
# The interpolated data
load("data/gls_fitted_full_interp_grown.RData")
gls_df_interp <- gls_df
gls_df_interp$index <- as.factor(paste(gls_df_interp$site, gls_df_interp$src, sep = "/ "))
# gls_df_interp$type <- NA
# gls_df_interp$type[gls_df_interp$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "thermo"]))] <- "thermo"
# gls_df_interp$type[gls_df_interp$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "old"]))] <- "old"
# gls_df_interp$type[gls_df_interp$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "new"]))] <- "new"
# The non-interpolated data
load("data/gls_fitted_full_nointerp_grown.RData")
gls_df_non <- gls_df; rm(gls_df)

## Subset gls_fitted_full_interp_grown results by max(year_index) and Prec0.001 # Or just use a "natural" data frame
# The interpolated data
gls_df_interp <- gls_df_interp %>%
  group_by(site, src) %>%
  # filter(DT == "DT020") %>%
  filter(prec == "prec0001") #%>%
  filter(year_index == max(year_index))

# The non-interpolated data
gls_df_non <- gls_df_non %>%
  group_by(site, src) %>%
  # filter(DT == "DT020") %>%
  filter(year_index == max(year_index))

## Add NA% from data_summary2 # The warning issues persist... :(
# The interpolated data
# gls_df_interp <- gls_df_interp %>%
#   group_by(index) %>%
#   mutate(interp_perc = SACTN_sub2$na_perc[SACTN_sub2$index == index][1])
# gls_df_interp <- data.frame(gls_df_interp)

# The non-interpolated data
gls_df_non <- gls_df_non %>%
  group_by(index) %>%
  mutate(na_perc = SACTN_sub2$na_perc[SACTN_sub2$index == index][1])
gls_df_non <- data.frame(gls_df_non)

## "Grow" the limit of NA/ Interp used on the data 
    # I have since realised that these differences could be calculated easily without for loops...

# Set limits for missing data
miss_limit <- c(1, 2.5, 5, 7.5, 10, 12.5, 15)

# Grow the interpolated data
# gls_df_interp_grow <- data.frame()
# for(i in 1:length(miss_limit)){
#   data1 <- data.frame(gls_df_interp[gls_df_interp$interp_perc <= miss_limit[i],])
#   data1$miss_limit <- miss_limit[i]
#   gls_df_interp_grow <- rbind(gls_df_interp_grow, data1)
# }; rm(data1)

# Grow the non-interpolated data
gls_df_non_grow <- data.frame()
for(i in 1:length(miss_limit)) {
  data1 <- data.frame(gls_df_non[gls_df_non$na_perc <= miss_limit[i],])
  data1$miss_limit <- miss_limit[i]
  gls_df_non_grow <- rbind(gls_df_non_grow, data1)
}; rm(data1)

## Show relation between p_trend and missing values
# # The interpolated data
# ggplot(data = gls_df_interp_grow, aes(x = interp_perc, y = p_trend)) +
#   geom_point(aes(colour = as.factor(DT))) +
#   # geom_smooth(aes(colour = as.factor(DT)), method = "glm", alpha = 0.2) + 
#   geom_smooth(aes(colour = as.factor(DT)), method = "glm", method.args = list(family = "poisson")) +
#   facet_wrap(~miss_limit, scales = "free_x")
# ggsave("interp.pdf", height = 8, width = 10)

# The non-interpolated data
head(gls_df_non_grow)
gls_df_non_grow$DT[gls_df_non_grow$DT == "DT000"] <- 0
gls_df_non_grow$DT[gls_df_non_grow$DT == "DT005"] <- 0.05
gls_df_non_grow$DT[gls_df_non_grow$DT == "DT010"] <- 0.10
gls_df_non_grow$DT[gls_df_non_grow$DT == "DT015"] <- 0.15
gls_df_non_grow$DT[gls_df_non_grow$DT == "DT020"] <- 0.20
gls_df_non_grow$DT <- as.numeric(gls_df_non_grow$DT)

library(fitdistrplus)
descdist(gls_df_non_grow$na_perc, boot = 500, graph = TRUE)
plot(hist(gls_df_non_grow$na_perc))
plot(hist(log(gls_df_non_grow$na_perc)))

gls_df_non_grow %>% 
  filter(miss_limit != 7.5) %>% 
  ggplot(aes(x = log(na_perc), y = p_trend)) +
  geom_point(aes(shape = as.factor(DT)), col = "black", stroke = 0.3) +
  geom_smooth(aes(fill = as.factor(DT)), method = "lm", size = 0.3, col = "black") +
  # geom_smooth(aes(colour = as.factor(DT)), method = "glm", method.args = list(family = "poisson")) +
  scale_x_continuous(name = "Log % NA") +
  scale_y_continuous(name = "p-value") +
  scale_fill_discrete(name = expression(paste("Trend (", degree, "C/dec)"))) +
  scale_shape_discrete(name = expression(paste("Trend (", degree, "C/dec)"))) +
  facet_wrap(~miss_limit, scales = "free_x") 
ggsave("graph/non_NA_perc.pdf", height = 6, width = 10)

## Calculate correlation summary between p_trend and missing values
# The interpolated data
# gls_df_interp_stats <- gls_df_interp_grow %>%
#   group_by(DT, miss_limit) %>%
#   mutate(p_mean = mean(p_trend)) %>%
#   mutate(r_miss_p = cor(p_trend, interp_perc)) %>%
#   mutate(p_miss_p = as.numeric(cor.test(p_trend, interp_perc)[3]))
# gls_df_interp_stats <- gls_df_interp_stats[c(3,19:22)]
# gls_df_interp_stats <- gls_df_interp_stats %>%
#   unique() %>%
#   mutate(method = "interp")
# The non-interpolated data
gls_df_non_stats <- gls_df_non_grow %>%
  group_by(DT, miss_limit) %>%
  mutate(p_mean = mean(p_trend)) %>%
  mutate(r_miss_p = cor(p_trend, na_perc)) %>%
  mutate(p_miss_p = as.numeric(cor.test(p_trend, na_perc)[3]))
gls_df_non_stats <- gls_df_non_stats[c(3,18:21)]
gls_df_non_stats <- gls_df_non_stats %>%
  unique() %>%
  mutate(method = "non-interp")
# Combine and Melt for plotting
gls_df_all_stats <- rbind(gls_df_interp_stats, gls_df_non_stats)
gls_df_all_stats_long <- melt(gls_df_all_stats, id = c("method", "DT", "miss_limit"))
# xtable::xtable(gls_df_interp_stats)

## Compare the two sets of stats visually
ggplot(data = gls_df_all_stats_long, aes(x = miss_limit, y = value)) +# bw_update +
  geom_point(aes(colour = DT, linetype = method)) +
  geom_line(aes(colour = DT, linetype = method)) +
  facet_wrap(~variable, ncol = 1, scales = "free_y")
ggsave("interp_vs_non_stats.pdf", height = 6, width = 10)
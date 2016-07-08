library(doMC) # multi-core
require(mgcv)
library(plyr)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)

# set-up and load data ----------------------------------------------------
doMC::registerDoMC(cores = 4)

# Select one of 1., 2. or 3.:
# load("data/SACTN_full_interp.Rdata") # 1. interpolated, full, grown 
# mod <- as_tibble(ISfull) %>%
# load(file = "data/SACTN_full_natural_no_interp.Rdata") # 2. not interpolated, full, natural
# mod <- as_tibble(SACTN_full_natural_no_interp)
load(file = "data/SACTN_full_grown_no_interp.Rdata") # 3. not interpolated, full, grown
mod <- as_tibble(SACTN_full_grown_no_interp)

# Use below for grown data only (not interested in precision effects)
mod %>%
  filter(prec == "prec0001")

rm(SACTN_full_grown_no_interp)

# tDat <- SACTN_full_natural %>%
#   filter(prec == "prec0001") %>%
#   filter(site == "Lamberts Bay") %>%
#   filter(DT == "DT020")
# rm(SACTN_full_natural)

# the GLS-AR2 -------------------------------------------------------------
gls_fun <- function(df) {
  out <- tryCatch({
    model <- gls(
      temp ~ num,
      correlation = corARMA(form = ~ 1 | year, p = 2),
      method = "REML",
      data = df, na.action = na.exclude
    )
    stats <-
      data.frame(
        DT_model = round(as.numeric(coef(model)[2]) * 120, 3),
        se_trend = round(summary(model)[["tTable"]][[2, 2]], 10),
        sd_initial = round(sd(df$temp, na.rm = TRUE), 2),
        sd_residual = round(sd(model$residuals), 2),
        r = NA,
        R2 = NA,
        p_trend = summary(model)[["tTable"]][[2, 4]],
        p_seas = NA,
        length = length(df$temp),
        model = "gls"
      )
    return(stats)
  },
  error = function(cond) {
    stats <- data.frame(
      DT_model = NA,
      se_trend = NA,
      sd_initial = round(sd(df$temp, na.rm = TRUE), 2),
      sd_residual = NA,
      r = NA,
      R2 = NA,
      p_trend = NA,
      p_seas = NA,
      length = length(df$temp),
      model = "gls"
    )
    return(stats)
  })
  rm(model)
  return(out)
}

# add year_index and remove prec if grown data are used
system.time(mod_gls <- dlply(mod, .(site, src, DT, year_index), .progress = "text",
                             .parallel = TRUE, gls_fun))
# timing
# Progress disabled when using parallel plyr
# user   system  elapsed
# 1566.123  346.321  644.384
gls_df <- ldply(mod_gls, data.frame, .progress = "text")

## Add "type" column
# Load the data that has percent NA and type columns to add them to the GLS data frame
load("data/SACTN_sub2.Rdata")
colnames(SACTN_sub2)[8] <- "na_perc"
SACTN_sub2 <- as.data.frame(SACTN_sub2)
# Add the type column
gls_df$index <- as.factor(paste(gls_df$site, gls_df$src, sep = "/ "))
gls_df$type <- NA
gls_df$type[gls_df$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "thermo"]))] <- "thermo"
gls_df$type[gls_df$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "old"]))] <- "old"
gls_df$type[gls_df$index %in% levels(droplevels(SACTN_sub2$index[SACTN_sub2$type == "new"]))] <- "new"

ht(gls_df)
save(gls_df, file = "data/gls_fitted_full_nointerp_grown.RData")


# the linear GAMM ---------------------------------------------------------
gamm_lin_fun <- function(df) {
  out <- tryCatch({
    model <- gamm(
      temp ~ s(month, bs = "cc", k = 12) + num,
      correlation = corARMA(form = ~ 1 | year, p = 2),
      method = "REML",
      data = df, na.action = na.omit
    )
    stats <-
      data.frame(
        DT_model = round(as.numeric(summary(model$gam)$p.coeff[2]) * 120, 3),
        se_trend = round(as.numeric(summary(model$gam)$se[2]), 10),
        sd_initial = round(sd(df$temp, na.rm = TRUE), 2),
        sd_residual = round(sd(model$gam$residuals), 2),
        r = round(sqrt(summary(model$gam)$r.sq), 2),
        R2 = round(summary(model$gam)$r.sq, 2),
        p_trend = as.numeric(summary(model$gam)$p.pv[2]),
        p_seas = as.numeric(summary(model$gam)$s.pv),
        length = length(df$temp),
        model = "gamm_lin"
      )
    return(stats)
  },
  error = function(cond) {
    stats <- data.frame(
      DT_model = NA,
      se_trend = NA,
      sd_initial = round(sd(df$temp, na.rm = TRUE), 2),
      sd_residual = NA,
      r = NA,
      R2 = NA,
      p_trend = NA,
      p_seas = NA,
      length = length(df$temp),
      model = "gamm_lin"
    )
    return(stats)
  })
  rm(model)
  return(out)
}

# add year_index if grown
system.time(mod_gamm_lin <- dlply(mod, .(site, src, DT, prec), .progress = "text",
                                  .parallel = TRUE, gamm_lin_fun))
# timing
#     user   system  elapsed
# 4009.411  343.352 4358.262
gamm_lin_df <- ldply(mod_gamm_lin, data.frame, .progress = "text")
ht(gamm_lin_df)
save(gamm_lin_df, file = "data/gamm_lin_fitted_full_nointerp_grown.RData")


# the non-linear GAMM -----------------------------------------------------


gamm_non_fun <- function(df) {
  out <- tryCatch({
    model <-
      gamm(
        temp ~ s(month, bs = "cc", k = 6) + s(num, bs = "cr"),
        correlation = corARMA(form = ~ 1 | year, p = 2),
        method = "REML",
        data = df, na.action = na.omit
      )
    stats <- data.frame(
      DT_model = NA,
      se_trend = NA,
      sd_initial = round(sd(df$temp, na.rm = TRUE), 2),
      sd_residual = round(sd(model$gam$residuals), 2),
      r = round(sqrt(summary(model$gam)$r.sq), 2),
      R2 = round(summary(model$gam)$r.sq, 2),
      p_trend = as.numeric(summary(model$gam)$p.pv[2]),
      p_seas = as.numeric(summary(model$gam)$s.pv),
      length = length(df$temp),
      model = "gamm_non"
    )
    return(stats)
  },
  error = function(cond) {
    stats <- data.frame(
      DT_model = NA,
      se_trend = NA,
      sd_initial = round(sd(df$temp, na.rm = TRUE), 2),
      sd_residual = NA,
      r = NA,
      R2 = NA,
      p_trend = NA,
      p_seas = NA,
      length = length(df$temp),
      model = "gamm_non"
    )
    return(stats)
  })
  rm(model)
  return(out)
}

# add year_index if grown
system.time(mod_gamm_non <- dlply(mod, .(site, src, DT, prec), .progress = "text",
                                  .parallel = TRUE, gamm_non_fun))
gamm_non_df <- ldply(mod_gamm_non, data.frame, .progress = "text")
ht(gamm_non_df)
save(gamm_non_df, file = "data/gamm_non_fitted_full_nointerp_grown.RData")

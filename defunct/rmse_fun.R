rmse <- function(vec1, vec2) {
  sqrt(mean((vec1 - vec2)^2))
}

load("data/SACTN_full_natural_no_interp.Rdata")
SACTN <- SACTN_full_natural_no_interp %>% 
  filter(prec == "prec0001")

rmse_fun <- function(df) {
  a = as.vector(predict(gls(temp ~ num,
                            correlation = corARMA(form = ~ 1 | year, p = 2),
                            method = "REML", data = df, na.action = na.exclude)))
  b = df$temp
  rmse(b, a)
}

rmse_df <- ddply(SACTN, .(site, src, index, DT), .progress = "text", .parallel = FALSE, rmse_fun)

cor_fun <- function(df) {
  a = as.vector(predict(gls(temp ~ num,
                            correlation = corARMA(form = ~ 1 | year, p = 2),
                            method = "REML", data = df, na.action = na.exclude)))
  b = df$temp
  round(cor(b, a), 4)
}

cor_df <- ddply(SACTN, .(site, src, index, DT), .progress = "text", .parallel = TRUE, cor_fun)
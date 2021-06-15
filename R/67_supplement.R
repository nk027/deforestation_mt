
# Dependencies ------------------------------------------------------------

stopifnot(
  file.exists("data/data.rds"),
  require("Matrix"),
  require("sf"),
  require("dplyr"),
  require("spatialreg"),
  require("coda")
)

source("R/51_supp.R")
source("R/52_helpers.R")

data <- readRDS("data/data.rds")

# Analyse -----------------------------------------------------------------

# Add plot for the main table -----

load("data/est_extended_qu.rda")

x_sm <- rbind(summary(out_sdm, out_sdm$var_names, 0.95), 
  c(mean(out_sdm$rho), HPDinterval(mcmc(out_sdm$rho), .95), 0)[c(2, 1, 3, 4)])
df <- cbind(out_sdm$beta[, !grepl("[ti]fe$", colnames(out_sdm$beta))], out_sdm$rho)
colnames(df) <- c("Intercept", 
  paste0(c("Forest share", "Pasture share", "Cropland share", "Cattle density", 
    "Soy yield", "Population density", "Dry month"), ", direct"),
  paste0(c("Forest share", "Pasture share", "Cropland share", "Cattle density", 
    "Soy yield", "Population density", "Dry month"), ", indirect"), "Rho")

pdf("outputs/parameter_density.pdf", 8, 6)
png("outputs/parameter_density.png", 1600, 1200, pointsize = 24)
op <- par(mfrow = c(4, 4), mar = c(2, 2, 2, 0.5), bg = "transparent")
for(i in seq(nrow(x_sm))) {
  densplot(mcmc(df[, i]), main = colnames(df)[i])
  abline(v = 0, col = "#800080")
  abline(v = x_sm[i, c(1, 3)], col = "darkgray", lty = 3)
  abline(v = x_sm[i, 2], col = "#008080", lty = 2)
}
par(op)
dev.off()

# Build classic regression Table -----

load("data/est_extended_qu.rda")

rmse_sdm <- rmse(out_sdm)
rmse_sar <- rmse(out_sar)
rmse_slx <- rmse(out_slx)
rmse_clm <- rmse(out_clm)

tbl1 <- rbind(
  "rho" = c(
    mean(out_sdm$rho), HPDinterval(mcmc(out_sdm$rho)),
    mean(out_sar$rho), HPDinterval(mcmc(out_sar$rho)),
    0, 0, 0, 0, 0, 0
  ),
  "RMSE" = c(
    mean(rmse_sdm), HPDinterval(mcmc(rmse_sdm)),
    mean(rmse_sar), HPDinterval(mcmc(rmse_sar)),
    mean(rmse_slx), HPDinterval(mcmc(rmse_slx)),
    mean(rmse_clm), HPDinterval(mcmc(rmse_clm))
  ),
  "BIC" = c(
    BIC(out_sdm), 0, 0, BIC(out_sar), 0, 0,
    BIC(out_slx), 0, 0, BIC(out_clm), 0, 0))
tbl1 <- rbind(cbind(
  summary(out_sdm)[, c(2, 1, 3)], 
  rbind(summary(out_sar)[, c(2, 1, 3)], matrix(0, 7, 3)), 
  summary(out_slx)[, c(2, 1, 3)],
  rbind(summary(out_clm)[, c(2, 1, 3)], matrix(0, 7, 3))),
  tbl1)
row.names(tbl1)[1:15] <- c("Intercept", 
  c("Forest share", "Pasture share", "Cropland share", "Cattle density", 
    "Soy yield", "Population density", "Dry month"),
  paste0(c("Forest share", "Pasture share", "Cropland share", "Cattle density", 
  "Soy yield", "Population density", "Dry month")))
tbl1 <- round(tbl1, 3)
tbl1[tbl1 == 0] <- ""
tbl1[1:15, colnames(tbl1) == "low"] <- paste0("(", tbl1[1:15, colnames(tbl1) == "low"])
tbl1[1:15, colnames(tbl1) == "up"] <- paste0(tbl1[1:15, colnames(tbl1) == "up"], ")")
knitr::kable(tbl1, format = "latex", booktabs = TRUE)

# Add direct and indirect effects for Table 2 -----

# Biome split ---
load("data/est_extended_qu_biome.rda")
eff_sdm <- readRDS("outputs/est_extended_qu_biome.rds")
eff_sdm$total <- eff_sdm$direct + eff_sdm$indirect

# Posterior summary
# tbl2a_tot <- apply(eff_sdm$total, 2, function(x) {
#   c(mean(x), HPDinterval(mcmc(x)))
# })
tbl2a_dir <- apply(eff_sdm$direct, 2, function(x) {
  c(mean(x), HPDinterval(mcmc(x)))
})
tbl2a_ind <- apply(eff_sdm$indirect, 2, function(x) {
  c(mean(x), HPDinterval(mcmc(x)))
})

tbl2a <- data.frame(direct = t(tbl2a_dir), indirect = t(tbl2a_ind),
  row.names = c("c", out_sdm$meta$var_names))
# Subset
tbl2a <- tbl2a[c(paste0(c("crop_px_km2_lag", "soy_filled_lag", 
  "pasture_px_km2_lag", "cattle_dens_lag_log"), "_a"), 
  paste0(c("crop_px_km2_lag", "soy_filled_lag", 
    "pasture_px_km2_lag", "cattle_dens_lag_log"), "_b")), ]
# Output
colnames(tbl2a) <- c("mean_a", "low_a", "high_a", 
  "mean_b", "low_b", "high_b")
row.names(tbl2a) <- c("Croplands", "Soy yields", "Pasture", "Cattle",
  paste0(c("Croplands", "Soy yields", "Pasture", "Cattle"), " "))
tbl2a <- round(tbl2a, 3)
tbl2a$low_a <- paste0("(", tbl2a$low_a)
tbl2a$high_a <- paste0(tbl2a$high_a, ")")
tbl2a$low_b <- paste0("(", tbl2a$low_b)
tbl2a$high_b <- paste0(tbl2a$high_b, ")")
knitr::kable(tbl2a, "latex", booktabs = TRUE)

# Date split ---
load("data/est_extended_qu_time.rda")
eff_sdm <- readRDS("outputs/est_extended_qu_time.rds")
eff_sdm$total <- eff_sdm$direct + eff_sdm$indirect

# Posterior summary
# tbl2a_tot <- apply(eff_sdm$total, 2, function(x) {
#   c(mean(x), HPDinterval(mcmc(x)))
# })
tbl2b_dir <- apply(eff_sdm$direct, 2, function(x) {
  c(mean(x), HPDinterval(mcmc(x)))
})
tbl2b_ind <- apply(eff_sdm$indirect, 2, function(x) {
  c(mean(x), HPDinterval(mcmc(x)))
})

tbl2b <- data.frame(direct = t(tbl2b_dir), indirect = t(tbl2b_ind),
  row.names = c("c", out_sdm$meta$var_names))
# Subset
tbl2b <- tbl2b[c(paste0(c("crop_px_km2_lag", "soy_filled_lag", 
  "pasture_px_km2_lag", "cattle_dens_lag_log"), "_pre"), 
  paste0(c("crop_px_km2_lag", "soy_filled_lag", 
    "pasture_px_km2_lag", "cattle_dens_lag_log"), "_post")), ]
# Output
colnames(tbl2b) <- c("mean_a", "low_a", "high_a", 
  "mean_b", "low_b", "high_b")
row.names(tbl2b) <- c("Croplands", "Soy yields", "Pasture", "Cattle",
  paste0(c("Croplands", "Soy yields", "Pasture", "Cattle"), " "))
tbl2b <- round(tbl2b, 3)
tbl2b$low_a <- paste0("(", tbl2b$low_a)
tbl2b$high_a <- paste0(tbl2b$high_a, ")")
tbl2b$low_b <- paste0("(", tbl2b$low_b)
tbl2b$high_b <- paste0(tbl2b$high_b, ")")
knitr::kable(tbl2b, "latex", booktabs = TRUE)




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

# Compute effects -----

paths <- paste0("data/est_extended_qu", 
  c("", "_biome", "_time", "_biome_time"), ".rda")
p <- paths[1]
for(p in paths) {
  models <- load(p)
  plot(out_sdm)
  eff_sdm <- effects(out_sdm, n_draw = 10000)
  saveRDS(eff_sdm, paste0("outputs/", gsub(".*/(.*).rda", "\\1.rds", p)))
}


# Create Table 1 -----

load("data/est_extended_qu.rda")
eff_sdm <- readRDS("outputs/est_extended_qu.rds")
eff_sdm$total <- eff_sdm$direct + eff_sdm$indirect

# Posterior summary
tbl1_dir <- apply(eff_sdm$direct, 2, function(x) {
  c(mean(x), HPDinterval(mcmc(x)))
})
tbl1_ind <- apply(eff_sdm$indirect, 2, function(x) {
  c(mean(x), HPDinterval(mcmc(x)))
})

tbl1 <- data.frame(direct = t(tbl1_dir), indirect = t(tbl1_ind), 
  row.names = c("c", out_sdm$meta$var_names))
# Subset
tbl1 <- tbl1[c("crop_px_km2_lag", "soy_filled_lag", 
  "pasture_px_km2_lag", "cattle_dens_lag_log"), ]
# Output
colnames(tbl1) <- c("mean_a", "low_a", "high_a", 
  "mean_b", "low_b", "high_b")
row.names(tbl1) <- c("Croplands", "Soy yields", "Pasture", "Cattle")
tbl1 <- round(tbl1, 3)
tbl1$low_a <- paste0("(", tbl1$low_a)
tbl1$high_a <- paste0(tbl1$high_a, ")")
tbl1$low_b <- paste0("(", tbl1$low_b)
tbl1$high_b <- paste0(tbl1$high_b, ")")
knitr::kable(tbl1, "latex", booktabs = TRUE)


# Create Table 2 -----

# Biome split ---
load("data/est_extended_qu_biome.rda")
eff_sdm <- readRDS("outputs/est_extended_qu_biome.rds")
eff_sdm$total <- eff_sdm$direct + eff_sdm$indirect

# Posterior summary
tbl2a_tot <- apply(eff_sdm$total, 2, function(x) {
  c(mean(x), HPDinterval(mcmc(x)))
})
# tbl2a_dir <- apply(eff_sdm$direct, 2, function(x) {
#   c(mean(x), HPDinterval(mcmc(x)))
# })
# tbl2a_ind <- apply(eff_sdm$indirect, 2, function(x) {
#   c(mean(x), HPDinterval(mcmc(x)))
# })

tbl2a <- data.frame(direct = t(tbl2a_tot),
  row.names = c("c", out_sdm$meta$var_names))
# Subset
tbl2a <- cbind(tbl2a[paste0(c("crop_px_km2_lag", "soy_filled_lag", 
    "pasture_px_km2_lag", "cattle_dens_lag_log"), "_a"), ],
  tbl2a[paste0(c("crop_px_km2_lag", "soy_filled_lag", 
    "pasture_px_km2_lag", "cattle_dens_lag_log"), "_b"), ])
# Output
colnames(tbl2a) <- c("mean_a", "low_a", "high_a", 
  "mean_b", "low_b", "high_b")
row.names(tbl2a) <- c("Croplands", "Soy yields", "Pasture", "Cattle")
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
tbl2b_tot <- apply(eff_sdm$total, 2, function(x) {
  c(mean(x), HPDinterval(mcmc(x)))
})
# tbl2b_dir <- apply(eff_sdm$direct, 2, function(x) {
#   c(mean(x), HPDinterval(mcmc(x)))
# })
# tbl2b_ind <- apply(eff_sdm$indirect, 2, function(x) {
#   c(mean(x), HPDinterval(mcmc(x)))
# })

tbl2b <- data.frame(direct = t(tbl2b_tot),
  row.names = c("c", out_sdm$meta$var_names))
# Subset
tbl2b <- cbind(tbl2b[paste0(c("crop_px_km2_lag", "soy_filled_lag", 
    "pasture_px_km2_lag", "cattle_dens_lag_log"), "_pre"), ],
  tbl2b[paste0(c("crop_px_km2_lag", "soy_filled_lag", 
    "pasture_px_km2_lag", "cattle_dens_lag_log"), "_post"), ])
# Output
colnames(tbl2b) <- c("mean_a", "low_a", "high_a", 
  "mean_b", "low_b", "high_b")
row.names(tbl2b) <- c("Croplands", "Soy yields", "Pasture", "Cattle")
tbl2b <- round(tbl2b, 3)
tbl2a$low_a <- paste0("(", tbl2a$low_a)
tbl2a$high_a <- paste0(tbl2a$high_a, ")")
tbl2a$low_b <- paste0("(", tbl2a$low_b)
tbl2a$high_b <- paste0(tbl2a$high_b, ")")
knitr::kable(tbl2b, "latex", booktabs = TRUE)


# Magnitude results -----

# Calculate predictions after reducing driver occurrence to 90%

km2 <- data %>% filter(date == 2017) %>%
  select(area_m2) %>% .$area_m2 / 1e6
def <- function(prediction) {
  prediction * km2 / 100 # Deforestation from ha/km^2 to km^2
}

# SDM ---

load("data/est_extended_qu.rda")

var_names <- out_sdm$meta$var_names
# Get the data for the last year
X <- X_crop_ind <- X_crop_tot <- X_catt_dir <- X_catt_tot <- 
  tail(out_sdm$meta$X, 141L)
W <- out_sdm$meta$W[1:141, 1:141]
# Get coefficients
beta <- apply(out_sdm$beta, 2, mean)
rho <- mean(out_sdm$rho)

# Croplands (indirect and both)
X_crop_ind[, 1 + length(var_names) + which(grepl("crop", var_names))] <-
  X[, 1 + length(var_names) + which(grepl("crop", var_names))] * .9
X_crop_tot[, c(1 + which(grepl("crop", var_names)),
  1 + length(var_names) + which(grepl("crop", var_names)))] <-
  X[, c(1 + which(grepl("crop", var_names)), 1 + length(var_names) +
    which(grepl("crop", var_names)))] * .9
# Cattle (direct and both) -- note the log-scale
X_catt_dir[, 1 + which(grepl("cattle", var_names))] <-
  log(exp(X[, 1 + which(grepl("cattle", var_names))]) * .9)
X_catt_tot[, c(1 + which(grepl("cattle", var_names)),
  1 + length(var_names) + which(grepl("cattle", var_names)))] <-
  log(exp(X[, c(1 + which(grepl("cattle", var_names)),
    1 + length(var_names) + which(grepl("cattle", var_names)))]) * .9)

pred_true <- tail(out_sdm$meta$y, 141L)
pred_base <- solve(diag(141) - rho * W, X %*% beta)[, 1]
pred_crop_ind <- solve(diag(141) - rho * W, X_crop_ind %*% beta)[, 1] # Less indirect croplands
pred_crop_tot <- solve(diag(141) - rho * W, X_crop_tot %*% beta)[, 1] # Less croplands
pred_catt_dir <- solve(diag(141) - rho * W, X_catt_dir %*% beta)[, 1] # Fewer direct cattle (count)
pred_catt_tot <- solve(diag(141) - rho * W, X_catt_tot %*% beta)[, 1] # Fewer cattle (count)

# Deforestation in ha/km^2
summary(pred_true)
summary(pred_base)
summary(pred_crop_ind)
summary(pred_crop_tot)
summary(pred_catt_dir)
summary(pred_catt_tot)

# Total km^2
sum(def(pred_true))
sum(def(pred_base))
sum(def(pred_crop_ind))
sum(def(pred_crop_tot))
sum(def(pred_catt_dir))
sum(def(pred_catt_tot))

# Classical ---

var_names <- out_clm$meta$var_names
X <- X_crop <- X_catt <- tail(out_clm$meta$X, 141L)
beta <- apply(out_clm$beta, 2, mean)

# Croplands
X_crop[, 1 + which(grepl("crop", var_names))] <-
  X[, 1 + which(grepl("crop", var_names))] * .9
# Cattle
X_catt[, 1 + which(grepl("cattle", var_names))] <-
  log(exp(X[, 1 + which(grepl("cattle", var_names))]) * .9)

# Deforestation in ha/km^2
summary(pred <- X %*% beta) # Plain
summary(pred_crop <- X_crop %*% beta) # Less croplands
summary(pred_catt <- X_catt %*% beta) # Less cattle (count)

# Total km^2
sum(def(pred))
sum(def(pred_crop))
sum(def(pred_catt))


# Dependencies ------------------------------------------------------------

stopifnot(
  file.exists("data/data.rds"),
  require("Matrix"),
  require("sf"),
  require("dplyr"),
  require("spatialreg")
)

source("R/50_estimation.R")
source("R/51_supp.R")
source("R/52_helpers.R")


# Prep data ---------------------------------------------------------------

data <- readRDS("data/data.rds")

dates <- 2006:2017
dates_len <- length(dates)


# Prep models -------------------------------------------------------------

# Models
variables <- list(
  extended = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
    "pop_km2_lag_log",
    "cattle_dens_lag_log", "soy_filled_lag",
    "spei_wet", "spei_dry", "biome_a"),
  base = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
    "pop_km2_lag_log", "gdp_cap_lag_log",
    "cattle_dens_lag_log", "soy_filled_lag",
    "spei_wet", "spei_dry", "biome_a"),
  base_lags = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
    "forest_px_km2_lag2", "pasture_px_km2_lag2", "crop_px_km2_lag2",
    "pop_km2_lag_log", "gdp_cap_lag_log",
    "cattle_dens_lag_log", "soy_filled_lag",
    "spei_wet", "spei_dry"),
  base_lag2 = c("forest_ch_km2",
    "forest_px_km2_lag2", "pasture_px_km2_lag2", "crop_px_km2_lag2",
    "pop_km2_lag_log", "gdp_cap_lag_log",
    "cattle_dens_lag_log", "soy_filled_lag",
    "spei_wet", "spei_dry"),
  no_pop = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
    "gdp_cap_lag_log",
    "cattle_dens_lag_log", "soy_filled_lag",
    "spei_wet", "spei_dry"),
  no_inc = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
    "pop_km2_lag_log",
    "cattle_dens_lag_log", "soy_filled_lag",
    "spei_wet", "spei_dry"),
  contemp = c("forest_ch_km2",
    "forest_px_km2", "pasture_px_km2", "crop_px_km2",
    "pasture_px_km2_lag", "crop_px_km2_lag",
    "pop_km2_log", "gdp_cap_lag",
    "pop_km2_lag_log", "gdp_cap_lag_log",
    "cattle_dens_log", "soy_filled",
    "cattle_dens_lag_log", "soy_filled_lag",
    "spei_wet_lag", "spei_dry_lag",
    "spei_wet", "spei_dry"),
  no_yields = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
    "pop_km2_lag_log", "gdp_cap_lag_log",
    "spei_wet", "spei_dry"),
  no_land = c("forest_ch_km2",
    "forest_px_km2_lag",
    "pop_km2_lag_log", "gdp_cap_lag_log",
    "cattle_dens_lag_log", "soy_filled_lag",
    "spei_wet", "spei_dry"),
  all_land = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag",
    "crop_px_km2_lag", "cerr_px_km2_lag",
    "pop_km2_lag_log", "gdp_cap_lag_log",
    "cattle_dens_lag_log", "soy_filled_lag",
    "spei_wet", "spei_dry"))

# Weights
Ws <- list(
  qu = get_weights(data, type = "queen"),
  k5 = get_weights(data, type = "knear", k = 5),
  k7 = get_weights(data, type = "knear", k = 7),
  k9 = get_weights(data, type = "knear", k = 9)
)


# Prep sampler ------------------------------------------------------------

n_rho <- 500
n_draw <- 10000L
n_burn <- 2000L
sigma_a <- 10
sigma_b <- 1
beta_mean <- 0
beta_var <- 10 ^ 6
rho_a <- 1.01


# Execute -----------------------------------------------------------------

model <- "extended"
weights <- "qu"
re_grid <- FALSE
agr_interact <- TRUE

for(model in names(variables)) {

x <- get_matrix(data, variables[[model]], dates)
if(agr_interact && grepl("biome_a", variables[[model]])) {
  int <- x [, grepl("(pasture|crop|cattle|soy)", colnames(x))]
  colnames(int) <- paste0(colnames(int), "_a")
  ind <- x[, grepl("biome_a", colnames(x))]
  x <- cbind(x[, !grepl("biome_a", colnames(x))], int * ind)
}

if(!exists("rho_grid") || re_grid) {
  rho_grid <- get_grid(NULL, W_pre = Ws[[weights]],
    y = get_matrix(data, "forest_ch_km2", dates),
    N = nrow(x), n_rho = n_rho, type = "eigen")
  saveRDS(rho_grid, "data/rho_grid.rds")
}

out_sdm <- sar(x, Ws[[weights]], LX = TRUE,
  n_draw = n_draw, n_burn = n_burn, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time = length(dates),
  sigma_a = sigma_a, sigma_b = sigma_b,
  beta_mean = beta_mean, beta_var = beta_var, rho_a = rho_a,
  grid = rho_grid, verbose = TRUE)
out_sar <- sar(x, Ws[[weights]], LX = FALSE,
  n_draw = n_draw, n_burn = n_burn, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time = length(dates),
  sigma_a = sigma_a, sigma_b = sigma_b,
  beta_mean = beta_mean, beta_var = beta_var, rho_a = rho_a,
  grid = rho_grid, verbose = TRUE)
out_slx <- clm(x, Ws[[weights]], LX = TRUE,
  n_draw = n_draw, n_burn = n_burn, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time = length(dates),
  sigma_a = sigma_a, sigma_b = sigma_b,
  beta_mean = beta_mean, beta_var = beta_var,
  verbose = TRUE)
out_clm <- clm(x, LX = FALSE,
  n_draw = n_draw, n_burn = n_burn, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time = length(dates),
  sigma_a = sigma_a, sigma_b = sigma_b,
  beta_mean = beta_mean, beta_var = beta_var,
  verbose = TRUE)

save(out_sdm, out_sar, out_slx, out_clm,
  file = paste0("data/est_", model, "_", weights, ".rda"))

}

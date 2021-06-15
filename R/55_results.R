
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
# Weights
Ws <- list(
  qu = get_weights(data, type = "queen"),
  k5 = get_weights(data, type = "knear", k = 5),
  k7 = get_weights(data, type = "knear", k = 7),
  k9 = get_weights(data, type = "knear", k = 9)
)


# Prep sampler ------------------------------------------------------------

n_rho <- 500
n_draw <- 25000L
n_burn <- 5000L
sigma_a <- 2
sigma_b <- 1
beta_mean <- 0
beta_var <- 10 ^ 6
rho_a <- 1.01


# Execute -----------------------------------------------------------------

model <- list("extended" = c("forest_ch_km2",
  "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
  "cattle_dens_lag_log", "soy_filled_lag",
  "pop_km2_lag_log", "spei_dry"))
weights <- "qu"
re_grid <- FALSE
agr_interact <- FALSE
time_interact <- FALSE

# Estimate all
todo <- expand.grid(data.frame(
  "agr_interact" = c(TRUE, FALSE),
  "time_interact" = c(TRUE, FALSE)
))
todo$weight <- "qu"
# Lazy for k5 here
todo <- rbind(todo, c(TRUE, TRUE, "k5"))


# Estimate ----------------------------------------------------------------

for(i in seq_len(nrow(todo))) {
  weight <- todo[i, "weight"]
  agr_interact <- todo[i, "agr_interact"]
  time_interact <- todo[i, "time_interact"]

x <- get_matrix(data,
  if(agr_interact) {c(model[[1]], "biome_a")} else {model[[1]]}, dates)

if(agr_interact) {
  y <- x[, 1]
  x <- x[, -1]
  int <- int2 <- x[, grepl("(pasture|crop|cattle|soy)", colnames(x))]
  colnames(int) <- paste0(colnames(int), "_a")
  colnames(int2) <- paste0(colnames(int2), "_b")
  ind <- x[, grepl("biome_a", colnames(x))]
  x <- x[, !grepl("(pasture|crop|cattle|soy|_a)", colnames(x))]
  x <- cbind(y, x, int * ind, int2 * abs(ind - 1))
}
if(time_interact) {
  y <- x[, 1]
  x <- x[, -1]
  int <- int2 <- x[, grepl("(pasture|crop|cattle|soy)", colnames(x))]
  colnames(int) <- paste0(colnames(int), "_pre")
  colnames(int2) <- paste0(colnames(int2), "_post")
  ind <- c(rep(1, nrow(x) / 2), rep(0, nrow(x) / 2))
  x <- x[, !grepl("(pasture|crop|cattle|soy|_a)", colnames(x))]
  x <- cbind(y, x, int * ind, int2 * abs(ind - 1))
}
var_names <- colnames(x)[-1]

if(!exists("rho_grid") || re_grid) {
  rho_grid <- get_grid(NULL, W_pre = Ws[[weights]],
    y = get_matrix(data, "forest_ch_km2", dates),
    N = nrow(x), n_rho = n_rho, type = "eigen")
  saveRDS(rho_grid, paste0("data/rho_grid_", weights, ".rds"))
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
  file = paste0("data/est_", names(model)[[1]], "_", weights,
    if(agr_interact) {"_int"}, if(time_interact) {"_split"}, ".rda"))

}

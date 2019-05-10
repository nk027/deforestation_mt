
library(MASS)
library(dplyr)
library(spdep)
source("R/5_functions.R")
source("R/7_functions.R")
source("R/7_panel.R")

data <- readRDS("data/data.rds")

dates <- c(2005, 2015)
dates_len <- length(dates[1]:dates[2])
   

# Build model -------------------------------------------------------------

names(data)

municipio_subset <- c()
# municipio_subset <- read.table("municipios.txt")[[1]]
data <- data %>% filter(!code %in% municipio_subset)

data <- data %>% 
  mutate(
    forest_px_km2_lag = lag(forest_px_km2),
    pasture_px_km2_lag = lag(pasture_px_km2),
    cerr_px_km2_lag = lag(cerr_px_km2),
    crop_px_km2_lag = lag(crop_px_km2),
    cattle_dens_lag = lag(cattle_dens),
    max_yield_brl_lag = lag(max_yield_brl),
    milk_brl_cow_lag = lag(milk_brl_cow),
    spei_wet_lag = lag(spei_wet),
    spei_dry_lag = lag(spei_dry),
    pop_km2_lag = lag(pop_km2),
    gdp_cap_lag = lag(gdp_cap),
    crop_px_km2_lag2 = lag(crop_px_km2, 2L),
    crop_px_km2_lag3 = lag(crop_px_km2, 3L),
    crop_px_km2_lag4 = lag(crop_px_km2, 4L)
  )

variables <- list(
  base = c("forest_ch_km2", 
    "forest_px_km2", "pasture_px_km2", "crop_px_km2", 
    "pop_km2", "gdp_cap", "milk_brl_cow", "cattle_dens", 
    "max_yield_brl", "spei_wet", "spei_dry"),
  lag = c("forest_ch_km2", 
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag", 
    "pop_km2_lag", "gdp_cap_lag", "milk_brl_cow_lag", "cattle_dens_lag", 
    "max_yield_brl_lag", "spei_wet_lag", "spei_dry_lag"),
  lag_crop = c("forest_ch_km2", 
    "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag", 
    "pop_km2", "gdp_cap", "milk_brl_cow", "cattle_dens", 
    "max_yield_brl_lag", "spei_wet", "spei_dry"),
  lag_crop4 = c("forest_ch_km2", 
    "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag4", 
    "pop_km2", "gdp_cap", "milk_brl_cow", "cattle_dens", 
    "max_yield_brl", "spei_wet", "spei_dry"),
  lag_spei = c("forest_ch_km2", 
    "forest_px_km2", "pasture_px_km2", "crop_px_km2", 
    "pop_km2", "gdp_cap", "milk_brl_cow", "cattle_dens", 
    "max_yield_brl", "spei_wet_lag", "spei_dry_lag"),
  base_cerr = c("forest_ch_km2", 
    "forest_px_km2", "cerr_px_km2", "pasture_px_km2", "crop_px_km2", 
    "pop_km2", "gdp_cap", "milk_brl_cow", "cattle_dens", 
    "max_yield_brl", "spei_wet", "spei_dry"),
  lag_cerr = c("forest_ch_km2", 
    "forest_px_km2_lag", "cerr_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag", 
    "pop_km2_lag", "gdp_cap_lag", "milk_brl_cow_lag", "cattle_dens_lag", 
    "max_yield_brl_lag", "spei_wet_lag", "spei_dry_lag"),
  lag_crop_cerr = c("forest_ch_km2", 
    "forest_px_km2", "cerr_px_km2", "pasture_px_km2", "crop_px_km2_lag", 
    "pop_km2", "gdp_cap", "milk_brl_cow", "cattle_dens", 
    "max_yield_brl_lag", "spei_wet", "spei_dry"),
  lag_lu_cerr = c("forest_ch_km2", 
    "forest_px_km2_lag", "cerr_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag", 
    "pop_km2", "gdp_cap", "milk_brl_cow", "cattle_dens", 
    "max_yield_brl_lag", "spei_wet", "spei_dry"),
  base_lim = c("forest_ch_km2", 
    "forest_px_km2", "pasture_px_km2", "crop_px_km2", 
    "pop_km2", "milk_brl_cow", "cattle_dens", 
    "max_yield_brl", "spei_wet"),
  lag_lim = c("forest_ch_km2", 
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag", 
    "pop_km2_lag", "milk_brl_cow_lag", "cattle_dens_lag", 
    "max_yield_brl_lag", "spei_wet_lag"),
  base_vlim = c("forest_ch_km2",
    "forest_px_km2", "pasture_px_km2", "crop_px_km2", 
    "pop_km2", "cattle_dens", "max_yield_brl", "spei_wet"),
  lag_crop_vlim = c("forest_ch_km2",
    "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag", 
    "pop_km2", "cattle_dens", "max_yield_brl_lag", "spei_wet"),
  lag_catt_vlim = c("forest_ch_km2",
    "forest_px_km2", "pasture_px_km2_lag", "crop_px_km2", 
    "pop_km2", "cattle_dens_lag", "max_yield_brl", "spei_wet"),
  lag_vlim = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag", 
    "pop_km2_lag", "cattle_dens_lag", "max_yield_brl_lag", "spei_wet_lag")
)

matrices <- list()
results_qu <- list()
results_kn <- list()

W_qu <- get_W(data, type = "queen")
W_kn <- get_W(data, type = "knear", k = 7)

counter <- 1
for(counter in seq_along(variables)) {
  matrices[[counter]] <- get_matr(data, variables[[counter]], dates = dates)
  results_qu[[counter]] <- sdm_panel(matrices[[counter]], W_qu, dates_len)
  results_kn[[counter]] <- sdm_panel(matrices[[counter]], W_kn, dates_len)
}



# Print -------------------------------------------------------------------


print_results <- function(x) {
  x$res_effects[2] <- ifelse(x$res_effects[2] < 0, -1, 1)
  x$res_effects[4] <- ifelse(x$res_effects[4] < 0, -1, 1)
  x$res_effects[3] <- ifelse(abs(x$res_effects[3]) > 2, 3, 
                             ifelse(abs(x$res_effects[3]) > 1.5, 2,
                                    ifelse(abs(x$res_effects[3]) > 1, 1, 0)))
  x$res_effects[5] <- ifelse(abs(x$res_effects[5]) > 2, 3, 
                             ifelse(abs(x$res_effects[5]) > 1.5, 2,
                                    ifelse(abs(x$res_effects[5]) > 1, 1, 0)))
  print(x$res_effects)
  print(c("R2" = x$res_other[1, 2]))
  print(c("rho" = mean(x$rho_post)))
}

summarise_results <- function(x) {
  
  x$res_effects[2] <- round(x$res_effects[2], 7)
  x$res_effects[4] <- round(x$res_effects[4], 7)
  x$res_effects[3] <- ifelse(abs(x$res_effects[3]) > 2.576, " ***", 
                             ifelse(abs(x$res_effects[3]) > 1.96, " **",
                                    ifelse(abs(x$res_effects[3]) > 1.645, " *", "")))
  x$res_effects[5] <- ifelse(abs(x$res_effects[5]) > 2.576, " ***", 
                             ifelse(abs(x$res_effects[5]) > 1.96, " **",
                                    ifelse(abs(x$res_effects[5]) > 1.645, " *", "")))
  
  tibble("variable" = c(as.character(x$res_effects[[1]]), 
                        "Rho", "R2", "AIC", "BIC"), 
         "value/direct" = c(paste0(x$res_effects[[2]], x$res_effects[[3]]),
                            round(mean(x$rho_post), 3),
                            round(x$res_other[1, 2], 3),
                            round(x$res_other[3, 2], 1),
                            round(x$res_other[4, 2], 1)),
         "indirect" = c(paste0(x$res_effects[[4]], x$res_effects[[5]]),
                        "", "", "", ""))
}

print_vars <- function(x) {
  paste0(x[1], " ~ ", paste(x[-1], collapse = " + "))
}

# sdm_fit(matrices[[counter]], dates_len, dates[2] + 1,
#         results[[counter]]$beta_post, results[[counter]]$rho_post)


# curr_resid <- as.matrix((diag(N) - curr_rho * W) %*% y - (X %*% curr_beta))
# SSR <- crossprod(curr_resid)
# TSS <- crossprod(y - mean(y))
# R2_post[s] <- 1 - SSR / TSS



png("plots/rho_densities.png", width = 800, height = 400, pointsize = 18)
op <- par(mar = c(2, 2, 2, 0.5))
plot(density(results_qu[[1]]$rho_post), xlim = c(0.4, 1), ylim = c(0, 15), 
     col = "darkgreen", main = "Rho posterior densities")
for(i in 2:length(results_qu)) lines(density(results_qu[[i]]$rho_post), col = "darkgreen")
for(i in 1:length(results_kn)) lines(density(results_kn[[i]]$rho_post), col = "darkgreen")
par(op)
dev.off()

png("plots/r2_density.png", width = 1200, height = 600)
plot(density(c(sapply(results_qu, function(x) x$res_other[1, 2]),
               sapply(results_kn, function(x) x$res_other[1, 2]))), 
     main = "R2 density")
dev.off()

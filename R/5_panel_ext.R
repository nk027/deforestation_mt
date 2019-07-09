
library(MASS)
library(dplyr)
library(sf)
library(spdep)

source("R/5_functions.R")

data <- readRDS("data/data_soyed.rds")

dates <- c(2005, 2015)
dates_len <- length(dates[1]:dates[2])


# Setup -------------------------------------------------------------------

names(data)

municipio_subset <- c()
# municipio_subset <- read.table("txt/municipios.txt")[[1]]
data <- data %>% filter(!code %in% municipio_subset)

# Lags
data <- data %>% 
  mutate(
    forest_px_km2_lag = lag(forest_px_km2),
    pasture_px_km2_lag = lag(pasture_px_km2),
    cerr_px_km2_lag = lag(cerr_px_km2),
    crop_px_km2_lag = lag(crop_px_km2),
    cattle_dens_lag = lag(cattle_dens),
    max_yield_brl_lag = lag(max_yield_brl),
    soy_filled_lag = lag(soy_filled),
    milk_brl_cow_lag = lag(milk_brl_cow),
    spei_wet_lag = lag(spei_wet),
    spei_dry_lag = lag(spei_dry),
    pop_km2_lag = lag(pop_km2),
    gdp_cap_lag = lag(gdp_cap),
    crop_px_km2_lag2 = lag(crop_px_km2, 2L),
    crop_px_km2_lag3 = lag(crop_px_km2, 3L),
    crop_px_km2_lag4 = lag(crop_px_km2, 4L)
  )


# Variables ---------------------------------------------------------------

variables <- list(
  base = c("forest_ch_km2", 
           "forest_px_km2", "pasture_px_km2", "crop_px_km2", 
           "pop_km2", "gdp_cap", "cattle_dens", 
           "soy_filled", "spei_wet", "spei_dry"),
  lag_crop = c("forest_ch_km2", 
               "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag", 
               "pop_km2", "gdp_cap", "cattle_dens", 
               "soy_filled_lag", "spei_wet", "spei_dry"),
  lag_catt = c("forest_ch_km2", 
               "forest_px_km2", "pasture_px_km2_lag", "crop_px_km2", 
               "pop_km2", "gdp_cap", "cattle_dens_lag", 
               "soy_filled", "spei_wet", "spei_dry"),
  base_vlim = c("forest_ch_km2",
                "forest_px_km2", "pasture_px_km2", "crop_px_km2", 
                "pop_km2", "cattle_dens", "soy_filled", "spei_wet"),
  lag_crop_vlim = c("forest_ch_km2",
                    "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag", 
                    "pop_km2", "cattle_dens", "soy_filled_lag", "spei_wet"),
  lag_catt_vlim = c("forest_ch_km2",
                    "forest_px_km2", "pasture_px_km2_lag", "crop_px_km2", 
                    "pop_km2", "cattle_dens_lag", "soy_filled", "spei_wet"),
  lag_vlim = c("forest_ch_km2",
               "forest_px_km2", "pasture_px_km2", "crop_px_km2", 
               "pop_km2_lag", "gdp_cap_lag", "cattle_dens", "soy_filled", "spei_wet")
)

matrices <- list()
results_qu <- list()
results_kn <- list()

W_qu <- get_W(data, type = "queen")
W_kn <- get_W(data, type = "knear", k = 7)


# Calculate ---------------------------------------------------------------

# matrices[[1]] <- get_matr(data, variables[[1]], dates = dates)

counter <- 1
for(counter in seq_along(variables)) {
  matrices[[counter]] <- get_matr(data, variables[[counter]], dates = dates)
  results_qu[[counter]] <- sdm_panel(matrices[[counter]], W_qu, dates_len)
  results_kn[[counter]] <- sdm_panel(matrices[[counter]], W_kn, dates_len)
}


# Chow test ---------------------------------------------------------------

matrices1 <- list()
matrices2 <- list()
results_qu1 <- list()
results_kn1 <- list()
results_qu2 <- list()
results_kn2 <- list()

# Breaks: 2008, 2010
dates <- c(2005, 2008)
dates_len <- length(dates[1]:dates[2])

counter <- 1
for(counter in seq_along(variables)) {
  matrices1[[counter]] <- get_matr(data, variables[[counter]], dates = dates)
  results_qu1[[counter]] <- sdm_panel(matrices1[[counter]], W_qu, dates_len)
  results_kn1[[counter]] <- sdm_panel(matrices1[[counter]], W_kn, dates_len)
}

dates <- c(2009, 2015)
dates_len <- length(dates[1]:dates[2])

counter <- 1
for(counter in seq_along(variables)) {
  matrices2[[counter]] <- get_matr(data, variables[[counter]], dates = dates)
  results_qu2[[counter]] <- sdm_panel(matrices2[[counter]], W_qu, dates_len)
  results_kn2[[counter]] <- sdm_panel(matrices2[[counter]], W_kn, dates_len)
}

i <- 1
chow <- (results_qu[[i]]$ssr - (results_qu1[[i]]$ssr + results_qu2[[i]]$ssr)) / (length(variables[[i]]) - 1) /
  (results_qu1[[i]]$ssr + results_qu2[[i]]$ssr) / (nrow(matrices1[[i]]) + nrow(matrices2[[i]]) - 2 * (length(variables[[i]]) - 1))
significance <- qf(0.95, (length(variables[[i]]) - 1), (nrow(matrices1[[i]]) + nrow(matrices2[[i]]) - 2 * (length(variables[[i]]) - 1)))
# chow > significance
chow
i <- i + 1


library(dplyr)
library(sf)

source("R/5_functions.R")

data <- readRDS("data/data_soyed.rds")


# Prep data ---------------------------------------------------------------

# Subset
municipio_subset <- c()
# municipio_subset <- read.table("txt/municipios.txt")[[1]]
data <- data %>% filter(!code %in% municipio_subset)

# Transform variables
data <- data %>% 
  mutate( # Lag
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
    gdp_cap_lag = lag(gdp_cap)
  ) %>% 
  mutate( # Log
    forest_px_km2_log = log(forest_px_km2),
    pasture_px_km2_log = log(pasture_px_km2),
    cerr_px_km2_log = log(cerr_px_km2),
    crop_px_km2_log = log(crop_px_km2),
    cattle_dens_log = log(cattle_dens),
    max_yield_brl_log = log(max_yield_brl),
    soy_filled_log = log(soy_filled),
    milk_brl_cow_log = log(milk_brl_cow),
    spei_wet_log = log(spei_wet),
    spei_dry_log = log(spei_dry),
    pop_km2_log = log(pop_km2),
    gdp_cap_log = log(gdp_cap)
  )



# Prep models -------------------------------------------------------------

variables <- list(
  base = c("forest_ch_km2",
           "forest_px_km2", "pasture_px_km2", "crop_px_km2",
           "pop_km2", "gdp_cap", "cattle_dens", "soy_filled", 
           "spei_wet", "spei_dry"),
  lag_crop1 = c("forest_ch_km2",
                "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag",
                "pop_km2", "gdp_cap", "cattle_dens", "soy_filled_lag", 
                "spei_wet", "spei_dry"),
  lag_crop2 = c("forest_ch_km2",
                "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag",
                "pop_km2", "gdp_cap", "cattle_dens", "soy_filled", 
                "spei_wet", "spei_dry"),
  log = c("forest_ch_km2",
          "forest_px_km2_log", "pasture_px_km2_log", "crop_px_km2_log",
          "pop_km2_log", "gdp_cap_log", "cattle_dens_log", "soy_filled_log", 
          "spei_wet", "spei_dry"),
  base_vlim = c("forest_ch_km2",
                "forest_px_km2", "pasture_px_km2", "crop_px_km2", 
                "pop_km2", "cattle_dens", "soy_filled", "spei_wet"),
  lag_crop1_vlim = c("forest_ch_km2",
                     "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag", 
                     "pop_km2", "cattle_dens", "soy_filled", "spei_wet"),
  log_vlim = c("forest_ch_km2",
               "forest_px_km2_log", "pasture_px_km2_log", "crop_px_km2_log", 
               "pop_km2_log", "cattle_dens_log", "soy_filled_log", "spei_wet")
)

formula_ify <- function(x) { # To convert this for plm & splm
  as.formula(paste0(x[1], " ~ ", paste(x[-1], collapse = " + ")), env = globalenv())
}

# Weights
W_qu <- get_W(data, type = "queen")
# W_k4n <- get_W(data, type = "knear", k = 4)
W_k5n <- get_W(data, type = "knear", k = 5)
W_k7n <- get_W(data, type = "knear", k = 7)

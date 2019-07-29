
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
  group_by(code) %>% 
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
  mutate( # Lag2
    forest_px_km2_lag2 = lag(forest_px_km2, 2),
    pasture_px_km2_lag2 = lag(pasture_px_km2, 2),
    cerr_px_km2_lag2 = lag(cerr_px_km2, 2),
    crop_px_km2_lag2 = lag(crop_px_km2, 2),
    cattle_dens_lag2 = lag(cattle_dens, 2),
    soy_filled_lag2 = lag(soy_filled, 2),
    pop_km2_lag2 = lag(pop_km2),
    gdp_cap_lag2 = lag(gdp_cap)
  ) %>% 
  ungroup() %>% 
  mutate( # Log
    forest_px_km2_log = log(forest_px_km2),
    pasture_px_km2_log = log(pasture_px_km2),
    cerr_px_km2_log = log(cerr_px_km2),
    crop_px_km2_log = log(crop_px_km2),
    crop_px_km2_lag_log = log(crop_px_km2_lag),
    cattle_dens_log = log(cattle_dens),
    max_yield_brl_log = log(max_yield_brl),
    soy_filled_log = log(soy_filled),
    milk_brl_cow_log = log(milk_brl_cow),
    pop_km2_log = log(pop_km2),
    gdp_cap_log = log(gdp_cap)
  ) %>% # Watch out for log(0)
  mutate(
    forest_px_km2_log = ifelse(is.finite(forest_px_km2_log), forest_px_km2_log, -27),
    crop_px_km2_log = ifelse(is.finite(crop_px_km2_log), crop_px_km2_log, -27),
    crop_px_km2_lag_log = ifelse(is.finite(crop_px_km2_lag_log), crop_px_km2_lag_log, -27)
  ) %>% 
  mutate(
    crop_ch_km2 = crop_ch / area_km2,
    pasture_ch_km2 = pasture_ch / area_km2
  )



# Prep models -------------------------------------------------------------

variables <- list(
  base = c("forest_ch_km2",
           "forest_px_km2", "pasture_px_km2", "crop_px_km2",
           "pop_km2", "gdp_cap", "cattle_dens", "soy_filled",
           "spei_wet", "spei_dry"),
  crop = c("forest_ch_km2",
               "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag",
               "pop_km2", "gdp_cap", "cattle_dens", "soy_filled", 
               "spei_wet", "spei_dry"),
  log = c("forest_ch_km2",
          "forest_px_km2_log", "pasture_px_km2_log", "crop_px_km2_log",
          "pop_km2_log", "gdp_cap_log", "cattle_dens_log", "soy_filled_log",
          "spei_wet", "spei_dry"),
  log_crop = c("forest_ch_km2", 
               "forest_px_km2_log", "pasture_px_km2_log", "crop_px_km2_lag_log",
               "pop_km2_log", "gdp_cap_log", "cattle_dens_log", "soy_filled_log", 
               "spei_wet", "spei_dry"),
  base_lim = c("forest_ch_km2",
               "forest_px_km2", "pasture_px_km2", "crop_px_km2",
                "pop_km2", "cattle_dens", "soy_filled", "spei_wet"),
  crop_lim = c("forest_ch_km2",
               "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag", 
               "pop_km2", "cattle_dens", "soy_filled", "spei_wet"),
  log_lim = c("forest_ch_km2",
              "forest_px_km2_log", "pasture_px_km2_log", "crop_px_km2_log",
              "pop_km2_log", "cattle_dens_log", "soy_filled_log", "spei_wet"),
  log_crop_lim = c("forest_ch_km2",
                   "forest_px_km2_log", "pasture_px_km2_log", "crop_px_km2_lag_log", 
                   "pop_km2_log", "cattle_dens_log", "soy_filled_log", "spei_wet"),
  base_vlim = c("forest_ch_km2",
                "forest_px_km2", "pasture_px_km2", "crop_px_km2",
                "pop_km2", "soy_filled"),
  crop_vlim = c("forest_ch_km2",
                "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag", 
                "pop_km2", "soy_filled"),
  log_vlim = c("forest_ch_km2",
               "forest_px_km2_log", "pasture_px_km2_log", "crop_px_km2_log",
               "pop_km2_log", "soy_filled_log"),
  log_crop_vlim = c("forest_ch_km2",
                    "forest_px_km2_log", "pasture_px_km2_log", "crop_px_km2_lag_log", 
                    "pop_km2_log", "soy_filled_log")
)

variables <- c(
  logbase = c("forest_ch_km2",
              "forest_px_km2", "pasture_px_km2", "crop_px_km2",
              "pop_km2_log", "gdp_cap_log", "cattle_dens_log", "soy_filled",
              "spei_wet", "spei_dry"),
  use_lag = c("forest_ch_km2",
              "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
              "pop_km2_log", "gdp_cap_log", "cattle_dens_log", "soy_filled",
              "spei_wet", "spei_dry"),
  use_lag2 = c("forest_ch_km2",
              "forest_px_km2_lag2", "pasture_px_km2_lag2", "crop_px_km2_lag2",
              "pop_km2_log", "gdp_cap_log", "cattle_dens_log", "soy_filled",
              "spei_wet", "spei_dry"),
  spei_lag = c("forest_ch_km2",
              "forest_px_km2", "pasture_px_km2", "crop_px_km2",
              "pop_km2_log", "gdp_cap_log", "cattle_dens_log", "soy_filled",
              "spei_wet_lag", "spei_dry_lag"),
  crop_lag2 = c("forest_ch_km2",
                "forest_px_km2", "pasture_px_km2", "crop_px_km2_lag2",
                "pop_km2_log", "gdp_cap_log", "cattle_dens_log", "soy_filled",
                "spei_wet", "spei_dry"),
  change = c("forest_ch_km2",
             "forest_px_km2", "pasture_ch_km2", "crop_ch_km2",
             "pop_km2_log", "gdp_cap_log", "cattle_dens_log", "soy_filled",
             "spei_wet", "spei_dry")
)

formula_ify <- function(x) { # To convert this for plm & splm
  as.formula(paste0(x[1], " ~ ", paste(x[-1], collapse = " + ")), env = globalenv())
}

# Weights
W_qu <- get_W(data, type = "queen")
# W_k4n <- get_W(data, type = "knear", k = 4)
W_k5n <- get_W(data, type = "knear", k = 5)
W_k7n <- get_W(data, type = "knear", k = 7)

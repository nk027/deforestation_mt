
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
    max_yield_brl_lag2 = lag(max_yield_brl, 2),
    soy_filled_lag2 = lag(soy_filled, 2),
    milk_brl_cow_lag2 = lag(milk_brl_cow, 2),
    spei_wet_lag2 = lag(spei_wet, 2),
    spei_dry_lag2 = lag(spei_dry, 2),
    pop_km2_lag2 = lag(pop_km2, 2),
    gdp_cap_lag2 = lag(gdp_cap, 2)
  ) %>% 
  ungroup() %>% 
  mutate( # Log
    forest_px_km2_log = log(forest_px_km2),
    pasture_px_km2_log = log(pasture_px_km2),
    cerr_px_km2_log = log(cerr_px_km2),
    crop_px_km2_log = log(crop_px_km2),
    cattle_dens_log = log(cattle_dens),
    max_yield_brl_log = log(max_yield_brl),
    soy_filled_log = log(soy_filled),
    milk_brl_cow_log = log(milk_brl_cow),
    pop_km2_log = log(pop_km2),
    gdp_cap_log = log(gdp_cap)
  ) %>% # Watch out for log(0)
  mutate(
    forest_px_km2_lag_log = log(forest_px_km2_lag),
    pasture_px_km2_lag_log = log(pasture_px_km2_lag),
    cerr_px_km2_lag_log = log(cerr_px_km2_lag),
    crop_px_km2_lag_log = log(crop_px_km2_lag),
    cattle_dens_lag_log = log(cattle_dens_lag),
    max_yield_brl_lag_log = log(max_yield_brl_lag),
    soy_filled_lag_log = log(soy_filled_lag),
    milk_brl_cow_lag_log = log(milk_brl_cow_lag),
    pop_km2_lag_log = log(pop_km2_lag),
    gdp_cap_lag_log = log(gdp_cap_lag)
  ) %>% 
  mutate(
    forest_px_km2_log = ifelse(is.finite(forest_px_km2_log), forest_px_km2_log, -27),
    forest_px_km2_lag_log = ifelse(is.finite(forest_px_km2_log), forest_px_km2_log, -27),
    crop_px_km2_log = ifelse(is.finite(crop_px_km2_log), crop_px_km2_log, -27),
    crop_px_km2_lag_log = ifelse(is.finite(crop_px_km2_lag_log), crop_px_km2_lag_log, -27)
  ) %>% 
  mutate(
    crop_ch_km2 = crop_ch / area_km2,
    pasture_ch_km2 = pasture_ch / area_km2,
    crop_ch_km2_lag = lag(crop_ch_km2),
    pasture_ch_km2_lag = lag(pasture_ch_km2)
  )



# Prep models -------------------------------------------------------------

variables <- list(
  base = c("forest_ch_km2",
           "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
           "pop_km2_lag", "gdp_cap_lag", "cattle_dens_lag", "soy_filled_lag",
           "spei_wet_lag", "spei_dry_lag"),
  log = c("forest_ch_km2",
          "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
          "pop_km2_lag_log", "gdp_cap_lag_log", "cattle_dens_lag_log", "soy_filled_lag",
          "spei_wet_lag", "spei_dry_lag"),
  # base_use = c("forest_ch_km2",
  #              "forest_px_km2_lag2", "pasture_px_km2_lag2", "crop_px_km2_lag2",
  #              "pop_km2_lag", "gdp_cap_lag", "cattle_dens_lag", "soy_filled_lag",
  #              "spei_wet_lag", "spei_dry_lag"),
  # log_use = c("forest_ch_km2",
  #             "forest_px_km2_lag2", "pasture_px_km2_lag2", "crop_px_km2_lag2",
  #             "pop_km2_lag_log", "gdp_cap_lag_log", "cattle_dens_lag_log", "soy_filled_lag",
  #             "spei_wet_lag", "spei_dry_lag"),
  # log_spei = c("forest_ch_km2",
  #              "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
  #              "pop_km2_lag_log", "gdp_cap_lag_log", "cattle_dens_lag_log", "soy_filled_lag",
  #              "spei_wet_lag2", "spei_dry_lag2"),
  # log_crop = c("forest_ch_km2",
  #              "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag2",
  #              "pop_km2_lag_log", "gdp_cap_lag_log", "cattle_dens_lag_log", "soy_filled_lag",
  #              "spei_wet_lag", "spei_dry_lag"),
  # base_change = c("forest_ch_km2",
  #                 "forest_px_km2_lag", "pasture_ch_km2_lag", "crop_ch_km2_lag",
  #                 "pop_km2_lag", "gdp_cap_lag", "cattle_dens_lag", "soy_filled_lag",
  #                 "spei_wet_lag", "spei_dry_lag"),
  # log_change = c("forest_ch_km2",
  #                "forest_px_km2_lag", "pasture_ch_km2_lag", "crop_ch_km2_lag",
  #                "pop_km2_lag_log", "gdp_cap_lag_log", "cattle_dens_lag_log", "soy_filled_lag",
  #                "spei_wet_lag", "spei_dry_lag"),
  base_lim = c("forest_ch_km2",
               "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
               "pop_km2_lag", "cattle_dens_lag", "soy_filled_lag",
               "spei_wet_lag"),
  base_vlim1 = c("forest_ch_km2",
                 "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
                 "pop_km2_lag", "spei_wet_lag"),
  base_vlim2 = c("forest_ch_km2",
                 "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
                 "cattle_dens_lag", "soy_filled_lag")
)

formula_ify <- function(x) { # To convert this for plm & splm
  as.formula(paste0(x[1], " ~ ", paste(x[-1], collapse = " + ")), env = globalenv())
}

# Weights
W_qu <- get_W(data, type = "queen")
# W_k4n <- get_W(data, type = "knear", k = 4)
W_k5n <- get_W(data, type = "knear", k = 5)
W_k7n <- get_W(data, type = "knear", k = 7)

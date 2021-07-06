
# Dependencies ------------------------------------------------------------

stopifnot(
  file.exists("data/data.rds"),
  require("Matrix"),
  require("sf"),
  require("dplyr")
)

source("R/50_estimation.R")
source("R/51_supp.R")
source("R/52_helpers.R")


# Export data -------------------------------------------------------------

data <- readRDS("data/data.rds")
dates <- 2006:2017

data %>% filter(date %in% dates) %>% 
  select(date, code, name, area_m2, forest_ch_km2,
    forest_px_km2_lag, pasture_px_km2_lag, crop_px_km2_lag,
    cattle_dens_lag_log, soy_filled_lag,
    pop_km2_lag_log, spei_dry) %>% 
  st_drop_geometry() %>% 
  write.csv("outputs/data.csv")

weights <- get_weights(data, type = "queen")
write.csv(weights, "outputs/connectivity.csv")

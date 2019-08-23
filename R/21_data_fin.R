
# Dependencies ------------------------------------------------------------

stopifnot(
  file.exists("data/data_soy.rds"),
  require(dplyr),
  require(sf)
)

data <- readRDS("data/data_soy.rds")


# Add lags and logs -------------------------------------------------------

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
    gdp_cap_lag = lag(gdp_cap),
    crop_ch_km2_lag = lag(crop_ch_km2),
    pasture_ch_km2_lag = lag(pasture_ch_km2)
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
    gdp_cap_lag2 = lag(gdp_cap, 2),
    crop_ch_km2_lag = lag(crop_ch_km2, 2),
    pasture_ch_km2_lag = lag(pasture_ch_km2, 2)
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
  )

# Store
saveRDS(data, "data/data_raw.rds")


detach("package:dplyr")
detach("package:sf")

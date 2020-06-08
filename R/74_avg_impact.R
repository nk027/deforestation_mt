
# Dependencies ------------------------------------------------------------

stopifnot(
  exists("data"), # 31
  file.exists("txt/result-f_base_qu.csv"), # 56
  require("dplyr"),
  require("sf")
)


# Calculate ---------------------------------------------------------------

area <- data %>% filter(date == 2016) %>% .$area_km2


smy <- data %>% 
  filter(date == 2016) %>% 
  st_set_geometry(NULL) %>%
  dplyr::select(forest_ch_km2, forest_px_km2_lag, pasture_px_km2_lag,
    crop_px_km2_lag, pop_km2_lag_log, gdp_cap_lag_log, cattle_dens_lag_log, 
    soy_filled_lag, spei_wet, spei_dry) %>%
  apply(2, mean)


tbl <- read.csv(paste0("txt/result-f_base_qu.csv"), stringsAsFactors = FALSE)

x <- c(1, smy[-1], 1, smy[-1]) * tbl$slx[1:20]
names(x) <- tbl$vars[1:20]

abs(x) / sum(abs(x), na.rm = TRUE)

y <- x[1:10] + c(0, x[12:20])
names(y) <- names(x)[1:10]
y
abs(y) / sum(abs(y), na.rm = TRUE)

z <- c(1, smy[-1]) * tbl$clm[1:10]
names(z) <- tbl$vars[1:10]
z
abs(z) / sum(abs(z), na.rm = TRUE)

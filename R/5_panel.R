
library(MASS)
library(dplyr)
source("R/7_ln_det.R")
source("R/7_w_matr.R")

data <- readRDS("data/data.rds")

dates <- c(2005, 2016)
dates_len <- length(dates[1]:dates[2])
   

# Prep --------------------------------------------------------------------

names(data)

variables <- c("forest_ch_km2", 
  "forest_px_km2", "pasture_px_km2", "crop_px_km2", "max_yield_brl", "pop_km2", 
  "gdp_cap", "spei_dry", "spei_wet", "milk_brl_cow", "cattle_dens")

matr <- data %>%
  filter(date >= dates[1], date <= dates[2]) %>% 
  ungroup() %>%
  sf:::select.sf(variables) %>% 
  sf::`st_geometry<-`(NULL) %>% 
  as.matrix(matr, rownames.force = FALSE)

W_pre <- get_W(data)
W <- kronecker(diag(dates_len), W_pre)

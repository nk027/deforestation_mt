
library(dplyr)
library(splm)
library(spdep)
library(sf)

data <- readRDS("data/data.rds")


# Neighbour matrices ------------------------------------------------------

nb <- data %>% dplyr::filter(date == 2017) %>% 
  as_Spatial()

qu_nb <- poly2nb(nb, row.names = nb$code, queen = TRUE)
w_qu_nb <- nb2listw(qu_nb, style = "W")
w_queen <- listw2mat(w_qu_nb)
rm(qu_nb)

k_nb <- knearneigh(sp::coordinates(nb), k = 5)
w_k_nb <- nb2listw(knn2nb(k_nb))
w_knear <- listw2mat(w_k_nb)
rm(k_nb)
rm(nb)



# Test -------------------------------------------------------------------

cor_data <- data %>% 
  ungroup() %>% 
  select(forest_ch, cerr_ch, nature_ch, forest_px, cerr_ch, nature_px,
         pasture_px, cattle, chicken, swine, 
         oilseed_ton, oilseed_brl,
         spei_mean, pop, gdp_cap, 
         ends_with("_hha"))

# Correlation test
cor_test <- function(x, cutoff = 0.8, na.0 = TRUE) {
  x$geometry <- NULL
  x <- as.matrix(x)
  if(na.0) {x[is.na(x)] <- 0}
  y <- cor(x)
  diag(y) <- 0
  which(y > cutoff | y < -cutoff, arr.ind = TRUE)
}

cor_test(cor_data, 0.8)
cor_test(cor_data, 0.9)


# Prep for Bayes ----------------------------------------------------------

bayes <- data %>% 
  filter(date > 2004, date < 2017) %>% 
  ungroup() %>%
  sf:::select.sf(forest_ch_km2, forest_px_km2, pasture_px_km2, crop_px_km2, 
         max_yield_brl, pop_km2, gdp_cap, spei_dry, spei_wet, 
         milk_brl_cow, cattle_dens)
bayes$geometry <- NULL
summary(bayes)
bayes <- as.matrix(bayes)



# Stuff -------------------------------------------------------------------

formula <- forest_ch ~ forest_px + cerr_px + pasture_px +
  soy_hha + soy_brl_hha + soy_ton_hha +
  # rice_hha + rice_brl_hha + rice_ton_hha +
  # corn_hha + corn_brl_hha + corn_ton_hha +
  # cott_hha + cott_brl_hha + cott_ton_hha +
  # sugar_hha + sugar_brl_hha + sugar_ton_hha +
  # manioc_hha + manioc_brl_hha + manioc_ton_hha +
  # bean_hha + bean_brl_hha + bean_ton_hha +
  # sunfl_hha + sunfl_brl_hha + sunfl_ton_hha +
  # sorg_hha + sorg_brl_hha + sorg_ton_hha +
  cattle + swine + chicken + oilseed_ton + oilseed_brl +
  mean + pop + gdp_cap

formula <- forest_ch ~ pasture_px + crop_px +
  soy_hha + soy_brl_hha + soy_ton_hha +
  # rice_hha + rice_brl_hha + rice_ton_hha +
  # corn_hha + corn_brl_hha + corn_ton_hha +
  # cott_hha + cott_brl_hha + cott_ton_hha +
  # sugar_hha + sugar_brl_hha + sugar_ton_hha +
  # manioc_hha + manioc_brl_hha + manioc_ton_hha +
  # bean_hha + bean_brl_hha + bean_ton_hha +
  # sunfl_hha + sunfl_brl_hha + sunfl_ton_hha +
  # sorg_hha + sorg_brl_hha + sorg_ton_hha +
  cattle + swine + chicken + oilseed_ton + oilseed_brl +
  mean + pop + gdp_cap

bayes <- shp %>% 
  ungroup() %>%
  sf:::select.sf(forest_ch, forest_px, pasture_px, crop_px, soy_brl_hha, 
         pop, gdp_cap, mean, cattle, area_m2)
bayes$geometry <- NULL
bayes <- as.matrix(bayes)


moran.test(shp$forest_ch, listw = mat2listw(kronecker(diag(12), w_queen)))

sp1 <- lagsarlm(formula, shp, listw = mat2listw(kronecker(diag(12), w_queen)), Durbin = TRUE)

pm1 <- spml(formula, index = c("code", "date"), 
            shp, listw = w_k_nb, model = "within") # FE
pm2 <- spml(formula, index = c("code", "date"), 
            shp, listw = w_qu_nb, model = "random") # RE
pm3 <- spml(formula, index = c("code", "date"), 
            shp, listw = w_qu_nb, spatial.error = "none", lag = TRUE) # SAR
pm4 <- spml(formula, index = c("code", "date"), 
            shp, listw = w_qu_nb, spatial.error = "b") # SEM
pm5 <- spml(formula, index = c("code", "date"), 
            shp, listw = w_qu_nb, spatial.error = "b", lag = TRUE) # SDM

imp1 <- impacts(pm3, listw = w_qu_nb, time = 1)
summary(imp1, zstats = TRUE, short = TRUE)

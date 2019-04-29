
library(dplyr)
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


# Prepare -----------------------------------------------------------------

data_mat <- data %>%
  filter(date > 2004, date < 2017) %>% 
  ungroup() %>%
  sf:::select.sf(forest_ch_km2, forest_px_km2, pasture_px_km2, crop_px_km2, 
                 max_yield_brl, pop_km2, gdp_cap, spei_dry, spei_wet, 
                 milk_brl_cow, cattle_dens) %>% 
  mutate(max_yield_brl = max_yield_brl / 1000,
         milk_brl_cow = milk_brl_cow / 1000)
data_mat$geometry <- NULL
summary(data_mat)
data_mat <- as.matrix(data_mat)

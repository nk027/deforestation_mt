
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


# Data --------------------------------------------------------------------

# Changes
data <- data %>% 
  group_by(code) %>% 
  mutate(forest_ch = forest_px - lag(forest_px),
         cerr_ch = cerr_px - lag(cerr_px),
         nature_px = forest_px + cerr_px,
         nature_ch = forest_ch + cerr_ch)

data <- data %>% 
  mutate(gdp_cap = gdp / pop, 
         crop_px = soy_px + soycorn_px + soycott_px + cott_px + 
           soymill_px + soysunfl_px + sugar_px)

# Visualise
shp <- data %>% filter(date > 2004, date < 2017)
summary(shp)
hist(shp$forest_ch, breaks = 50)
hist(shp$forest_ch[shp$forest_ch < 0], breaks = 50)
hist(shp$cerr_ch, breaks = 50)
hist(shp$nature_ch, breaks = 50)

shp <- shp %>% 
  mutate(rice_brl_hha = rice_brl / rice_hha,
         sugar_brl_hha = sugar_brl / sugar_hha,
         manioc_brl_hha = manioc_brl / manioc_hha,
         corn_brl_hha = corn_brl / corn_hha,
         bean_brl_hha = bean_brl / bean_hha,
         sunfl_brl_hha = sunfl_brl / sunfl_hha,
         sorg_brl_hha = sorg_brl / sorg_hha,
         cott_brl_hha = cott_brl / cott_hha,
         soy_brl_hha = soy_brl / soy_hha) %>% 
  mutate(rice_ton_hha = rice_ton / rice_hha,
         sugar_ton_hha = sugar_ton / sugar_hha,
         manioc_ton_hha = manioc_ton / manioc_hha,
         corn_ton_hha = corn_ton / corn_hha,
         bean_ton_hha = bean_ton / bean_hha,
         sunfl_ton_hha = sunfl_ton / sunfl_hha,
         sorg_ton_hha = sorg_ton / sorg_hha,
         cott_ton_hha = cott_ton / cott_hha,
         soy_ton_hha = soy_ton / soy_hha)

# Set NaN to 0
shp <- shp %>% 
  mutate_at(vars(ends_with("_brl_hha")), .funs = funs(ifelse(is.nan(.), 0, .))) %>% 
  mutate_at(vars(ends_with("_ton_hha")), .funs = funs(ifelse(is.nan(.), 0, .)))

# Same for Oilseed
shp <- shp %>% 
  mutate_at(vars(starts_with("oilseed")), .funs = funs(ifelse(is.na(.), 0, .)))


# Model -------------------------------------------------------------------

cor_data <- shp %>% 
  ungroup() %>% 
  select(forest_ch, cerr_ch, nature_ch, forest_px, cerr_ch, nature_px,
         pasture_px, cattle, chicken, swine, 
         oilseed_ton, oilseed_brl,
         mean, pop, gdp_cap, 
         ends_with("_hha"))

# Correlation test
cor_data$geometry <- NULL
cor_data <- as.matrix(cor_data)
cor_data[is.na(cor_data)] <- 0 # a few values on oilseeds
cor_test <- cor(cor_data)
diag(cor_test) <- 0
cutoff <- 0.8
which(cor_test > cutoff | cor_test < -cutoff, arr.ind = TRUE)


# Panel -------------------------------------------------------------------

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

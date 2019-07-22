
library(dplyr)
library(sf)

timescale <- "03"


# Wrap up all data---------------------------------------------------------

shp <- readRDS("data/geo/shp.rds")
tab <- readRDS("data/tab/tab.rds")
spei <- readRDS(paste0("data/geo/spei", timescale, ".rds"))
iiasa <- readRDS("data/geo/iiasa.rds")

data <- full_join(shp, tab, by = c("code", "date")) %>% 
  full_join(spei, by = c("code", "date")) %>% 
  full_join(iiasa, by = c("code", "date"))

refcols <- c("code", "name", "date")
data <- data[, c(refcols, setdiff(names(data), refcols))]
data <- data %>% ungroup()

saveRDS(data, "data/data_raw.rds")


# Changes------------------------------------------------------------------

data <- readRDS("data/data_raw.rds")

data <- data %>% 
  mutate(gdp_cap = gdp / pop, 
         crop_px = soy_px + soycorn_px + soycott_px + cott_px + 
           soymill_px + soysunfl_px + sugar_px)

data <- data %>% 
  group_by(code) %>% 
  mutate(forest_ch = forest_px - lag(forest_px),
         cerr_ch = cerr_px - lag(cerr_px),
         nature_px = forest_px + cerr_px,
         nature_ch = forest_ch + cerr_ch,
         crop_ch = crop_px - lag(crop_px),
         pasture_ch = pasture_px - lag(pasture_px)) %>% 
  ungroup()

data <- data %>% 
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

# Have a look at NaN values
x <- data[data$date > 2004 & data$date < 2018, grep("brl_hha$", names(data))]
x$geometry <- NULL
apply(x, 2, function(x) sum(is.na(x)))
y <- data[grep("brl_hha$", names(data))]; y$geometry <- NULL
# data$avg_yield_brl <- apply(y, 1, function(x) sum(x, na.rm = TRUE) / sum(!is.na(x)))
data$max_yield_brl <- apply(y, 1, function(x) max(x, 0, na.rm = TRUE))

x <- data[data$date > 2004 & data$date < 2018, grep("ton_hha$", names(data))]
x$geometry <- NULL
apply(x, 2, function(x) sum(is.na(x)))
y <- data[grep("ton_hha$", names(data))]; y$geometry <- NULL
# data$avg_yield <- apply(y, 1, function(x) sum(x, na.rm = TRUE) / sum(!is.na(x)))
data$max_yield <- apply(y, 1, function(x) max(x, 0, na.rm = TRUE))

data %>% filter(date == 2005) %>% 
  select(max_yield, max_yield_brl) %>% plot()

# # Set NaN to 0 - Bad!
# shp <- shp %>% 
#   mutate_at(vars(ends_with("_brl_hha")), .funs = funs(ifelse(is.nan(.), 0, .))) %>% 
#   mutate_at(vars(ends_with("_ton_hha")), .funs = funs(ifelse(is.nan(.), 0, .)))

# Set oilseed NA to 0
data <- data %>% 
  mutate_at(vars(starts_with("oilseed")), .funs = funs(ifelse(is.na(.), 0, .)))


# By area -----------------------------------------------------------------

data <- data %>% 
  mutate(area_km2 = area_m2 / 1000000) %>% 
  mutate_at(vars(ends_with("px")), .funs = funs(. * (231.6564) ^ 2 / 1e6))
# Change px from 232m^2 to 1km^2
  
data <- data %>% 
  mutate(forest_px_km2 = forest_px / area_km2,
         cerr_px_km2 = cerr_px / area_km2,
         nature_px_km2 = nature_px / area_km2,
         forest_ch_km2 = forest_ch / area_km2,
         cerr_ch_km2 = cerr_ch / area_km2,
         nature_ch_km2 = nature_ch / area_km2,
         crop_px_km2 = crop_px / area_km2,
         pasture_px_km2 = pasture_px / area_km2,
         pop_km2 = pop / area_km2,
         cattle_km2 = cattle / area_km2,
         cattle_dens = cattle / pasture_px,
         milk_brl_lt = milk_brl / milk_lt,
         milk_brl_cow = milk_brl / milk_cow,
         milk_lt_cow = milk_lt / milk_cow,
         milk_cow_cattle = milk_cow / cattle,
         milk_cow_km2 = milk_cow / area_km2) %>% 
  ungroup()

saveRDS(data, "data/data.rds")

# Finish by adding filled soy yields
source("R/3_soy_yield.R")


library(dplyr)
library(sf)

data <- readRDS("data/data_soyed.rds")

dates <- seq(2006, 2016)


# Summarise data ---------------------------------------------------------

data_summary <- data %>% 
  filter(date %in% dates) %>% 
  st_set_geometry(NULL) %>% 
  # group_by(name) %>% 
  summarise_at(vars(forest_ch_km2, forest_px_km2_lag, pasture_px_km2_lag,
                    crop_px_km2_lag, pop_km2_lag, gdp_cap_lag, cattle_dens_lag, 
                    soy_filled_lag, spei_wet_lag, spei_dry_lag), 
            .funs = list("mean" = mean, "sd" = sd))# %>% 
  # mutate(name = gsub("(.*)\\ \\(MT\\)$", "\\1", name, perl = TRUE))

data_summary <- data %>% 
  filter(date %in% dates) %>% 
  st_set_geometry(NULL) %>% 
  select(forest_ch_km2, forest_px_km2_lag, pasture_px_km2_lag,
         crop_px_km2_lag, pop_km2_lag, gdp_cap_lag, cattle_dens_lag, 
         soy_filled_lag, spei_wet_lag, spei_dry_lag)

out <- cbind(
  apply(data_summary, 2, min),
  apply(data_summary, 2, quantile, 0.2),
  apply(data_summary, 2, mean),
  apply(data_summary, 2, median),
  apply(data_summary, 2, quantile, 0.8),
  apply(data_summary, 2, max)
)
colnames(out) <- c("Min", "Qu20", "Mean", "Median", "Qu80", "Max")

write.csv(out, 
          file = paste0("txt/data_summary.csv"))

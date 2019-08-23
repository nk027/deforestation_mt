
library(dplyr)
library(sf)
library(ggplot2)

data <- readRDS("data/data.rds")
load("data/models_twoways.rda")

# date_fit <- max(dates) + 1

x <- data %>%
  filter(date == date_fit) %>%
  transmute(act = forest_ch_km2,
            sdm_qu = sdm_qu_fit_mean,
            sdm_k5 = sdm_k5n_fit_mean,
            sdm_k7 = sdm_k7n_fit_mean,
            clm = clm_fit,
            sar_qu = sar_qu_fit,
            sem_qu = sem_qu_fit)

lim <- c(-0.5, 0.1)

map_plot <- cowplot::plot_grid(
  ggplot(x) + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = lim) +
    cowplot::theme_map(),
  ggplot(x) + geom_sf(aes(fill = sdm_qu)) +
    scale_fill_viridis_c(limits = lim) +
    cowplot::theme_map(),
  ggplot(x) + geom_sf(aes(fill = sdm_k5)) +
    scale_fill_viridis_c(limits = lim) +
    cowplot::theme_map(),
  ggplot(x) + geom_sf(aes(fill = sdm_k7)) +
    scale_fill_viridis_c(limits = lim) +
    cowplot::theme_map(),
  ggplot(x) + geom_sf(aes(fill = clm)) +
    scale_fill_viridis_c(limits = lim) +
    cowplot::theme_map(),
  ggplot(x) + geom_sf(aes(fill = sar_qu)) +
    scale_fill_viridis_c(limits = lim) +
    cowplot::theme_map(),
  ggplot(x) + geom_sf(aes(fill = sem_qu)) +
    scale_fill_viridis_c(limits = lim) +
    cowplot::theme_map()
)

print(map_plot)

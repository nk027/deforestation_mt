
library(dplyr)
library(ggplot2)
library(cowplot)
library(sf)

timescale <- "03"


# Explore graphically -----------------------------------------------------

df <- readRDS(paste0("data/geo/spei", timescale, ".rds"))

shp <- read_sf("data/municipios/") %>% 
  st_transform(crs_sin) %>% 
  transmute(code = as.integer(CD_GEOCMU)) %>% 
  right_join(df, by = "code")

i <- 2000

for(i in 2000:2017) {
  x <- shp %>% 
    filter(date == i) %>% 
    mutate(spei_mean = ifelse(spei_mean < -3, -3, spei_mean),
           spei_qu2 = ifelse(spei_qu2 < -3, -3, spei_qu2),
           spei_qu8 = ifelse(spei_qu8 < -3, -3, spei_qu8)) %>% 
    mutate(spei_qu8 = ifelse(spei_qu8 > 2, 2, spei_qu8))
  plot_grid(
    ggplot(x) +
      geom_sf(aes(fill = spei_mean)) +
      scale_fill_viridis_c(option = "plasma", limits = c(-3, 2)),
    ggplot(x) +
      geom_sf(aes(fill = spei_sd)) +
      scale_fill_viridis_c(option = "viridis", limits = c(0, 1.2)),
    ggplot(x) +
      geom_sf(aes(fill = spei_qu2)) +
      scale_fill_viridis_c(option = "plasma", limits = c(-3, 2)),
    ggplot(x) +
      geom_sf(aes(fill = spei_qu8)) +
      scale_fill_viridis_c(option = "plasma", limits = c(-3, 2))
  )
  ggsave(paste0("plots/spei_", i, ".png"), width = 30, height = 20, units = "cm")
}

spei_plot <- function(x, ...) {
  
  plot(density(x), ...)
  lines(density(rnorm(100000, mean = mean(x), sd = sd(x))), col = "darkgray")
  
}

i <- 1
spei_plot(df_spei$value[df_spei$month == i], main = i); i <- i + 1

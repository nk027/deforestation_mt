
library(dplyr)
crs_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

df_spei <- readRDS("data/geo/geo_spei.rds")

df_spei$year <- as.integer(gsub("^([0-9]{4})-..$", "\\1", df_spei$variable))
df_spei$month <- as.integer(gsub("^....-([0-9]{2})$", "\\1", df_spei$variable))
df_spei$variable <- NULL

library(ggplot2)
library(cowplot)
library(sf)

df <- df_spei %>% 
  filter(year < 2018) %>% 
  group_by(year, code) %>% 
  summarise(mean = mean(value), sd = sd(value), 
            qu2 = quantile(value, 0.2), 
            qu5 = quantile(value, 0.5),
            qu8 = quantile(value, 0.8)) %>% 
  mutate(iqr = qu8 - qu2)

shp <- read_sf("data/municipios/") %>% 
  st_transform(crs_sin) %>% 
  transmute(code = as.integer(CD_GEOCMU)) %>% 
  right_join(df, by = "code")

i <- 2000

for(i in 2000:2017) {
  x <- shp %>% 
    filter(year == i) %>% 
    mutate(mean = ifelse(mean < -3, -3, mean),
           qu2 = ifelse(qu2 < -3, -3, qu2),
           qu8 = ifelse(qu8 < -3, -3, qu8)) %>% 
    mutate(qu8 = ifelse(qu8 > 2, 2, qu8))
  plot_grid(
    ggplot(x) +
      geom_sf(aes(fill = mean)) +
      scale_fill_viridis_c(option = "plasma", limits = c(-3, 2)),
    ggplot(x) +
      geom_sf(aes(fill = sd)) +
      scale_fill_viridis_c(option = "viridis", limits = c(0, 1.2)),
    ggplot(x) +
      geom_sf(aes(fill = qu2)) +
      scale_fill_viridis_c(option = "plasma", limits = c(-3, 2)),
    ggplot(x) +
      geom_sf(aes(fill = qu8)) +
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

source("R/settings.R")
library(ggplot2)
library(dplyr)

df_date <- readRDS("data/geo_merged_df_date.rds")

x <- df_date %>% 
  group_by(date) %>% 
  summarise(forest = sum(forest), crop = sum(crop), 
            other = sum(other), pasture = sum(pasture))

ggplot(x, aes(x = date)) +
  geom_line(aes(y = forest), colour = colour["forest"]) +
  geom_line(aes(y = crop), colour = colour["soy_corn"]) +
  geom_line(aes(y = other), colour = colour["urban"]) +
  geom_line(aes(y = pasture), colour = colour["pasture"]) +
  ggthemes::theme_base()

y <- df_date %>% 
  mutate(size = (forest + pasture + crop + other)) %>% 
  filter(size > quantile(size, 0.95))

ggplot(y, aes(x = log(forest), y = log(pasture), colour = date, group = id)) +
  geom_path() + 
  viridis::scale_colour_viridis() +
  ggthemes::theme_base()

cowplot::plot_grid(
  ggplot(x, aes(x = log(forest), y = log(pasture), colour = date)) +
    geom_path() + 
    viridis::scale_colour_viridis() +
    ggthemes::theme_base(),
  ggplot(x, aes(x = log(forest), y = log(crop), colour = date)) +
    geom_path() + 
    ggplot2::scale_colour_viridis_c() +
    ggthemes::theme_base()
)

ggplot(y, aes(y = forest, x = pasture, colour = id)) +
  geom_point() +
  ggplot2::scale_color_viridis_d() +
  facet_grid(. ~ date)

ggplot(y, aes(y = forest, x = pasture, colour = date)) +
  geom_path() +
  ggplot2::scale_color_viridis_c() +
  facet_grid(. ~ id, scales = "free")

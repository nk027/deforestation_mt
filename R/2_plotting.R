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
  geom_line(aes(y = crop), colour = colour["pasture"]) +
  geom_line(aes(y = other), colour = colour["urban"]) +
  geom_line(aes(y = pasture), colour = colour["soy_corn"]) +
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
    viridis::scale_colour_viridis() +
    ggthemes::theme_base()
)

ggplot(y, aes(x = forest, y = crop, colour = id)) +
  geom_point() +
  facet_grid(. ~ date)

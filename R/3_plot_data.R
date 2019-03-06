library(sf)
library(dplyr)

library(ggplot2)
library(cowplot)

library(extrafonts)
extrafont::loadfonts()

source("R/settings.R")

shp <- readRDS("data/data.rds")

# area == m² -> area/1e6 == km²
shp <- shp %>% 
  mutate(gdp_c = gdp / pop, pop_d = log(pop / as.double(area) * 1e6), 
         for_d = forest / 16 / as.double(area) * 1e6, 
         pas_d = pasture / 16 / as.double(area) * 1e6, 
         cro_d = crop / 16 / as.double(area) * 1e6)


# Maps --------------------------------------------------------------------

# GDP per Capita
x <- vector("list", length(c(2002:2006, 2008:2009, 2011:2016)))
i <- 1
for(year in c(2002:2006, 2008:2009, 2011:2016)) {
  x[[i]] <- shp %>% 
    filter(date %in% year) %>% 
    ggplot() +
    ggtitle(year) +
    geom_sf(aes(fill = gdp_c)) +
    scale_fill_viridis_c() +
    theme(text = element_text(family = "DejaVu Sans Mono"), 
          axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          plot.background = element_blank())
  i <- i + 1
}
plot_grid(plotlist = x, ncol = 4)
ggsave("plots/gdp_per_capita.pdf", width = 16, height = 12)

# Population density
x <- vector("list", length(c(2001:2006, 2008:2009, 2011:2018)))
i <- 1
for(year in c(2002:2006, 2008:2009, 2011:2018)) {
  x[[i]] <- shp %>% 
    filter(date %in% year) %>% 
    ggplot() +
    ggtitle(year) +
    geom_sf(aes(fill = pop_d)) +
    scale_fill_viridis_c() +
    theme(text = element_text(family = "DejaVu Sans Mono"), 
          axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          plot.background = element_blank())
  i <- i + 1
}
plot_grid(plotlist = x, ncol = 4)
ggsave("plots/population_density.pdf", width = 16, height = 12)

# Forest
x <- vector("list", length(2002:2017))
i <- 1
for(year in 2002:2017) {
  x[[i]] <- shp %>% 
    filter(date %in% year) %>% 
    ggplot() +
    ggtitle(year) +
    geom_sf(aes(fill = for_d)) +
    scale_fill_viridis_c() +
    theme(text = element_text(family = "DejaVu Sans Mono"), 
          axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          plot.background = element_blank())
  i <- i + 1
}
plot_grid(plotlist = x, ncol = 4)
ggsave("plots/forest_density.pdf", width = 16, height = 12)

# Crop
x <- vector("list", length(2002:2017))
i <- 1
for(year in 2002:2017) {
  x[[i]] <- shp %>% 
    filter(date %in% year) %>% 
    ggplot() +
    ggtitle(year) +
    geom_sf(aes(fill = cro_d)) +
    scale_fill_viridis_c() +
    theme(text = element_text(family = "DejaVu Sans Mono"), 
          axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          plot.background = element_blank())
  i <- i + 1
}
plot_grid(plotlist = x, ncol = 4)
ggsave("plots/crop_density.pdf", width = 16, height = 12)

# Pasture
x <- vector("list", length(2002:2017))
i <- 1
for(year in 2002:2017) {
  x[[i]] <- shp %>% 
    filter(date %in% year) %>% 
    ggplot() +
    ggtitle(year) +
    geom_sf(aes(fill = pas_d)) +
    scale_fill_viridis_c() +
    theme(text = element_text(family = "DejaVu Sans Mono"), 
          axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          plot.background = element_blank())
  i <- i + 1
}
plot_grid(plotlist = x, ncol = 4)
ggsave("plots/pasture_density.pdf", width = 16, height = 12)


# Line --------------------------------------------------------------------


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


# Other -------------------------------------------------------------------

lapply(shp, summary)

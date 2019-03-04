library(sf)
library(dplyr)

library(ggplot2)
library(cowplot)

library(extrafonts)
extrafont::loadfonts()

shp <- readRDS("data/data.rds")

centr <- st_centroid(shp)
centr <- cbind(centr, st_coordinates(st_centroid(shp$geometry)))

# area == m² -> area/1e6 == km²
shp <- shp %>% 
  mutate(gdp_c = gdp / pop, pop_d = log(pop / as.double(area) * 1e6), 
         for_d = forest / 16 / as.double(area) * 1e6, 
         pas_d = pasture / 16 / as.double(area) * 1e6, 
         cro_d = crop / 16 / as.double(area) * 1e6)


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


lapply(shp, summary)

# geom_text(data = centr, aes(x = X, y = Y, label = name),
#           color = "gray", fontface = "bold", check_overlap = TRUE) +

library(dplyr)
library(sf)

library(ggplot2)
library(cowplot)

shp <- readRDS("data/data.rds")

# Other -------------------------------------------------------------------

lapply(shp, summary)

summary(lm(crop ~ ton_soy, data = shp))
summary(lm(crop ~ ton_soy + ton_sug, data = shp))
summary(lm(crop ~ ton_soy + ton_cot, data = shp))
summary(lm(crop ~ ton_soy + ton_sug + ton_cot, data = shp))
plot(lm(crop ~ ton_soy + ton_sug + ton_cot, data = shp))

summary(lm(gdp ~ pop + I(pop^2), data = shp))

summary(lm(forest ~ pop + gdp, data = shp))
plot(lm(forest ~ pop + gdp, data = shp))

shp %>% select(code, date, forest) %>% 
  group_by(code) %>%
  mutate(delta = (forest - lag(forest))) %>% 
  filter(code == 5100102, date > 2008)

shp %>% select(code, date, forest, crop, pasture) %>% 
  filter(date %in% c(2015:2016)) %>% 
  group_by(code)

# Forest changes
x <- vector("list", length(2003:2015))
i <- 1
for(year in 2003:2015) {
  x[[i]] <- shp %>% 
    group_by(code) %>%
    mutate(delta = (forest - lag(forest)) / lag(forest)) %>% 
    filter(date %in% year) %>% 
    select(delta, date, code) %>%
    ggplot() +
    ggtitle(year) +
    geom_sf(aes(fill = delta)) +
    scale_fill_viridis_c(limits = c(-1, 2), na.value = "white") +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          plot.background = element_blank())
  i <- i + 1
}
plot_grid(plotlist = x, ncol = 4)
ggsave("plots/forest_change_pct.pdf", width = 16, height = 12)

# Forest changes
x <- vector("list", length(2003:2015))
i <- 1
for(year in 2003:2015) {
  x[[i]] <- shp %>% 
    group_by(code) %>%
    mutate(delta = forest - lag(forest)) %>% 
    filter(date %in% year) %>% 
    select(delta, date, code) %>%
    ggplot() +
    ggtitle(year) +
    geom_sf(aes(fill = delta)) +
    scale_fill_viridis_c(limits = c(-100000, 100000)) +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          plot.background = element_blank())
  i <- i + 1
}
plot_grid(plotlist = x, ncol = 4)
ggsave("plots/forest_change.pdf", width = 16, height = 12)

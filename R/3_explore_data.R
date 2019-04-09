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
x <- vector("list", length(2003:2017))
i <- 1
for(year in 2003:2017) {
  x[[i]] <- shp %>% 
    group_by(code) %>%
    mutate(delta = (forest - lag(forest)) / lag(forest)) %>% 
    filter(date %in% year) %>% 
    select(delta, date, code) %>%
    ggplot() +
    ggtitle(year) +
    geom_sf(aes(fill = delta)) +
    # scale_fill_viridis_c(limits = c(-1, 2), na.value = "white") +
    scale_fill_gradient2(low = "#440154FF", mid = "#FFFFFF", high = "#FDE725FF",
                         limits = c(-1, 3)) +
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
x <- vector("list", length(2003:2017))
i <- 1
for(year in 2003:2017) {
  x[[i]] <- shp %>% 
    group_by(code) %>%
    mutate(delta = forest - lag(forest)) %>% 
    filter(date %in% year) %>% 
    select(delta, date, code) %>%
    ggplot() +
    ggtitle(year) +
    geom_sf(aes(fill = delta)) +
    # scale_fill_viridis_c(limits = c(-100000, 100000)) +
    scale_fill_gradient2(low = "#440154FF", mid = "#FFFFFF", high = "#FDE725FF",
                         limits = c(-100000, 100000)) +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          plot.background = element_blank())
  i <- i + 1
}
plot_grid(plotlist = x, ncol = 4)
ggsave("plots/forest_change.pdf", width = 16, height = 12)

shp %>% 
  group_by(code) %>%
  filter(date %in% c(2003, 2017)) %>% 
  mutate(delta = forest - lag(forest)) %>% 
  filter(date %in% year) %>% 
  select(delta, date, code) %>%
  ggplot() +
  ggtitle(2017) +
  geom_sf(aes(fill = delta)) +
  # scale_fill_viridis_c() +
  scale_fill_gradient2(low = "#440154FF", mid = "#FFFFFF", high = "#FDE725FF") +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        panel.background = element_blank(), panel.border = element_blank(), 
        plot.background = element_blank())


# explore data

# x <- as_tibble(t(gdp[c(-1, -2)]))
# names(x) <- gdp$code
# x$date <- 2002:2016
x <- as_tibble(t(pop[c(-1, -2)]))
names(x) <- pop$code
x$date <- 2000 + c(1:6, 8:9, 11:18)

i <- 1
x[c(((i + 1) * 18 - 35):min((i * 18), 141), 142)] %>% 
  reshape2::melt(id.vars = "date") %>% 
  ggplot2::ggplot(ggplot2::aes(x = date, colour = variable, y = value)) +
  ggplot2::geom_line()
i <- i + 1
# gdp:
# Cuiabá, 5103403, grows rapidly
# Paranaíta, 5106299, rises sharply in 12, plateaus 13-15 and then falls off.

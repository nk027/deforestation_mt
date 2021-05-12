
library("tmap")
library("dplyr")

data <- readRDS("data/data.rds")


tm <- data %>% filter(date %in% 2006:2017) %>%
  mutate(forest_ch = forest_ch / 100) %>%
  tm_shape() +
  tm_borders(alpha = 1, col = "#e3e3e3") +
  tm_fill("forest_ch", midpoint = 0, palette = "RdGy",
    title = "Forest change (km²)",
    breaks = c(-50000, -40000, -30000, -20000, -10000, -2500,
      2500, 10000, 20000) / 100) +
  tm_facets(by = "date") +
  tm_layout(legend.outside = TRUE, outer.margins = 0,
    legend.text.size = 0.45, bg.color = "transparent",
    legend.outside.position = "right", legend.outside.size = .175)

print(tm)

tmap_save(tm, "outputs/forest_change.png",
  height = 4, width = 5, bg = "transparent")
tmap_save(tm, "outputs/forest_change.pdf",
  height = 4, width = 5, bg = "transparent")


detach("package:tmap")

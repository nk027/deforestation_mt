
library("tmap")
library("sf")
library("dplyr")

map <- st_read("data/municipios") %>%
  mutate(mt = CD_GEOCMU > 5050000 & CD_GEOCMU < 5200000)
bra <- st_union(map)
mt <- st_union(map %>% filter(mt))
biome <- st_read("data/biomes_brazil") %>%
  select(name = Name) %>% st_transform(st_crs(mt))
biome_mt <- st_intersection(biome, mt)


cols <- c("#8dd3c7", "#ffffb3", "#e3e3e3", "#bebada")
cols <- c("#ccebc5", "#fbb4ae", "#decbe4", "#b3cde3")
names(cols) <- c("Amazônia", "Cerrado", "Other", "Pantanal")

tm <- tm_shape(bra) +
  tm_borders() +
  tm_shape(biome_mt) +
    tm_fill("name", alpha = 1, title = "Biome", legend.show = FALSE,
      palette = cols[-3]) +
  tm_shape(biome %>%
    mutate(name = ifelse(grepl("Cerr|Pant|Amaz", name), name, "Other"))) +
    tm_fill("name", alpha = 0.5, legend.show = FALSE,
      palette = cols) +
  tm_add_legend(type = "fill", size = 3,
    col = cols,
    labels = names(cols), title = "Biome") +
  tm_shape(mt) +
    tm_borders(lwd = 2) +
    tm_scale_bar(c(0, 500, 1000), position = "left", text.size = 1.2) +
  tm_layout(frame = FALSE,
    outer.margins = 0, bg.color = "transparent",
    legend.outside.position = "right", fontfamily = "Helvetica",
    legend.text.size = 1.2, legend.title.size = 1.8)

print(tm)

tmap_save(tm, "outputs/brazil_location.png",
  height = 5, width = 6, bg = "transparent")
tmap_save(tm, "outputs/brazil_location.pdf",
  height = 5, width = 6, bg = "transparent")

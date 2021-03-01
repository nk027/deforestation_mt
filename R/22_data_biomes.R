
# Dependencies ------------------------------------------------------------

stopifnot(
  file.exists("data/data.rds"),
  file.exists("data/biomes_brazil/bra_biomes.shp"),
  require(dplyr),
  require(sf)
)

data <- readRDS("data/data.rds")
biomes <- read_sf("data/biomes_brazil/")


# Add biomes --------------------------------------------------------------

biomes <- st_transform(biomes, crs = st_crs(data))
biomes <- biomes %>% transmute(biome = Name)
x <- data %>% filter(date == 2010) %>% select(code)

y <- st_intersection(x, biomes)

y <- y %>% mutate(area = st_area(y))

biome_mapping <- y %>% group_by(code) %>%
  filter(area == max(area)) %>% transmute(code, biome = as.factor(biome)) %>%
  st_drop_geometry()

data <- left_join(data, biome_mapping)

data <- data %>%
  mutate(
    biome_a = biome == "Amazônia",
    biome_c = biome == "Cerrado",
    biome_p = biome == "Pantanal")

# Store
saveRDS(data, "data/data.rds")


detach("package:dplyr")
detach("package:sf")

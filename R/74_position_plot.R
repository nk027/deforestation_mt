
# Dependencies ------------------------------------------------------------

stopifnot(
  require("tmap"),
  require("sf"),
  require("dplyr")
)

sirgas <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"


# Data --------------------------------------------------------------------

# Obtained from <gadm.org>
map_sa <- rbind(
  readRDS("data/maps/gadm36_ARG_0_sf.rds"),
  readRDS("data/maps/gadm36_BOL_0_sf.rds"),
  readRDS("data/maps/gadm36_BRA_0_sf.rds"),
  readRDS("data/maps/gadm36_CHL_0_sf.rds"),
  readRDS("data/maps/gadm36_COL_0_sf.rds"),
  readRDS("data/maps/gadm36_ECU_0_sf.rds"),
  readRDS("data/maps/gadm36_GUF_0_sf.rds"),
  readRDS("data/maps/gadm36_GUY_0_sf.rds"),
  readRDS("data/maps/gadm36_PRY_0_sf.rds"),
  readRDS("data/maps/gadm36_PER_0_sf.rds"),
  readRDS("data/maps/gadm36_SUR_0_sf.rds"),
  readRDS("data/maps/gadm36_URY_0_sf.rds"),
  readRDS("data/maps/gadm36_VEN_0_sf.rds")
) %>% st_transform(crs = sirgas)
# Municipality structure changed later on, we cannot use <gadm.org>
# map_bra <- readRDS("data/maps/gadm36_BRA_1_sf.rds") %>%
#   mutate(MT = NAME_1 == "Mato Grosso") %>%
#   st_transform(crs = sirgas)
# map_mt <- readRDS("data/maps/gadm36_BRA_2_sf.rds") %>%
#   filter(NAME_1 == "Mato Grosso") %>%
#   st_transform(crs = sirgas)
# Instead, we use the maps from <ftp://geoftp.ibge.gov.br/>
map_bra <- st_read("data/states") %>%
  mutate(MT = NM_ESTADO == "MATO GROSSO") %>%
  st_transform(crs = sirgas)
map_mt <- st_read("data/municipios") %>%
  filter(CD_GEOCMU > 5050000 & CD_GEOCMU < 5200000) %>%
  st_transform(crs = sirgas)
map_bio <- st_read("data/biomes_brazil") %>%
  select(name = Name) %>% st_transform(crs = sirgas) %>%
  mutate(name = ifelse(name == "Amazônia", "Amazon", name)) %>%
  mutate(name = ifelse(grepl("Cerr|Pant|Amaz", name), name, "Other"))


# Setup -------------------------------------------------------------------

bbx <- st_bbox(filter(map_bra))
bbx[3] <- bbx[3] - 5 # Cut out Atlantic islands
bbx_mt <- st_bbox(map_mt)

# cols <- c("#ccebc5", "#fbb4ae", "#eeeeee", "#b3cde3")
cols <- c("#16A455", "#DBAF02", "#eeeeee", "#2D4391")
names(cols) <- c("Amazon", "Cerrado", "Other", "Pantanal")


# Maps --------------------------------------------------------------------

p1 <- tm_shape(map_bio, bbox = bbx) +
  tm_graticules(labels.size = 1.2, lwd = 0.25, n.y = 3, n.x = 3) +
  tm_fill("name", legend.show = FALSE, palette = cols) +
  tm_shape(map_sa) + tm_borders(col = "#aaaaaa") +
  tm_shape(map_bra) + tm_borders(col = "#444444") +
  # tm_shape(st_as_sfc(bbx_mt - c(0.5, 0.5, -0.5, -0.25))) +
  #   tm_borders(col = "#444444", lwd = 2) +
  tm_compass(type = "8star", position = c("left", "bottom"), size = 4) +
  tm_scale_bar(c(0, 500, 1000),
    position = c("left", "bottom"), text.size = 1) +
  tm_add_legend(type = "fill", size = 3, title = "Biomes",
    col = cols[c(1, 2, 4, 3)], labels = names(cols[c(1, 2, 4, 3)])) +
  tm_layout(frame = TRUE, fontfamily = "Helvetica", # title = "Mato Grosso",
    title.position = c("right", "top"), title.size = 1.8,
    outer.bg.color = "transparent", bg.color = "white",
    inner.margins = c(0.01, 0.01, 0.01, 0.01),
    legend.position = c("right", "bottom"), legend.frame = TRUE,
    legend.text.size = 1.2, legend.title.size = 1.8)

tmap_save(p1, "outputs/brazil_1.png", dpi = 300,
  height = 2000, width = 2000, bg = "transparent")

p2i <- tm_shape(map_sa, bbox = bbx) +
    tm_graticules(labels.show = FALSE, lwd = 0.25, n.y = 4, n.x = 4) +
    tm_fill(col = "#eeeeee") + tm_borders(col = "#bbbbbb") +
  tm_shape(map_bra) +
    tm_fill("MT", palette = c("#cccccc", "#666666"), legend.show = FALSE) +
    tm_borders(col = "#888888") +
  tm_layout(frame = TRUE, fontfamily = "Helvetica",
    outer.bg.color = "transparent", bg.color = "white",
    inner.margins = c(0.01, 0.01, 0.01, 0.01))

vp2 <- viewport(x = 0.15, y = 0.2, width = 0.3, height = 0.3)

p2 <- tm_shape(st_intersection(map_bio, map_mt),
  bbox = bbx_mt - c(1.75, 0.25, -0.5, -0.25)) +
  tm_graticules(labels.size = 1.2, lwd = 0.25, n.y = 1, n.x = 3) +
  tm_fill("name", legend.show = FALSE, palette = cols) +
  tm_shape(map_mt) + tm_borders(col = "#cccccc") +
  tm_shape(map_bra) + tm_borders(col = "#444444") +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 3) +
  tm_scale_bar(c(0, 100, 200),
    position = c("right", "bottom"), text.size = 1) +
  tm_add_legend(type = "fill", size = 3, # title = "Biome",
    col = cols[c(1, 2, 4)], labels = names(cols[c(1, 2, 4)])) +
  tm_layout(frame = TRUE, main.title = "Biomes of Mato Grosso",
    main.title.position = c("right", "top"), main.title.size = 1.8,
    outer.bg.color = "transparent", bg.color = "white",
    fontfamily = "Helvetica",
    inner.margins = c(0.01, 0.01, 0.01, 0.01),
    legend.position = c("right", "top"), legend.frame = TRUE,
    legend.text.size = 1.2, legend.title.size = 1.8)

tmap_save(p2, "outputs/brazil_2.png", dpi = 300,
  height = 2000, width = 2200, bg = "transparent",
  insets_tm = list(p2i), insets_vp = list(vp2))


detach("package:tmap")
detach("package:sf")
detach("package:dplyr")

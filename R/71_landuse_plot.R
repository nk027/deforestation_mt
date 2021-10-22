
# Dependencies ------------------------------------------------------------

stopifnot(
  require("stars"),
  require("tmap"),
  require("dplyr")
)

# r01 <- raster("data/landsat/mt_2001_v3_1.tif")
# r17 <- raster("data/landsat/mt_2017_v3_1.tif")
r01 <- stars::read_stars("data/landsat/mt_goode01.tif") # Changed CRS with QGIS
r17 <- stars::read_stars("data/landsat/mt_goode17.tif") # Changed CRS with QGIS


# col_bs to apply to the plot --------------------------------------------

# full
col_a <- c("#b3cc33", "#be94e8", "#10773e", "#eeefce", "#e4a540", "#a4507d",
  "#c948a2", "#be5b1d", "#f09cde", "#877712", "#614040", "#1b5ee4", "#0cf8c1")
# less detail
col_b <- c("#EEEEEE", "#C18FE3", "#10773E", "#E8D313", "#C18FE3", "#C18FE3",
  "#C18FE3", "#C18FE3", "#C18FE3", "#C18FE3", "#EEEEEE", "#EEEEEE", "#EEEEEE")
names(col_a) <- names(col_b) <- c("cerrado", "cotton", "forest", "pasture",
  "soy-corn", "soy-cotton", "soy", "soy-millet", "soy-sunflower", "sugarcane",
  "urban", "water", "vegetation")


# Raster maps -------------------------------------------------------------

# Simplifed ---

# 2001
t1 <- tm_shape(r01, raster.downsample = FALSE) +
  tm_graticules(labels.size = 1.2, lwd = 0.25, n.y = 1, n.x = 1) +
  tm_raster(palette = col_b, as.count = TRUE,
    n = length(col_b), legend.show = FALSE) +
  tm_add_legend(type = "fill", size = 3,
    col = c("#10773E", "#E8D313", "#C18FE3", "#EEEEEE"),
    labels = c("Forest", "Pasture", "Croplands", "Other")) +
  tm_layout(frame = FALSE, fontfamily = "Helvetica", title = "2001",
    title.position = c("right", "top"), title.size = 2.4,
    title.fontface = "bold",
    outer.bg.color = "transparent", bg.color = "transparent",
    inner.margins = c(0.01, 0.01, 0.01, 0.01),
    legend.position = c("left", "bottom"), legend.frame = FALSE,
    legend.text.size = 1.2, legend.title.size = 1.8)

# 2017
t2 <- tm_shape(r17, raster.downsample = FALSE) +
  tm_graticules(labels.size = 1.2, lwd = 0.25, n.y = 1, n.x = 1) +
  tm_raster(palette = col_b, as.count = TRUE,
    n = length(col_b), legend.show = FALSE) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 3) +
  tm_scale_bar(c(0, 100, 200),
    position = c("right", "bottom"), text.size = 1) +
  tm_layout(frame = FALSE, fontfamily = "Helvetica", title = "2017",
    title.position = c("right", "top"), title.size = 2.4,
    title.fontface = "bold",
    outer.bg.color = "transparent", bg.color = "transparent",
    inner.margins = c(0.01, 0.01, 0.01, 0.01),
    legend.position = c("left", "bottom"), legend.frame = FALSE,
    legend.text.size = 1.2, legend.title.size = 1.8)

tmap_save(t1, "outputs/land_use_01.png", dpi = 300,
  height = 2000, width = 2000, bg = "transparent")
tmap_save(t2, "outputs/land_use_02.png", dpi = 300,
  height = 2000, width = 2000, bg = "transparent")

detach("package:stars")
detach("package:tmap")
detach("package:dplyr")


# Dependencies ------------------------------------------------------------

stopifnot(
  require("raster"),
  require("sf")
)

# r01 <- raster("data/landsat/mt_2001_v3_1.tif")
r01 <- raster("data/landsat/mt_goode01.tif") # Changed with QGIS
# r17 <- raster("data/landsat/mt_2017_v3_1.tif")
r17 <- raster("data/landsat/mt_goode17.tif") # Changed with QGIS
shp <- rgdal::readOGR("data/municipios/")


# Colours to apply to the plot --------------------------------------------

colour <- c("#b3cc33", "#be94e8", "#10773e", "#eeefce",
            "#e4a540", "#a4507d", "#c948a2", "#be5b1d",
            "#f09cde", "#877712",
            "#614040", "#1b5ee4", "#0cf8c1")
names(colour) <- c("cerrado", "cotton", "forest", "pasture",
                   "soy-corn", "soy-cotton", "soy", "soy-millet",
                   "soy-sunflower", "sugarcane",
                   "urban", "water", "vegetation")

# less detail
colour <- c("#b3cc33", "#e4a540", "#10773e", "#eeefce",
            "#e4a540", "#e4a540", "#e4a540", "#e4a540",
            "#e4a540", "#e4a540",
            "#dddddd", "#dddddd", "#dddddd")
# least detail
colour <- c("#EEEEEE", "#C18FE3", "#10773E", "#E8D313",
            "#C18FE3", "#C18FE3", "#C18FE3", "#C18FE3",
            "#C18FE3", "#C18FE3",
            "#EEEEEE", "#EEEEEE", "#EEEEEE")

# Helper to create legend
view_col <- function(x) {
  barplot(
    rep(1, length(x)), col = rev(x),
    space = 0.1, axes = FALSE,
    names.arg = if(is.null(names(x))) {rev(x)} else {rev(names(x))},
    cex.names = 1.8, horiz = TRUE, las = 1
  )
}

png("outputs/colour.png", width = 200, height = 800)
op <- par(mar = c(2, 12, 2, 0.5))
view_col(colour)
par(op)
dev.off()


# Raster ------------------------------------------------------------------

png("outputs/2001.png", width = 1600, height = 1440, bg = "transparent")
op <- par(mar = c(2, 2, 2, 0.5))
plot(r01, col = colour, axes = FALSE, ann = FALSE, legend = FALSE, box = FALSE)
par(op)
dev.off()

png("outputs/2017.png", width = 1600, height = 1440, bg = "transparent")
op <- par(mar = c(2, 2, 2, 0.5))
plot(r17, col = colour, axes = FALSE, ann = FALSE, legend = FALSE, box = FALSE)
par(op)
dev.off()


# Legend ------------------------------------------------------------------

library("cowplot")
library("ggplot2")
library("grid")
library("gridExtra")

df <- data.frame(
  Class = factor(x = c("Forest", "Pasture", "Croplands", "Other"),
                 levels = c("Forest", "Pasture", "Croplands", "Other")),
  Colour = c("#10773e", "#E8D313", "#C18FE3", "#eeeeee"),
  Value = 1:4,
  stringsAsFactors = FALSE)

(gg_legend <- ggplot(df, aes(Value, fill = Class)) +
  geom_bar() +
  scale_fill_manual(values = df$Colour) +
    theme_bw(base_family = "Arial") +
    theme(rect = element_rect(fill = "transparent")))

# Using the cowplot package
legend <- cowplot::get_legend(gg_legend)

grid.newpage()
grid.draw(legend)
ggsave("outputs/legend.png", legend, width = 2, height = 2, bg = "transparent")


detach("package:raster")
detach("package:sf")
detach("package:cowplot")
detach("package:ggplot2")
detach("package:grid")
detach("package:gridExtra")


# Update ---

library("stars")
library("tmap")
library("dplyr")
library("sf")

r01 <- stars::read_stars("data/landsat/mt_goode01.tif") # Changed with QGIS
r17 <- stars::read_stars("data/landsat/mt_goode17.tif") # Changed with QGIS

# Full colours
colour <- c("#b3cc33", "#be94e8", "#10773e", "#eeefce",
            "#e4a540", "#a4507d", "#c948a2", "#be5b1d",
            "#f09cde", "#877712",
            "#614040", "#1b5ee4", "#0cf8c1")
# Collapsed
colour <- c("#EEEEEE", "#C18FE3", "#10773E", "#E8D313",
            "#C18FE3", "#C18FE3", "#C18FE3", "#C18FE3",
            "#C18FE3", "#C18FE3",
            "#EEEEEE", "#EEEEEE", "#EEEEEE")

names(colour) <- c("cerrado", "cotton", "forest", "pasture",
            "soy-corn", "soy-cotton", "soy", "soy-millet",
            "soy-sunflower", "sugarcane",
            "urban", "water", "vegetation")


t1 <- tm_shape(r01, raster.downsample = FALSE) +
  tm_graticules(labels.size = 1.2, lwd = 0.25, n.y = 1, n.x = 1) +
  tm_raster(palette = colour, as.count = TRUE,
    n = length(colour), legend.show = FALSE) +
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

t2 <- tm_shape(r17, raster.downsample = FALSE) +
  tm_graticules(labels.size = 1.2, lwd = 0.25, n.y = 1, n.x = 1) +
  tm_raster(palette = colour, as.count = TRUE,
    n = length(colour), legend.show = FALSE) +
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

# Try adding municipality borders
map_mt <- st_read("data/municipios") %>%
  filter(CD_GEOCMU > 5050000 & CD_GEOCMU < 5200000) %>%
  st_transform(crs = crs(r01))

t3 <- tm_shape(r17, raster.downsample = FALSE) +
  # tm_graticules(labels.size = 1.2, lwd = 0.25, n.y = 1, n.x = 1) +
  tm_raster(palette = colour, n = length(colour), legend.show = FALSE) +
  tm_shape(map_mt) + tm_borders(col = "#444444") +
  tm_layout(frame = FALSE, fontfamily = "Helvetica",
    title.position = c("right", "top"), title.size = 2.4,
    title.fontface = "bold",
    outer.bg.color = "transparent", bg.color = "transparent",
    inner.margins = c(0.01, 0.01, 0.01, 0.01),
    legend.position = c("left", "bottom"), legend.frame = FALSE,
    legend.text.size = 1.2, legend.title.size = 1.8)

tmap_save(t3, "outputs/land_use_mess1.png", dpi = 300,
  height = 1000, width = 1000, bg = "transparent")

t4 <- tm_shape(r17, raster.downsample = FALSE) +
  # tm_graticules(labels.size = 1.2, lwd = 0.25, n.y = 1, n.x = 1) +
  tm_raster(palette = colour, n = length(colour), legend.show = FALSE) +
  tm_shape(map_mt) + tm_borders(col = "#cccccc") +
  tm_layout(frame = FALSE, fontfamily = "Helvetica",
    title.position = c("right", "top"), title.size = 2.4,
    title.fontface = "bold",
    outer.bg.color = "transparent", bg.color = "transparent",
    inner.margins = c(0.01, 0.01, 0.01, 0.01),
    legend.position = c("left", "bottom"), legend.frame = FALSE,
    legend.text.size = 1.2, legend.title.size = 1.8)

tmap_save(t4, "outputs/land_use_mess2.png", dpi = 300,
  height = 1000, width = 1000, bg = "transparent")


library("raster")
library("sf")


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

png("plots/colour.png", width = 200, height = 800)
op <- par(mar = c(2, 12, 2, 0.5))
view_col(colour)
par(op)
dev.off()


# Raster ------------------------------------------------------------------

r01 <- raster("data/landsat/mt_2001_v3_1.tif")
r17 <- raster("data/landsat/mt_2017_v3_1.tif")
shp <- rgdal::readOGR("data/municipios/")

png("plots/2001.png", width = 1600, height = 1440)
op <- par(mar = c(2, 2, 2, 0.5))
plot(r01, col = colour_final, axes = FALSE, ann = FALSE, legend = FALSE)
par(op)
dev.off()

png("plots/2017.png", width = 1600, height = 1440)
op <- par(mar = c(2, 2, 2, 0.5))
plot(r17, col = colour_final, axes = FALSE, ann = FALSE, legend = FALSE)
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
ggsave("plots/legend.png", legend, width = 2, height = 2, bg = "transparent")


detach("package:raster")
detach("package:sf")
detach("package:cowplot")
detach("package:ggplot2")
detach("package:grid")
detach("package:gridExtra")

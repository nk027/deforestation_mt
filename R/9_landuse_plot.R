library(raster)
library(sf)

colour <- c("#b3cc33", "#be94e8", "#10773e", "#eeefce",
            "#e4a540", "#a4507d", "#c948a2", "#be5b1d",
            "#f09cde", "#877712",
            "#614040", "#1b5ee4", "#0cf8c1")
colour_collapsed <- c("#b3cc33", "#e4a540", "#10773e", "#eeefce",
                      "#e4a540", "#e4a540", "#e4a540", "#e4a540",
                      "#e4a540", "#e4a540",
                      "#dddddd", "#dddddd", "#dddddd")
names(colour) <- c("cerrado", "cotton", "forest", "pasture",
                   "soy-corn", "soy-cotton", "soy", "soy-millet",
                   "soy-sunflower", "sugarcane",
                   "urban", "water", "vegetation")

view_col <- function(x) {
  #par(mai = c(0.2, max(strwidth(x, "inch") + 0.4, na.rm = TRUE), 0.2, 0.4))
  barplot(
    rep(1, length(x)), col = rev(x),
    space = 0.1, axes = FALSE,
    names.arg = if(is.null(names(x))) {rev(x)} else {rev(names(x))},
    cex.names = 1.8, horiz = TRUE, las = 1
  )
}

png("colour.png", width = 200, height = 800)
op <- par(mar = c(2, 12, 2, 0.5))
view_col(colour)
par(op)
dev.off()


r <- raster("~/Dokumente/msc/mt_landuse/mt_2001_v3_1.tif")
# r <- raster("~/Dokumente/msc/mt_landuse/mt_2017_v3_1.tif")
shp <- rgdal::readOGR("~/Dokumente/msc/mt_municipios_2014")

png("2001.png", width = 800, height = 800)
# png("2017.png", width = 800, height = 800)
op <- par(mar = c(2, 2, 2, 0.5))
plot(r, col = colour_collapsed, axes = FALSE, ann = FALSE, legend = FALSE)
par(op)
dev.off()


# Legend ------------------------------------------------------------------

library(cowplot)
library(ggplot2) 
library(grid)
library(gridExtra) 

df <- data.frame(
  Class = factor(x = c("Forest", "Pasture", "Cerrado", "Croplands", "Other"),
                 levels = c("Forest", "Pasture", "Cerrado", "Croplands", "Other")),
  Colour = c("#10773e", "#eeefce", "#b3cc33", "#e4a540", "#dddddd"),
  Value = 1:5,
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
ggsave("plots/legend.png", legend, width = 2, height = 4, bg = "transparent")

library(raster)

r <- raster("~/Dokumente/msc/iiasa/raster_simU/")
shp <- rgdal::readOGR("~/Dokumente/msc/br_municipios_2017/")

shp$CD_GEOCMU <- as.integer(as.character(shp$CD_GEOCMU))
shp <- subset(shp, shp$CD_GEOCMU > 5050000, drop = TRUE)
shp <- subset(shp, shp$CD_GEOCMU < 5200000, drop = TRUE)

extr <- extract(r, shp, df = TRUE, weights = TRUE)


library(raster)
library(dplyr)


# Raster ------------------------------------------------------------------

r <- raster("data/iiasa/raster/")
shp <- rgdal::readOGR(dsn = "data/municipios")

shp$CD_GEOCMU <- as.integer(as.character(shp$CD_GEOCMU))
# Limit to Mato Grosso
shp <- subset(shp, shp$CD_GEOCMU > 5050000, drop = TRUE)
shp <- subset(shp, shp$CD_GEOCMU < 5200000, drop = TRUE)

extr <- extract(r, shp, df = TRUE, weights = TRUE)

# Takes a bit
saveRDS(extr, "data/geo/geo_iiasa_raw.rds")


# Restructure -------------------------------------------------------------

extr <- readRDS("data/geo/geo_iiasa_raw.rds")

extr_df <- extr %>% group_by(ID, raster) %>% 
  summarise(sum(weight))
names(extr_df) <- c("id", "simu", "weight")

extr_df$code <- shp$CD_GEOCMU[extr_df$id]
extr_df$id <- NULL

saveRDS(extr_df, "data/geo/geo_iiasa.rds")

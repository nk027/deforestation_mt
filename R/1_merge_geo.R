library(dplyr)

crs_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

tifs <- list.files("data/landsat", "[.]tif$")

# sp for raster::extract to work
shp <- rgdal::readOGR(dsn = "data/municipios")
shp <- sp::spTransform(shp, crs_sin)

extr_vals <- vector("list", length(tifs))
for(i in seq_along(tifs)) {
  r <- raster::raster(paste0("data/landsat/", tifs[i]))
  extr_vals[[i]] <- raster::extract(r, shp, df = TRUE)
}



# 
# # continue with sf
# shp <- sf::st_read(dsn = "data/municipios")
# shp <- sf::st_transform(shp, crs = crs_sin)
# 
# # cut down to non NAs with more than 1000 tiles inside
# extr <- extr_raw[!is.na(extr_raw[2]), ]
# id_matches <- extr %>% group_by(ID) %>% 
#   count() %>% 
#   dplyr::filter(n > 1000) %>% 
#   dplyr::select(ID)
# extr <- extr %>% dplyr::filter(ID %in% id_matches$ID)
# 
# # filter the shapefile
# shp <- shp[id_matches$ID, ]
# shp$ID <- id_matches$ID
# 
# # add a name to the factor
# extr$mt_2005 <- factor(x = extr$mt_2005, 
#                        labels = c("cerrado", "fallow_cotton", "forest", 
#                                   "pasture", "soy_corn", "coy_cotton",
#                                   "soy_fallow", "soy_millet", "soy_sunflower",
#                                   "sugarcane", "urban", "water"))
# 
# extr %>% count(ID, mt_2005)

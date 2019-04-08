
library(dplyr)
library(sf)

crs_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"


# Join shapefile and extracted values -------------------------------------

shp <- read_sf("data/municipios/") %>% 
  transmute(name = NM_MUNICIP, code = as.integer(CD_GEOCMU)) %>% 
  st_transform(crs_sin)

shp$area <- st_area(shp)

geo_df <- readRDS("data/geo/geo_df_long.rds")
geo_df$code <- shp$code[geo_df$id]

shp <- shp %>%
  right_join(geo_df, by = "code")

saveRDS(shp, "data/geo/shp.rds")

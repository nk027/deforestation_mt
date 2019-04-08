
library(dplyr)
library(sf)

crs_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"


# Join shapefile and extracted values -------------------------------------

shp <- read_sf("data/municipios/") %>% 
  transmute(code = as.integer(CD_GEOCMU)) %>% 
  st_transform(crs_sin)
shp$area_m2 <- as.numeric(st_area(shp))

geo_df <- readRDS("data/geo/geo_df_long.rds")
geo_df$code <- shp$code[geo_df$id]
geo_df <- geo_df %>% 
  transmute(code, date, 
            forest_px = forest, pasture_px = pasture, 
            cott_px = fallow_cotton, soycorn_px = soy_corn, 
            soycott_px = soy_cotton, soy_px = soy_fallow,
            soymill_px = soy_millet, soysunfl_px = soy_sunflower, 
            sugar_px = sugarcane, cerr_px = cerrado,
            urban_px = urban, water_px = water,
            veg_px = sec_veg)


shp <- shp %>%
  right_join(geo_df, by = "code")

saveRDS(shp, "data/geo/shp.rds")

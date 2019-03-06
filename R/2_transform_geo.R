library(dplyr)

crs_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
crs_sirgas <- "+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

shp <- sf::read_sf("data/municipios/") %>% 
  transmute(name = NM_MUNICIP, code = as.integer(CD_GEOCMU))

geo_df <- readRDS("data/geo/geo_merged_df_date.rds")
geo_df$code <- shp$code[geo_df$id]

shp <- sf::st_transform(shp, crs_sin) # use sinusoidal projection

shp <- shp %>%
  filter(code %in% geo_df$code)
shp$area <- sf::st_area(shp)


pop <- readRDS("data/sidra/pop_long.rds")
gdp <- readRDS("data/sidra/gdp_long.rds")
crop <- readRDS("data/sidra/crop_long.rds")

x <- full_join(gdp, pop, by = c("date", "code"))
y <- full_join(x, crop, by = c("date", "code"))
df <- full_join(y, geo_df, by = c("code", "date")); rm(x, y)
df <- as_tibble(df)

shp_df <- full_join(shp, df, by = "code")

saveRDS(shp_df, "data/data.rds")

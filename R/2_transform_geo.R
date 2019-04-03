library(dplyr)

crs_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

shp <- sf::read_sf("data/municipios/") %>% 
  transmute(name = NM_MUNICIP, code = as.integer(CD_GEOCMU))

geo_df <- readRDS("data/geo/geo_merged_df_date.rds")
geo_df$code <- shp$code[geo_df$id]

shp <- sf::st_transform(shp, crs_sin) # use sinusoidal projection

shp <- shp %>%
  filter(code %in% geo_df$code)
shp$area <- sf::st_area(shp)

pop <- readRDS("data/sidra/pop_long_full.rds")
gdp <- readRDS("data/sidra/gdp_long.rds")
crop <- readRDS("data/sidra/crop_long.rds")
forestry <- readRDS("data/sidra/forestry_long.rds")

x <- full_join(gdp, pop, by = c("date", "code"))
y <- full_join(x, crop, by = c("date", "code"))
z <- full_join(y, forestry, by = c("date", "code"))
df <- full_join(z, geo_df, by = c("code", "date")); rm(x, y, z)
df <- as_tibble(df)

shp_df <- full_join(shp, df, by = "code")

saveRDS(shp_df, "data/data.rds")

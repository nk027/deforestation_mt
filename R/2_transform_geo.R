library(dplyr)

shp <- sf::read_sf("data/municipios/") %>% 
  transmute(name = NM_MUNICIP, code = as.integer(CD_GEOCMU))

geo_df <- readRDS("data/geo/geo_merged_df_date.rds")
geo_df$code <- shp$code[geo_df$id]

pop <- readRDS("data/sidra/pop_long.rds")
gdp <- readRDS("data/sidra/gdp_long.rds")
crop <- readRDS("data/sidra/crop_long.rds")

x <- full_join(gdp, pop, by = c("date", "code"))
y <- full_join(x, crop, by = c("date", "code"))
df <- full_join(y, geo_df, by = c("code", "date")); rm(x, y)


shp2 <- right_join(shp, geo_merged_df, by = c("CD_GEOCMU" = "code"))

shp2 %>% 
  select(y01_for, y10_for, y17_for) %>% 
  plot()

mt <- which(as.integer(shp$CD_GEOCMU) %in% gdp$id)
shp[mt, ] %>% select(NM_MUNICIP) %>% 
  plot(label = NM_MUNICIP)

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
#                                   "pasture", "soy_corn", "soy_cotton",
#                                   "soy_fallow", "soy_millet", "soy_sunflower",
#                                   "sugarcane", "urban", "water"))
# 
# extr %>% count(ID, mt_2005)

library(dplyr)

shp <- sf::read_sf("data/municipios/") %>% 
  transmute(name = NM_MUNICIP, code = as.integer(CD_GEOCMU))

geo_df <- readRDS("data/geo/geo_merged_df_date.rds")
geo_df$code <- shp$code[geo_df$id]

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

centr <- st_centroid(shp_df)
centr <- cbind(centr, st_coordinates(st_centroid(shp_df$geometry)))

library(ggplot2)

shp_df %>% 
  filter(date %in% c(2005)) %>% 
  mutate(gdp_cap = gdp / pop, pop_dens = pop / area, date) %>% 
  ggplot() +
  geom_sf(aes(fill = gdp)) +
#  geom_text(data = centr, aes(x = X, y = Y, label = name),
#            color = "gray", fontface = "bold", check_overlap = TRUE) +
  theme_minimal() 

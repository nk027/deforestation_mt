shp <- readRDS("data/data.rds")

shp %>% filter(date == 2015) %>% 
  group_by(code) %>%
  transmute(v = as.double(area), 
            r1 = (forest + cerrado + pasture + urban + water + crop) / 16 * 1e6,
            r2 = (forest + cerrado + pasture + urban + water + crop) / (1000 / 231.6564) ^ 2 * 1e6) %>% 
  mutate(x = v / r2)

r <- raster::raster("data/landsat/mt_2015_v3.tif")
raster::res(r)

(1000 / 231.6564) ^ 2

data %>% 
  transmute((forest_px + pasture_px + cerr_px + crop_px + urban_px + water_px) * 
              (231.6564) ^ 2 / 1e6,
            area_km2)

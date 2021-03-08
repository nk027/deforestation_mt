
library("sf")
library("dplyr")
library("ggspatial")

x <- st_read("/home/luckeneder/mining_geographies/input/WDPA/mt.shp")

y <- readRDS("data/data.rds")
y <- y[y$date == 2010, ]
x <- st_transform(x, crs = st_crs(y))

# plot(select(x, STATUS_YR))
# plot(st_geometry(y), add = TRUE)
# plot(select(filter(x, STATUS_YR > 2005), STATUS_YR))
# plot(st_geometry(y), add = TRUE)

plot(select(filter(x, STATUS_YR > 2005), STATUS_YR))
plot(st_geometry(x), add = TRUE)

x %>%
  transmute(STATUS_YR = ifelse(STATUS_YR > 2005, TRUE, FALSE)) %>%
  plot()

plot(world)

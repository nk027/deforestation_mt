library(dplyr)
library(sf)

crs_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

rdata <- list.files("data/mining")
rdata <- rdata[grep("[.]rdata$", rdata)]
names <- sub("^([^_\.]*)(.*)", "\\1", rdata)

mining <- list()
for(i in seq_along(rdata)) {
  load(paste0("data/mining/", rdata[i]))
  mining[[i]] <- x
}
names(mining) <- names
rm(x)
library(dplyr)

shp <- sf::read_sf("data/mining/S.gpkg") %>% 
  filter(country == "Brazil")

shp <- sf::st_transform(shp, crs_sin)

data <- readRDS("data/data.rds") %>% 
  filter(date == 2010) %>% 
  select(code)

mato <- sf::st_intersection(shp, data)
# n = 87

out <- sapply(mining, function(x) {
  y <- vector("numeric", nrow(mato))
  for(i in seq_along(nrow(mato)))
    y[i] <- sum(x %>% filter(snl_id == mato$snl_id[i]) %>% .$value, na.rm = TRUE)
  sum(y)
})

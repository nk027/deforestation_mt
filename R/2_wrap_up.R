
library(dplyr)
library(sf)


# Wrap up all data---------------------------------------------------------

shp <- readRDS("data/geo/shp.rds")
tab <- readRDS("data/tab/tab.rds")

data <- full_join(shp, tab, by = c("code", "date"))
saveRDS(data, "data/data.rds")

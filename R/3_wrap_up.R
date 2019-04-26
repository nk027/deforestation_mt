
library(dplyr)
library(sf)


# Wrap up all data---------------------------------------------------------

shp <- readRDS("data/geo/shp.rds")
tab <- readRDS("data/tab/tab.rds")
spei <- readRDS("data/geo/spei.rds")
iiasa <- readRDS("data/geo/iiasa.rds")

data <- full_join(shp, tab, by = c("code", "date")) %>% 
  full_join(spei, by = c("code", "date")) %>% 
  full_join(iiasa, by = c("code", "date"))

refcols <- c("code", "name", "date")
data <- data[, c(refcols, setdiff(names(data), refcols))]
data <- data %>% ungroup()

saveRDS(data, "data/data.rds")

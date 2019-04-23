library(raster)
library(dplyr)
library(readr)


# Raster ------------------------------------------------------------------

r <- raster("data/other/raster/")
shp <- rgdal::readOGR(dsn = "data/municipios")

shp$CD_GEOCMU <- as.integer(as.character(shp$CD_GEOCMU))
shp <- subset(shp, shp$CD_GEOCMU > 5050000, drop = TRUE)
shp <- subset(shp, shp$CD_GEOCMU < 5200000, drop = TRUE)

extr <- extract(r, shp, df = TRUE, weights = TRUE)

data <- extr %>% group_by(ID, raster) %>% 
  summarise(sum(weight))
names(data) <- c("id", "simu", "weight")

data$code <- shp$CD_GEOCMU[extr$id]


# CSV ---------------------------------------------------------------------

yields <- read_csv("data/other/yields.csv")
# SS - subsistence farm, LI - low input, HI - high input, IR - irrigated
# Relevant crops from IBGE:
# Rice, Sugarcane, Manioc, Corn, Beans, Sunflower, Sorghum, Cotton, Soy
grep("^BeaD..", names(yields))
grep("^Rice..", names(yields))
grep("^Srgh..", names(yields))
grep("^SugC..", names(yields))
grep("^Sunf..", names(yields))
grep("^Soya..", names(yields))

data <- left_join(data, yields, by = c("simu" = "SIMUID"))

time <- read_csv("data/other/acc_travel_min.csv")

data <- left_join(data, time, by = c("simu" = "VALUE"))

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

extr_df <- extr %>% group_by(ID, raster) %>% 
  summarise(sum(weight))
names(extr_df) <- c("id", "simu", "weight")

extr_df$code <- shp$CD_GEOCMU[extr$id]


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


time <- read_csv("data/other/acc_travel_min.csv")

data <- extr_df %>% 
  left_join(yields, by = c("simu" = "SIMUID")) %>% 
  left_join(time, by = c("simu" = "VALUE"))

# Some NAs for yields, fewer for travel time
data[is.na(data$MEANYLD), ]$weight
data[is.na(data$MEAN1), ]$weight
# Remove NAs
data <- data[!is.na(data$MEANYLD) & !is.na(data$MEAN1), ]
# Recalculate weights
reweigh <- data %>% group_by(code) %>% 
  summarise(reweigh = sum(weight))
data_wt <- left_join(data, reweigh, by = "code") %>% 
  mutate(reweight = weight / reweigh)
# Check
data_wt %>% group_by(code) %>% 
  summarise(check = round(sum(reweight), 1)) %>% .$check == 1

# Aggregate and finalise
data_wt %>% 
  transmute(code, yield = reweight * MEANYLD, travel = reweight * MEAN1) %>% 
  group_by(code) %>% 
  summarise(yield = sum(yield), travel = sum(travel)) %>% 
  saveRDS("data/other/iiasa.rds")

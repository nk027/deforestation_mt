
library(raster)

tif <- "data/deforestation/curtis2018.tif"
if(!file.exists(tif)) {
download.file(
  "https://science.sciencemag.org/highwire/filestream/715492/field_highwire_adjunct_files/2/aau3445-Data-S3.tif",
  tif)
}

# Taken from supplementary code
classification <- c(
  "minor_loss" = 0,
  "deforestation" = 1,
  "shifting_agriculture" = 2,
  "forestry" = 3,
  "wildfire" = 4,
  "urban" = 5
)


# Check using the land use cover raster -----------------------------------

defore <- raster(tif)
mt <- raster("data/landsat/mt_2017_v3_1.tif")
crs(defore) <- crs(mt)

intersection <- raster::intersect(defore, mt)
hist(values(intersection))


# Check using the municipality shapefile ----------------------------------

library(sf)
library(dplyr)

shp <- read_sf("data/municipios/") %>% 
  transmute(id = as.integer(CD_GEOCMU)) %>% 
  filter(id > 5100000 & id < 5200000)

# unweighted
extracted <- raster::extract(defore, shp, df = TRUE)
extracted$curtis2018 <- factor(extracted$curtis2018, 
                               labels = names(classification))


loss <- extracted$curtis2018[!is.na(extracted$curtis2018)]
# 7 NAs

plot(loss)
summary(loss)

sum(loss == "deforestation") / length(loss)
# 0.73
sum(loss %in% c("deforestation", "shifting_agriculture")) / length(loss)
# 0.83

sum(loss == "deforestation") / 
  (length(loss) - sum(loss == "minor_loss"))
# 0.84
sum(loss %in% c("deforestation", "shifting_agriculture")) / 
  (length(loss) - sum(loss == "minor_loss"))
# 0.95

# weighted
extracted <- raster::extract(defore, shp, df = TRUE, weight = TRUE)
extracted$curtis2018 <- factor(extracted$curtis2018, 
                               labels = names(classification))

loss <- extracted %>% 
  filter(!is.na(curtis2018)) %>% 
  group_by(curtis2018) %>% 
  tally(wt = weight)

loss[2, 2] / sum(loss[, 2])
# 0.68
sum(loss[2:3, 2]) / sum(loss[, 2])
# 0.81

loss[2, 2] / sum(loss[-1, 2])
# 0.78
sum(loss[2:3, 2]) / sum(loss[-1, 2])
# 0.92

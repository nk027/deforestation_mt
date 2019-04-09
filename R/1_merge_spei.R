
library(ncdf4)


# Examine NCDF file & get layers ------------------------------------------

ncdf <- paste0("data/spei/", list.files("data/spei/", "12[.]nc$"))

nc <- ncdf4::nc_open(ncdf)
time <- ncvar_get(nc, "time")
time_units <- ncatt_get(nc, "time", "units")
ncdf4::nc_close(nc)

dates <- as.Date(gsub("^.*([0-9]{4}-[0-9]{2}-[0-9]{2}).*$", 
                      "\\1", time_units[[2]])) + time
bands <- seq(which(dates == "2000-01-01"), which(dates == "2018-01-01"))


# Extract (computationally intensive) -------------------------------------

shp <- rgdal::readOGR(dsn = "data/municipios")
# Subset to MT (code 5100102:5108956)
shp$CD_GEOCMU <- as.integer(as.character(shp$CD_GEOCMU))
shp <- subset(shp, shp$CD_GEOCMU > 5050000, drop = TRUE)
shp <- subset(shp, shp$CD_GEOCMU < 5200000, drop = TRUE)
# shp <- subset(shp, shp$UF == "MT") # 

library(parallel)
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
start <- Sys.time()
extr_spei <- parLapply(cl, bands, function(x, ncdf, shp) {
  raster::extract(raster::raster(ncdf, band = x), shp, 
                  df = TRUE, weights = TRUE)
}, ncdf, shp)
cat("Calculation finished after", format(Sys.time() - start), "\n")
stopCluster(cl)

names(extr_spei) <- dates[bands]
saveRDS(extr_spei, "data/geo/spei.rds")

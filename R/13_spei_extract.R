
timescale <- "03"


# Dependencies ------------------------------------------------------------

stopifnot(
  file.exists(paste0("data/spei/spei", timescale, ".nc")),
  length(list.files("data/municipios", "[.]shp$")) > 0,
  nzchar(system.file(package = "ncdf4")),
  nzchar(system.file(package = "rgdal")),
  nzchar(system.file(package = "raster"))
)


# Examine NCDF file & get layers ------------------------------------------

ncdf <- paste0("data/spei/spei", timescale, ".nc")

nc <- ncdf4::nc_open(ncdf)
time <- ncdf4::ncvar_get(nc, "time")
time_units <- ncdf4::ncatt_get(nc, "time", "units")
ncdf4::nc_close(nc)

dates <- as.Date(gsub("^.*([0-9]{4}-[0-9]{2}-[0-9]{2}).*$", 
                      "\\1", time_units[[2]])) + time
bands <- seq(which(dates == "2000-01-01"), which(dates == "2018-01-01"))


# Prep SHP ----------------------------------------------------------------

shp <- rgdal::readOGR(dsn = "data/municipios")

# Subset to MT (code 5100102:5108956)
shp$CD_GEOCMU <- as.integer(as.character(shp$CD_GEOCMU))
shp <- subset(shp, shp$CD_GEOCMU > 5050000, drop = TRUE)
shp <- subset(shp, shp$CD_GEOCMU < 5200000, drop = TRUE)


# Extract (computationally intensive) -------------------------------------

# Read and merge NCDF bands
if(require("parallel")) {
  # Do it in parallel
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  start <- Sys.time()
  extr_spei <- parLapply(cl, bands, function(x, ncdf, shp) {
    raster::extract(raster::raster(ncdf, band = x), shp, 
                    df = TRUE, weights = TRUE)
  }, ncdf, shp)
  cat("Calculation finished after", format(Sys.time() - start), "\n")
  stopCluster(cl)
  detach("package:parallel")
} else {
  # Pure iterative approach
  extr_spei <- vector("list", length(bands))
  for(i in seq_along(bands)) {
    r <- raster::raster(ncdf, band = bands[i])
    extr_spei[[i]] <- raster::extract(r, shp, df = TRUE, weights = TRUE)
    cat("Extracted values ", i, " of ", length(bands), ".", sep = "")
  }
}
names(extr_spei) <- dates[bands]

# Store list of extracted values
saveRDS(extr_spei, paste0("data/geo/geo_spei_", timescale, "_raw.rds"))

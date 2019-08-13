
# Dependencies ------------------------------------------------------------

stopifnot(
  length(list.files("data/landsat", "[.]tif$")) > 0,
  length(list.files("data/municipios", "[.]shp$")) > 0,
  nzchar(system.file(package = "rgdal")),
  nzchar(system.file(package = "sp")),
  nzchar(system.file(package = "raster"))
)


# Merge vector & raster data ----------------------------------------------

tifs <- paste0("data/landsat/", list.files("data/landsat", "[.]tif$"))
shp <- rgdal::readOGR(dsn = "data/municipios")
shp <- sp::spTransform(
  shp, 
  "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
) # raster::extract only works with sp, not sf


# Extract (computationally intensive) -------------------------------------

# Read and merge TIFs 
if(require("parallel")) {
  # Do it in parallel
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  start <- Sys.time()
  extr_vals <- parLapply(cl, tifs, function(x, shp) {
    raster::extract(raster::raster(x), shp, df = TRUE)
  }, shp)
  cat("Calculation finished after", format(Sys.time() - start), "\n")
  stopCluster(cl)
  detach("package:parallel")
} else {
  # Pure iterative approach
  extr_vals <- vector("list", length(tifs))
  for(i in seq_along(tifs)) {
    r <- raster::raster(paste0("data/landsat/", tifs[i]))
    extr_vals[[i]] <- raster::extract(r, shp, df = TRUE)
  }
}
names(extr_vals) <- paste0("y", formatC(1:17, width = 2, flag = "0"))

# Store list of extracted values
saveRDS(extr_vals, "data/geo/geo_extract_raw.rds")

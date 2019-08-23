
timescale <- "03"


# Dependencies ------------------------------------------------------------

stopifnot(
  length(list.files("data/municipios", "[.]shp$")) > 0,
  require("dplyr"),
  nzchar(system.file(package = "reshape2"))
)
if(!exists("extr_spei")) {
  extr_spei <- readRDS(paste0("data/geo/geo_spei_", timescale, "_raw.rds"))
}


# Get IDs from SHP --------------------------------------------------------

shp <- rgdal::readOGR(dsn = "data/municipios")

# Subset to MT (code 5100102:5108956)
code <- as.integer(as.character(shp$CD_GEOCMU))
code <- code[code > 5050000 & code < 5200000]


# Transform raw extracted values ------------------------------------------

spei <- sapply(extr_spei, function(x)  {
  x$id <- x[[1]]
  x$value <- x[[2]] * x[[3]] # Weights
  aggregate(value ~ id, data = x, sum)[[2]]
})

spei <- data.frame(code = code, spei)
colnames(spei)[-1] <- gsub("^.([0-9]{4})[.]([0-9]{2})[.]..$", 
                           "\\1-\\2", colnames(spei)[-1])

df_spei <- as_tibble(reshape2::melt(spei, id.var = "code"))

# Store tidy tibble of the NCDF values
saveRDS(df_spei, paste0("data/geo/geo_spei_", timescale, ".rds"))


detach("package:dplyr")

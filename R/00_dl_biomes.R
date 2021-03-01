
# Biomes from https://data.globalforestwatch.org/ ---------------------------

path <- "data/biomes_brazil/"
link <- "http://gfw2-data.s3.amazonaws.com/country/bra/zip/bra_biomes.zip"
file <- "bra_biomes.zip"

if(!file.exists(paste0(path, file))) {
  download.file(paste0(link), destfile = paste0(path, file))
}

unzip(paste0(path, file), exdir = path, unzip = "/usr/bin/unzip")

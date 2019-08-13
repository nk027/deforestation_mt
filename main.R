

# Downloads ---------------------------------------------------------------

# Download necessary (and redundant) data to proceed with the analysis.

# Land use change maps from Câmara et al. (2019)
# Downloads yearly TIFs to "data/landsat/"
source("R/00_dl_landsat.R")

# Maps of political boundaries from IBGE
# Downloads ZIPs of yearly shapefiles, unzips them to "data/municipios/%YEAR"
# and copies the latest version to "data/municipios"
source("R/00_dl_municipios.R")

# SPEI from Vicente-Serrano (2010)
# Downloads NCDFs of desired timescales to "data/spei/"
source("R/00_dl_spei.R")

# Note: Additional IBGE data was downloaded manually from SIDRA

rm(list = ls()); gc()


# Read in data ------------------------------------------------------------


libs <- c("dplyr", "parallel")
deps <- c("rgdal", "sp", "raster")
source("R/1_merge_geo.R")
detach("package:dplyr")

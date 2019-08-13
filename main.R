

# Downloads ---------------------------------------------------------------

# Download necessary (and redundant) data to proceed with the analysis.

# Land use change maps from Câmara et al. (2019)
# Downloads yearly TIFs to "data/landsat/".
source("R/00_dl_landsat.R")

# Maps of political boundaries from IBGE
# Downloads ZIPs of yearly shapefiles, unzips them to "data/municipios/%YEAR"
# and copies the latest version to "data/municipios".
source("R/00_dl_municipios.R")

# SPEI from Vicente-Serrano (2010)
# Downloads NCDFs of desired timescales to "data/spei/".
source("R/00_dl_spei.R")

# Note: Additional IBGE data was downloaded manually from SIDRA.

rm(list = ls()); gc()


# Prepare land use change data --------------------------------------------

# Read in TIF raster files from "data/landsat", merge them with the SHP map 
# of political boundaries found in "data/municipios" and create a tidy tibble.
# Intermediate and final outputs are stored in "data/geo".

# Read in TIFs and merge them with the SHP file using `raster::extract`. Creates
# a list of extracted dataframes and stores it as "geo_extract_raw.rds".
# Very computationally intensive, done via `parallel::parLapply` if available.
source("R/10_raster_extract.R")

# Transform the list of extracted values into a wide tibble and clean variables.
# Stores said tibble as "geo_extract.rds".
source("R/11_raster_df.R")

# Tidy the tibble from the previous step, transforming it to a long table. 
# Stores the tidy tibble as "geo_df_long.rds".
source("R/12_raster_tidy.R")

rm(list = ls()); gc()

# Read in the SHP map using the sf package and join it with the tidy tibble
# of values extracted from the land use change maps.
# Stores an sf-tibble as "shp.rds".
source("R/15_raster_aggregate.R")

rm(list = ls()); gc()


# Prepare SPEI data -------------------------------------------------------

# Read in NCDF raster files from "data/spei", merge them with IDs from the maps 
# of political boundaries found in "data/municipios" and create a tidy tibble.
# Intermediate and final outputs are stored in "data/geo". The desired 
# SPEI timescale is set in the scripts.

# Read in NCDFs and merge them with the SHP file using weighted 
# `raster::extract`. Creates a list of extracted dataframes and stores it as 
# "geo_spei_%TIMESCALE_raw.rds".
# Very computationally intensive, done via `parallel::parLapply` if available.
source("R/10_spei_extract.R")

# Create and tidy a tibble from the list of extracted values. Stores the tidy 
# tibble in long format as "geo_spei_%TIMESCALE.rds".
source("R/11_spei_tidy.R")

# Transform the SPEI to the format used later on (i.e. two binary variables).
# Stores the resulting tibble as "spei_%TIMESCALE.rds".
source("R/15_spei_prep.R")

rm(list = ls()); gc()

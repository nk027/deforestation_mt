

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

# Note: Additional IBGE data was downloaded manually from SIDRA and adjusted by
# hand due to limitation of ODS et cetera.

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
# Tidy the tibble, (create a long table) and store it as "geo_df_long.rds".
source("R/11_raster_tidy.R")

# Read in the SHP map using the sf package and join it with the tidy tibble
# of values extracted from the land use change maps.
# Stores an sf-tibble as "shp.rds".
source("R/12_raster_aggregate.R")

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


# Prepare tabular data ----------------------------------------------------

# Prepare the manually downloaded and adjusted data from SIDRA. This part is not
# as easily reproduced, but should be replicable with the data that is available
# online.
# Outputs are stored as RDS files under "data/tab".

source("R/16_functions_read.R")

# Read in ODS files with crop data from "data/sidra" and merge them with each 
# other.
# The files are split to contain areas and quantities (i.e. value and weight). 
# Numbered files contain different periods, where #1 is dropped as it is too 
# early to be useful. Worksheets contain planted (1) and harvested (2) area.
# Creates two files - "crop_quant.rds" and "crop_value.rds".
source("R/17_crops_read.R")

# Read in ODS files with forestry data from "data/sidra".
# The files are split into actual forestry and vegetable data. Columns contain
# totals and subtotals, making subsetting necessary. Worksheets contain 
# quantities (1) and values (2).
# Creates four files - "forestry_quant.rds", "forestry_value.rds", 
# "veggies_quant.rds" and "veggies_value.rds".
source("R/17_forestry_read.R")

# Read in ODS files with livestock data from "data/sidra".
# The files are split into animal produce, with worksheets containing 
# quantities (1) and values (2), general herd sizes and the herd size of milk
# cows.
# Creates four files - "animal_quant.rds", "animal_value.rds", "herd_sizes.rds"
# and "milk_cows.rds".
source("R/17_livestock_read.R")

# Read in ODS files with socioeconomic data from "data/sidra" and merge them 
# with each other.
# The files are split into GDP, population estimates and population censuses for
# 2007 and 2010.
# Creates three files - "gdp.rds", "pop.rds" and "pop_details.rds".
source("R/17_socio_read.R")

rm(list = ls()); gc()


# Merge datasets ----------------------------------------------------------

source("R/18_tab_aggregate.R")

source("R/19_yield_fill.R")

source("R/20_data_wrap.R")
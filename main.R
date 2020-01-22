

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
source("R/13_spei_extract.R")

# Create and tidy a tibble from the list of extracted values. Stores the tidy
# tibble in long format as "geo_spei_%TIMESCALE.rds".
source("R/14_spei_tidy.R")

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

# Merge RDS files from the previous steps, derive variables of interest, explore
# yields and fill missing values for soy yields.
# Ultimately create an RDS file under "data/data.rds".

# Merge different IBGE datasets, keep variables of interest and adjust units.
# Store the resulting tibble as "data/tab/tab.rds".
source("R/18_tab_aggregate.R")

# Wrap up land use, SPEI and IBGE tabular data in a single file. Derive some
# variables of interest. Explore yield values of crops.
# Results are stored as sf-tibble under "data/data_raw.rds"
source("R/19_data_wrap.R")

# Explore and fill missing soy yields from the previous step.
# Store resulting sf-tibble as "data/data_soy.rds".
source("R/20_data_soy.R")

# Add some transformed and derived variables.
# Stores final sf-tibble as "data/data.rds".
source("R/21_data_fin.R")

rm(list = ls()); gc()


# Model -------------------------------------------------------------------

# Set up, calculate and assess models. Fit a custom Bayesian SDM, a CLM using
# `plm::plm` and a SAR / SEM using `splm::spml` to models built from "data.rds".
# Results are stored as RDA files under "data/models_%EFFECT.rda", where effect
# corresponds to the treatment of fixed effects.

# Functions to aid with model estimation (get data subsets, build weights
# matrices, ...).
source("R/30_functions_model.R")

# Preparation and options for the model estimation. Contained settings are also
# required for subsequent assessments.
source("R/31_model_setup.R")

# After an update you can now calculate all models using a Bayesian approach.
# Source (updated) functions to calculate SDM, SAR, SEM and CLM
source("R/33_functions_bayes.R")

# Execute Bayesian model estimation, based on the setup from before. Note
# that the MCMC algorithms tend to take quite a while.
# Outputs are RDA files stored as "models_bayesian_%EFFECT.rda".
source("R/34_calc_bayesian.R")

# Functions to aid in assessing results, i.e. summarise outputs, create tables
# and calculate in- and out-of-sample fits.
source("R/40_functions_assess.R")

# Assess the results created in previous steps. Depends on the settings from
# "31_model_setup.R" as well as the RDA files with lists of results.
# Generates CSV files with results and PNG files of the model fit.
source("R/41_assess_bayes.R")

# Perform several tests (Chow, LM, Moran's...). Depends on settings and data
# from "31_model_setup.R".
source("R/45_model_tests.R")

rm(list = ls()); gc()


# Other -------------------------------------------------------------------

# Crosscheck our studied area with the deforestation drivers identified by
# Curtis et al. (2018). Important for the forestloss ~ deforestation argument.
source("R/70_loss_drivers.R")

# Create a plot of relevant land use from the data of Câmara et al. (2019).
# The two evaluated years and the legend are plotted separately. The final
# version was created using GIMP.
source("R/71_landuse_plot.R")

# Create a plot of Mato Grosso's municipios.
source("R/72_municipio_plot.R")

# Create a CSV file with summary statistics of the data used.
source("R/73_summary_table.R")

# Create a PNG file with a heatmap of the RMSEs of models over time.
source("R/75_rmse_plot.R")

rm(list = ls()); gc()

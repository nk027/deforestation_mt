
# Dependencies ------------------------------------------------------------

stopifnot(
  file.exists("data/data.rds"),
  require("Matrix"),
  require("sf"),
  require("dplyr"),
  require("spatialreg")
)

source("R/50_estimation.R")
source("R/51_supp.R")
source("R/52_helpers.R")


# Load models -------------------------------------------------------------

weights <- "qu"
model <- "base"

load(file = paste0("data/est_", model, "_", weights, ".rda"))


# Analyse -----------------------------------------------------------------

hpdi_zero(hpdi(out_sdm))
hpdi_zero(hpdi(out_sar))
hpdi_zero(hpdi(out_slx))
hpdi_zero(hpdi(out_clm))

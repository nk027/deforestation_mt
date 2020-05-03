
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


# Analyse -----------------------------------------------------------------

# c("base", "base_lags", "no_pop", "contemp", "no_yields", "no_land")
weights <- "qu"
model <- "base"

for(model in names(variables)) {
  
vars <- variables[[model]][-1]
load(file = paste0("data/est_", model, "_", weights, ".rda"))

tbl <- list(
  "variables" = c("c", vars, "c_ind", paste0(vars, "_ind"), "rho"),
  "sdm" = hpdi_zero(hpdi(out_sdm)),
  "sar" = hpdi_zero(hpdi(out_sar)),
  "slx" = hpdi_zero(hpdi(out_slx)),
  "clm" = hpdi_zero(hpdi(out_clm)))

tmp <- tbl[["slx"]]
tbl[["slx"]] <- c(tmp[names(tmp) %in% c("alpha", "beta")], NA, 
  tmp[names(tmp) == "theta"], NA)
tmp <- tbl[["clm"]]
tbl[["clm"]] <- c(tmp[names(tmp) %in% c("alpha", "beta")], NA, 
  rep(NA, length(vars)), NA)

tbl <- Reduce(cbind, tbl)
dimnames(tbl) <- list(NULL, c("vars", "sdm", "sar", "slx", "clm"))

write.csv(tbl, paste0("txt/result_", model, "_", weights, ".csv"))

}

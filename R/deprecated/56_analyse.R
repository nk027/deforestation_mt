
# Dependencies ------------------------------------------------------------

stopifnot(
  exists("variables"), # 55
  require("Matrix"),
  require("sf"),
  require("dplyr"),
  require("spatialreg")
)

source("R/50_estimation.R")
source("R/51_supp.R")
source("R/52_helpers.R")

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Analyse -----------------------------------------------------------------

# c("base", "base_lags", "no_pop", "contemp", "no_yields", "no_land")
weights <- "qu"
model <- "base"

# Get asterisks from HPDI
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

ics <- rbind(
  c("RMSE", mean(rmse(out_sdm)), mean(rmse(out_sar)), 
  mean(rmse(out_slx)), mean(rmse(out_clm))),
  c("AIC", AIC(out_sdm, mode), AIC(out_sar, mode), 
    AIC(out_slx, mode), AIC(out_clm, mode)),
  c("BIC", BIC(out_sdm, mode), BIC(out_sar, mode), 
    BIC(out_slx, mode), BIC(out_clm, mode)),
  c("DIC", DIC(out_sdm), DIC(out_sar), DIC(out_slx), DIC(out_clm)))

tbl <- rbind(tbl, ics)

write.csv(tbl, paste0("txt/result_", model, "_", weights, ".csv"))

}


# Get values
for(model in names(variables)) {

vars <- variables[[model]][-1]
load(file = paste0("data/est_", model, "_", weights, ".rda"))

tbl <- list(
  "variables" = c("c", vars, "c_ind", paste0(vars, "_ind"), "rho"),
  "sdm" = c(unlist(lapply(effects(out_sdm), function(x) apply(x, 2, mean))), mean(out_sdm$rho)),
  "sar" = c(unlist(lapply(effects(out_sar), function(x) apply(x, 2, mean))), mean(out_sar$rho)),
  "slx" = apply(out_slx$beta[, !colnames(out_slx$beta) %in% c("ife", "tfe")], 2, mean),
  "clm" = apply(out_clm$beta[, !colnames(out_clm$beta) %in% c("ife", "tfe")], 2, mean))

tmp <- tbl[["slx"]]
tbl[["slx"]] <- c(tmp[names(tmp) %in% c("alpha", "beta")], NA, 
  tmp[names(tmp) == "theta"], NA)
tmp <- tbl[["clm"]]
tbl[["clm"]] <- c(tmp[names(tmp) %in% c("alpha", "beta")], NA, 
  rep(NA, length(vars)), NA)

tbl <- Reduce(cbind, tbl)
dimnames(tbl) <- list(NULL, c("vars", "sdm", "sar", "slx", "clm"))

ics <- rbind(
  c("RMSE", mean(rmse(out_sdm)), mean(rmse(out_sar)), 
    mean(rmse(out_slx)), mean(rmse(out_clm))),
  c("AIC", AIC(out_sdm, mode), AIC(out_sar, mode), 
    AIC(out_slx, mode), AIC(out_clm, mode)),
  c("BIC", BIC(out_sdm, mode), BIC(out_sar, mode), 
    BIC(out_slx, mode), BIC(out_clm, mode)),
  c("DIC", DIC(out_sdm), DIC(out_sar), DIC(out_slx), DIC(out_clm)))

tbl <- rbind(tbl, ics)

write.csv(tbl, paste0("txt/result-f_", model, "_", weights, ".csv"))

}


# Get HPDI
model <- "base"
weights <- "qu"

vars <- variables[[model]][-1]
load(file = paste0("data/est_", model, "_", weights, ".rda"))

tbl <- list(
  "sdm" = hpdi(out_sdm),
  "sar" = hpdi(out_sar),
  "slx" = hpdi(out_slx),
  "clm" = hpdi(out_clm))

rownames(tbl[[1]]) <- c("c", vars, "c_ind", paste0(vars, "_ind"), "rho")
rownames(tbl[[2]]) <- c("c", vars, "c_ind", paste0(vars, "_ind"), "rho")
rownames(tbl[[3]]) <- c("c", vars, paste0(vars, "_ind"))
rownames(tbl[[4]]) <- c("c", vars)

saveRDS(tbl, "data/result_hpdi.rds")


library(plm)
library(splm)
library(spatialreg)

dates <- seq(2005, 2015)
dates_len <- length(dates)

tfe <- TRUE
cfe <- TRUE
effect <- if(tfe) {if(cfe) {"twoways"} else {"time"}
} else {if(cfe) {"individual"} else{stop()}}

matrices <- list()
results_qu <- list()
results_k5n <- list()
results_k7n <- list()
results_plm <- list()
results_lag_qu <- list()
results_lag_k5n <- list()
results_err_qu <- list()
results_err_k5n <- list()


# Go over all models ------------------------------------------------------

counter <- 1
for(counter in seq_along(variables)) {

matrices[[counter]] <- get_matr(data, variables[[counter]], dates = dates)

df_plm <- as.data.frame(cbind(rep(1:(141 - len(municipio_subset)), dates_len), 
                              rep(1:dates_len, 141 - len(municipio_subset)),
                              matrices[[counter]]))


# Bayesian SDM ---------------------------------------------------------------

results_qu[[counter]] <- sdm_panel(matrices[[counter]], W_qu, dates_len, tfe = tfe, cfe = cfe)
results_k5n[[counter]] <- sdm_panel(matrices[[counter]], W_k5n, dates_len, tfe = tfe, cfe = cfe)
results_k7n[[counter]] <- sdm_panel(matrices[[counter]], W_k7n, dates_len, tfe = tfe, cfe = cfe)


# PLM ---------------------------------------------------------------------

results_plm[[counter]] <- plm::plm(formula_ify(variables[[counter]]), df_plm, 
                                   effect = effect, model = "within")


# SPLM --------------------------------------------------------------------

results_lag_qu[[counter]] <- spml(formula_ify(variables[[counter]]), df_plm, 
                                  listw = W_qu, model = "within", effect = effect, 
                                  lag = TRUE, spatial.error = "none")
results_lag_k5n[[counter]] <- spml(formula_ify(variables[[counter]]), df_plm, 
                                   listw = W_k5n, model = "within", effect = effect, 
                                   lag = TRUE, spatial.error = "none")

results_err_qu[[counter]] <- spml(formula_ify(variables[[counter]]), df_plm, 
                                  listw = W_qu, model = "within", effect = effect, 
                                  lag = FALSE, spatial.error = "b")
results_err_k5n[[counter]] <- spml(formula_ify(variables[[counter]]), df_plm, 
                                   listw = W_k5n, model = "within", effect = effect, 
                                   lag = FALSE, spatial.error = "b")

}


# Store results -----------------------------------------------------------

save(file = paste0("data/models_", effect, ".rda"), 
     list = c("results_qu", "results_k5n", "results_k7n", 
              "results_plm", 
              "results_lag_qu", "results_err_qu", 
              "results_lag_k5n", "results_err_k5n"))

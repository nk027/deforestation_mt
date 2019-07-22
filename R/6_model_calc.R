
dates <- seq(2005, 2015)
dates_len <- length(dates)

tfe <- TRUE
cfe <- TRUE
effect <- if(tfe) {if(cfe) {"twoways"} else {"time"}
} else {if(cfe) {"individual"} else{stop()}}

counter <- 1
# for(counter in seq_along(variables)) {}

matrices <- list()
results_qu <- list()
results_k5n <- list()
results_k7n <- list()
results_plm <- list()
results_lag <- list()
results_err <- list()

matrices[[counter]] <- get_matr(data, variables[[counter]], dates = dates)

df_plm <- as.data.frame(cbind(rep(1:(141 - len(municipio_subset)), dates_len), 
                              rep(1:dates_len, 141 - len(municipio_subset)),
                              matrices[[counter]])


# Bayesian SDM ---------------------------------------------------------------

results_qu[[counter]] <- sdm_panel(matrices[[counter]], W_qu, dates_len)
results_k5n[[counter]] <- sdm_panel(matrices[[counter]], W_k5n, dates_len)
results_k7n[[counter]] <- sdm_panel(matrices[[counter]], W_k7n, dates_len)


# PLM ---------------------------------------------------------------------

library(plm)

results_plm[[counter]] <- plm::plm(formula_ify(variables[[counter]]), df_plm, 
                                   effect = effect, model = "within")


# SPLM --------------------------------------------------------------------

library(splm)
library(spatialreg)

results_lag[[counter]] <- spml(formula_ify(variables[[counter]]), df_plm, 
                               listw = W_qu, model = "within", effect = effect, 
                               lag = TRUE, spatial.error = "none")

results_err[[counter]] <- spml(formula_ify(variables[[counter]]), df_plm, 
                               listw = W_qu, model = "within", effect = effect, 
                               lag = FALSE, spatial.error = "b")

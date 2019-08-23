
# Dependencies ------------------------------------------------------------

stopifnot(
  exists("fixed_effects"), exists("variables"), exists("dates"), # etc., 31
  require("dplyr"),
  require("plm"),
  require("splm"),
  require("spatialreg")
)


# Go over FEs -------------------------------------------------------------

# fe <- c(TRUE, TRUE)
for(fe in fixed_effects) {

tfe <- fe[1]; cfe <- fe[2]
pl_model <- "within"
if(tfe) {if(cfe) {effect <- "twoways"} else {effect <- "time"}} else {
  if(cfe) {effect <- "individual"} else {
    effect <- "individual" # Renamed later, must be one of the three
    pl_model <- "pooling"
  }
}

# Storage
matrices <- list()
# Bayesian SDM
results_qu <- list()
results_k5n <- list()
results_k7n <- list()
# plm
results_plm <- list()
# splm
results_lag_qu <- list()
results_lag_k5n <- list()
results_err_qu <- list()
results_err_k5n <- list()


# Go over all models ------------------------------------------------------

# counter <- 1
for(counter in seq_along(variables)) {

matrices[[counter]] <- get_matr(data, variables[[counter]], dates = dates)

df_plm <- as.data.frame(cbind(rep(1:(141 - len(municipio_subset)), dates_len), 
                              rep(1:dates_len, 141 - len(municipio_subset)),
                              matrices[[counter]]))


# Bayesian SDM ---------------------------------------------------------------

results_qu[[counter]] <- sdm_panel(matrices[[counter]], 
                                   W_qu, dates_len, tfe = tfe, cfe = cfe,
                                   n_iter = n_iter, n_save = n_save, 
                                   n_griddy = n_griddy)
results_k5n[[counter]] <- sdm_panel(matrices[[counter]], 
                                    W_k5n, dates_len, tfe = tfe, cfe = cfe,
                                    n_iter = n_iter, n_save = n_save, 
                                    n_griddy = n_griddy)
results_k7n[[counter]] <- sdm_panel(matrices[[counter]],
                                    W_k7n, dates_len, tfe = tfe, cfe = cfe,
                                    n_iter = n_iter, n_save = n_save, 
                                    n_griddy = n_griddy)


# PLM ---------------------------------------------------------------------

results_plm[[counter]] <- plm::plm(formula_ify(variables[[counter]]), df_plm, 
                                   effect = effect, model = pl_model)


# SPLM --------------------------------------------------------------------

results_lag_qu[[counter]] <- spml(formula_ify(variables[[counter]]), df_plm, 
                                  listw = W_qu, model = pl_model, 
                                  effect = effect, lag = TRUE, 
                                  spatial.error = "none")
results_lag_k5n[[counter]] <- spml(formula_ify(variables[[counter]]), df_plm, 
                                   listw = W_k5n, model = pl_model, 
                                   effect = effect, lag = TRUE, 
                                   spatial.error = "none")

results_err_qu[[counter]] <- spml(formula_ify(variables[[counter]]), df_plm, 
                                  listw = W_qu, model = pl_model, 
                                  effect = effect, lag = FALSE, 
                                  spatial.error = "b")
results_err_k5n[[counter]] <- spml(formula_ify(variables[[counter]]), df_plm, 
                                   listw = W_k5n, model = pl_model, 
                                   effect = effect, lag = FALSE, 
                                   spatial.error = "b")
}

if(pl_model == "pooling") {effect <- "none"} # Properly name effect now

# Store results -----------------------------------------------------------

save(file = paste0("data/models_", effect, ".rda"), 
     list = c("results_qu", "results_k5n", "results_k7n", 
              "results_plm", 
              "results_lag_qu", "results_err_qu", 
              "results_lag_k5n", "results_err_k5n"))
}


detach("package:dplyr")
detach("package:plm")
detach("package:splm")
detach("package:spatialreg")

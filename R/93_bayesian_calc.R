
# Dependencies ------------------------------------------------------------

stopifnot(
  exists("fixed_effects"), exists("variables"), exists("dates"), # etc., 31
  require("dplyr"),
  require("MASS"),
  require("Matrix"),
  require("matrixcalc"),
  require("plm"),
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

matrices <- list()
sdm_qu <- list()
sdm_k5 <- list()
sdm_k7 <- list()
sar_qu <- list()
sar_k5 <- list()
sar_k7 <- list()
sem_qu <- list()
sem_k5 <- list()
sem_k7 <- list()
clm <- list()

for(counter in seq_along(variables)) {

matrices[[counter]] <- get_matr(data, variables[[counter]], dates = dates)

sdm_qu <- sdm_panel(matrices[[counter]],
                    W_qu, dates_len, lag_X = TRUE, tfe = tfe, cfe = cfe,
                    rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                    n_iter = n_iter, n_save = n_save,
                    n_griddy = n_griddy)
sdm_k5 <- sdm_panel(matrices[[counter]],
                    W_k5n, dates_len, lag_X = TRUE, tfe = tfe, cfe = cfe,
                    rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                    n_iter = n_iter, n_save = n_save,
                    n_griddy = n_griddy)
sdm_k7 <- sdm_panel(matrices[[counter]],
                    W_k7n, dates_len, lag_X = TRUE, tfe = tfe, cfe = cfe,
                    rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                    n_iter = n_iter, n_save = n_save,
                    n_griddy = n_griddy)
sar_qu <- sdm_panel(matrices[[counter]],
                    W_qu, dates_len, lag_X = FALSE, tfe = tfe, cfe = cfe,
                    rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                    n_iter = n_iter, n_save = n_save,
                    n_griddy = n_griddy)
sar_k5 <- sdm_panel(matrices[[counter]],
                    W_k5n, dates_len, lag_X = FALSE, tfe = tfe, cfe = cfe,
                    rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                    n_iter = n_iter, n_save = n_save,
                    n_griddy = n_griddy)
sar_k7 <- sdm_panel(matrices[[counter]],
                    W_k7n, dates_len, lag_X = FALSE, tfe = tfe, cfe = cfe,
                    rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                    n_iter = n_iter, n_save = n_save,
                    n_griddy = n_griddy)
sem_qu <- sem_panel(matrices[[counter]],
                    W_qu, dates_len, tfe = tfe, cfe = cfe,
                    rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                    n_iter = n_iter, n_save = n_save)
sem_k5 <- sem_panel(matrices[[counter]],
                    W_k5n, dates_len, tfe = tfe, cfe = cfe,
                    rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                    n_iter = n_iter, n_save = n_save)
sem_k7 <- sem_panel(matrices[[counter]],
                    W_k7n, dates_len, tfe = tfe, cfe = cfe,
                    rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                    n_iter = n_iter, n_save = n_save)
clm <- clm_panel(matrices[[counter]], dates_len, tfe = tfe, cfe = cfe,
                 sigma_a, sigma_b, beta_mean, beta_var,
                 n_iter = n_iter, n_save = n_save)

}

if(pl_model == "pooling") {effect <- "none"} # Properly name effect now

# Store results -----------------------------------------------------------

save(file = paste0("data/models_bayesian_", effect, ".rda"),
     list = c("sdm_qu", "sdm_k5", "sdm_k7",
              "sar_qu", "sar_k5", "sar_k7",
              "sem_qu", "sem_k5", "sem_k7",
              "clm"))

}

detach("package:dplyr")
detach("package:MASS")
detach("package:Matrix")
detach("package:plm")
detach("package:spatialreg")

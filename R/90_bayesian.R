
fe <- fixed_effects[[1]]
tfe <- fe[1]; cfe <- fe[2]
efffect <- "twoways"

counter <- 1

matrices <- list()
matrices[[counter]] <- get_matr(data, variables[[counter]], dates = dates)

lag_X = TRUE
tfe = TRUE
cfe = TRUE
rho_a = 1.01
sigma_a = 0.01
sigma_b = 0.01
beta_mean = 0
beta_var = 10 ^ 8
n_iter = 20000
n_save = 10000
n_griddy = 2000

source("R/99_bayesian.R")

sdm <- sdm_panel(matrices[[counter]], 
                 W_qu, dates_len, lag_X = TRUE, tfe = tfe, cfe = cfe,
                 rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                 n_iter = n_iter, n_save = n_save, 
                 n_griddy = n_griddy)
sar <- sdm_panel(matrices[[counter]], 
                 W_qu, dates_len, lag_X = FALSE, tfe = tfe, cfe = cfe,
                 rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                 n_iter = n_iter, n_save = n_save, 
                 n_griddy = n_griddy)
sem <- sem_panel(matrices[[counter]], 
                 W_qu, dates_len, tfe = tfe, cfe = cfe,
                 rho_a, sigma_a, sigma_b, beta_mean, beta_var,
                 n_iter = n_iter, n_save = n_save)
clm <- clm_panel(matrices[[counter]], dates_len, tfe = tfe, cfe = cfe,
                 sigma_a, sigma_b, beta_mean, beta_var,
                 n_iter = n_iter, n_save = n_save)

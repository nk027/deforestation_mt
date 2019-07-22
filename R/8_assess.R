
source("R/7_calc_fit.R")
load("data/models.rda")

counter <- 1
cat(print_vars(variables[[counter]]))

sm_results(results_qu[[counter]])
sm_results(results_k5n[[counter]])
sm_results(results_k7n[[counter]])
summary(results_plm[[counter]])
summary(results_lag[[counter]])
summary(results_err[[counter]])

date_fit <- max(dates) + 1
tfe_idx <- if(date_fit %in% dates) {which(dates == date_fit)} else {NULL}

oos <- prep_fit(data, date_fit, variables[[counter]])

sdm_qu_fit <- bayesian_fit(oos, variables[[counter]], results_qu[[counter]],
                        W_qu, lag_X = TRUE, tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
sdm_qu_fit_mean <- apply(sdm_fit, 1, mean)

sdm_k5n_fit <- bayesian_fit(oos, variables[[counter]], results_k5n[[counter]],
                           W_k5n, lag_X = TRUE, tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
sdm_k5n_fit_mean <- apply(sdm_fit, 1, mean)

sdm_k7n_fit <- bayesian_fit(oos, variables[[counter]], results_k7n[[counter]],
                           W_k7n, lag_X = TRUE, tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
sdm_k7n_fit_mean <- apply(sdm_fit, 1, mean)

plm_fit <- plm_fit(oos, results_plm[[counter]], tfe, cfe, tfe_idx = tfe_idx)

sar_fit <- splm_fit(oos, results_lag[[counter]], W_qu, tfe, cfe, tfe_idx = tfe_idx)

sem_fit <- splm_fit(oos, results_err[[counter]], W_qu, tfe, cfe, tfe_idx = tfe_idx)

op <- par(mfrow = c(2, 3))
plot(oos[, 1] - sdm_qu_fit_mean)
abline(h = 0)
plot(oos[, 1] - sdm_k5n_fit_mean)
abline(h = 0)
plot(oos[, 1] - sdm_k7n_fit_mean)
abline(h = 0)
plot(oos[, 1] - plm_fit)
abline(h = 0)
plot(oos[, 1] - sar_fit)
abline(h = 0)
plot(oos[, 1] - sem_fit)
abline(h = 0)
par(op)

counter <- counter + 1
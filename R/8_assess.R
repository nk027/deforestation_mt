
source("R/7_calc_fit.R")
# load("data/models.rda")

date_fit <- max(dates) + 1
# date_fit <- 2010
tfe_idx <- if(date_fit %in% dates) {which(dates == date_fit)} else {NULL}

counter <- 1
for(counter in seq_along(variables)) {
  
cat(print_vars(variables[[counter]]))

sm_results(results_qu[[counter]])
sm_results(results_k5n[[counter]])
sm_results(results_k7n[[counter]])
summary(results_plm[[counter]])
summary(results_lag[[counter]])
summary(results_err[[counter]])


# Check fit ---------------------------------------------------------------

oos <- prep_fit(data, date_fit, variables[[counter]])

sdm_qu_fit <- bayesian_fit(oos, variables[[counter]], results_qu[[counter]],
                        W_qu, lag_X = TRUE, tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
sdm_qu_fit_mean <- apply(sdm_qu_fit, 1, mean)

sdm_k5n_fit <- bayesian_fit(oos, variables[[counter]], results_k5n[[counter]],
                           W_k5n, lag_X = TRUE, tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
sdm_k5n_fit_mean <- apply(sdm_k5n_fit, 1, mean)

sdm_k7n_fit <- bayesian_fit(oos, variables[[counter]], results_k7n[[counter]],
                           W_k7n, lag_X = TRUE, tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
sdm_k7n_fit_mean <- apply(sdm_k7n_fit, 1, mean)

clm_fit <- plm_fit(oos, results_plm[[counter]], tfe, cfe, tfe_idx = tfe_idx)

sar_fit <- splm_fit(oos, results_lag[[counter]], W_qu, tfe, cfe, tfe_idx = tfe_idx)

sem_fit <- splm_fit(oos, results_err[[counter]], W_qu, tfe, cfe, tfe_idx = tfe_idx)

png(paste0("plots/", date_fit, "_oos_fit_model_", counter, ".png"), width = 1000, height = 600)
print({op <- par(mfrow = c(2, 3), mar = c(2, 2, 2, 0.5))
plot(oos[, 1] - sdm_qu_fit_mean, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SDM, Q, SSR = ", ssr(oos[, 1], sdm_qu_fit_mean)))
plot(oos[, 1] - sdm_k5n_fit_mean, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SDM, K5, SSR = ", ssr(oos[, 1], sdm_k5n_fit_mean)))
plot(oos[, 1] - sdm_k7n_fit_mean, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SDM, K7, SSR = ", ssr(oos[, 1], sdm_k7n_fit_mean)))
plot(oos[, 1] - clm_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("PLM, SSR = ", ssr(oos[, 1], clm_fit)))
plot(oos[, 1] - sar_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SAR, Q, SSR = ", ssr(oos[, 1], sar_fit)))
plot(oos[, 1] - sem_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SEM, Q, SSR = ", ssr(oos[, 1], sem_fit)))
par(op)})
dev.off()

}

counter <- counter + 1

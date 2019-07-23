
source("R/7_calc_fit.R")
# load("data/models_twoways.rda")

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
summary(results_lag_qu[[counter]])
summary(results_lag_k5n[[counter]])
summary(results_err_qu[[counter]])
summary(results_err_k5n[[counter]])


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

sar_qu_fit <- splm_fit(oos, results_lag_qu[[counter]], W_qu, tfe, cfe, tfe_idx = tfe_idx)

sar_k5n_fit <- splm_fit(oos, results_lag_k5n[[counter]], W_k5n, tfe, cfe, tfe_idx = tfe_idx)

sem_qu_fit <- splm_fit(oos, results_err_qu[[counter]], W_qu, tfe, cfe, tfe_idx = tfe_idx)

sem_k5n_fit <- splm_fit(oos, results_err_k5n[[counter]], W_k5n, tfe, cfe, tfe_idx = tfe_idx)

png(paste0("plots/", date_fit, "_residual_model_", names(variables)[counter], ".png"), width = 1200, height = 600)
print({op <- par(mfrow = c(2, 4), mar = c(2, 2, 2, 0.5))
plot(oos[, 1] - sdm_qu_fit_mean, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SDM, Q, SSR = ", ssr(oos[, 1], sdm_qu_fit_mean)))
plot(oos[, 1] - sdm_k5n_fit_mean, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SDM, K5, SSR = ", ssr(oos[, 1], sdm_k5n_fit_mean)))
plot(oos[, 1] - sdm_k7n_fit_mean, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SDM, K7, SSR = ", ssr(oos[, 1], sdm_k7n_fit_mean)))
plot(oos[, 1] - clm_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("PLM, SSR = ", ssr(oos[, 1], clm_fit)))
plot(oos[, 1] - sar_qu_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SAR, Q, SSR = ", ssr(oos[, 1], sar_qu_fit)))
plot(oos[, 1] - sar_k5n_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SAR, K5, SSR = ", ssr(oos[, 1], sar_k5n_fit)))
plot(oos[, 1] - sem_qu_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SEM, Q, SSR = ", ssr(oos[, 1], sem_qu_fit)))
plot(oos[, 1] - sem_k5n_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SEM, K5, SSR = ", ssr(oos[, 1], sem_k5n_fit)))
par(op)})
dev.off()

png(paste0("plots/", date_fit, "_comparison_model_", names(variables)[counter], ".png"), width = 1200, height = 600)
print({op <- par(mfrow = c(2, 4), mar = c(2, 2, 2, 0.5))
plot(c(-0.3, 0.1), c(-0.3, 0.1), xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sdm_qu_fit_mean, col = "black"); lines(x = -1:1, y = -1:1)
title(paste0("SDM, Q, SSR = ", ssr(oos[, 1], sdm_qu_fit_mean)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sdm_k5n_fit_mean); lines(x = -1:1, y = -1:1)
title(paste0("SDM, K5, SSR = ", ssr(oos[, 1], sdm_k5n_fit_mean)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sdm_k7n_fit_mean); lines(x = -1:1, y = -1:1)
title(paste0("SDM, K7, SSR = ", ssr(oos[, 1], sdm_k7n_fit_mean)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], clm_fit); lines(x = -1:1, y = -1:1)
title(paste0("PLM, SSR = ", ssr(oos[, 1], clm_fit)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sar_qu_fit); lines(x = -1:1, y = -1:1)
title(paste0("SAR, Q, SSR = ", ssr(oos[, 1], sar_qu_fit)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sar_k5n_fit); lines(x = -1:1, y = -1:1)
title(paste0("SAR, K5, SSR = ", ssr(oos[, 1], sar_k5n_fit)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sem_qu_fit); lines(x = -1:1, y = -1:1)
title(paste0("SEM, Q, SSR = ", ssr(oos[, 1], sem_qu_fit)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sem_k5n_fit); lines(x = -1:1, y = -1:1)
title(paste0("SEM, K5N, SSR = ", ssr(oos[, 1], sem_k5n_fit)))
par(op)})
dev.off()

}

counter <- counter + 1

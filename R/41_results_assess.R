
# Dependencies ------------------------------------------------------------

stopifnot(
  exists("fixed_effects"), exists("variables"), exists("dates"), # etc., 31
  exists("prep_fit"), exists("bayesian_fit"), exists("sm_results"), # etc., 40
  require("dplyr"),
  require("plm"),
  require("splm")
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

# Retrieve estimated model based on FE
load(paste0("data/models_", effect, ".rda"))


# Go over all models ------------------------------------------------------

# counter <- 1
for(counter in seq_along(variables)) {


# Check out results

cat(print_vars(variables[[counter]]))

sm_results(results_qu[[counter]])
sm_results(results_k5n[[counter]])
sm_results(results_k7n[[counter]])
summary(results_plm[[counter]])
summary(results_lag_qu[[counter]])
summary(results_lag_k5n[[counter]])
summary(results_err_qu[[counter]])
summary(results_err_k5n[[counter]])

table <- do.call(cbind, 
        lapply(list(results_qu[[counter]], results_k5n[[counter]], 
                    results_k7n[[counter]], results_plm[[counter]], 
                    results_lag_qu[[counter]], results_lag_k5n[[counter]], 
                    results_err_qu[[counter]], results_err_k5n[[counter]]), 
               table_ise, variables[[counter]]))
table <- table[, c(1, which(!names(table) == "variables"))]
write.csv(table, file = paste0("txt/fit_", effect, "_", 
                               names(variables)[counter], ".csv"))


# Check out fit

# date_fit <- max(dates) + 1
# date_fit <- 2010
for(date_fit in c(dates, max(dates) + 1)) {

tfe_idx <- if(date_fit %in% dates) {which(dates == date_fit)} else {NULL}


oos <- prep_fit(data, date_fit, variables[[counter]])

sdm_qu_fit <- bayesian_fit(oos, variables[[counter]], results_qu[[counter]],
                           W_qu, dates_len, lag_X = TRUE, 
                           tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
sdm_qu_fit_mean <- apply(sdm_qu_fit, 1, mean)

sdm_k5n_fit <- bayesian_fit(oos, variables[[counter]], results_k5n[[counter]],
                            W_k5n, dates_len, lag_X = TRUE,
                            tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
sdm_k5n_fit_mean <- apply(sdm_k5n_fit, 1, mean)

sdm_k7n_fit <- bayesian_fit(oos, variables[[counter]], results_k7n[[counter]],
                            W_k7n, dates_len, lag_X = TRUE, 
                            tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
sdm_k7n_fit_mean <- apply(sdm_k7n_fit, 1, mean)

clm_fit <- plm_fit(oos, results_plm[[counter]], tfe, cfe, tfe_idx = tfe_idx)

sar_qu_fit <- splm_fit(oos, results_lag_qu[[counter]], W_qu, 
                       tfe, cfe, tfe_idx = tfe_idx)

sar_k5n_fit <- splm_fit(oos, results_lag_k5n[[counter]], W_k5n, 
                        tfe, cfe, tfe_idx = tfe_idx)

sem_qu_fit <- splm_fit(oos, results_err_qu[[counter]], W_qu, 
                       tfe, cfe, tfe_idx = tfe_idx)

sem_k5n_fit <- splm_fit(oos, results_err_k5n[[counter]], W_k5n, 
                        tfe, cfe, tfe_idx = tfe_idx)


# Plot fit

png(paste0("plots/fit_resid/", date_fit, "_residual_", 
           effect, "_", names(variables)[counter], ".png"), 
    width = 1200, height = 600)
print({op <- par(mfrow = c(2, 4), mar = c(2, 2, 2, 0.5))
plot(oos[, 1] - sdm_qu_fit_mean, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SDM, Q, RMSE = ", rmse(oos[, 1], sdm_qu_fit_mean)))
plot(oos[, 1] - sdm_k5n_fit_mean, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SDM, K5, RMSE = ", rmse(oos[, 1], sdm_k5n_fit_mean)))
plot(oos[, 1] - sdm_k7n_fit_mean, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SDM, K7, RMSE = ", rmse(oos[, 1], sdm_k7n_fit_mean)))
plot(oos[, 1] - clm_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("PLM, RMSE = ", rmse(oos[, 1], clm_fit)))
plot(oos[, 1] - sar_qu_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SAR, Q, RMSE = ", rmse(oos[, 1], sar_qu_fit)))
plot(oos[, 1] - sar_k5n_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SAR, K5, RMSE = ", rmse(oos[, 1], sar_k5n_fit)))
plot(oos[, 1] - sem_qu_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SEM, Q, RMSE = ", rmse(oos[, 1], sem_qu_fit)))
plot(oos[, 1] - sem_k5n_fit, xlab = "region", ylab = "residual")
abline(h = 0); title(paste0("SEM, K5, RMSE = ", rmse(oos[, 1], sem_k5n_fit)))
par(op)})
dev.off()

png(paste0("plots/fit_line/", date_fit, "_comparison_", 
           effect, "_", names(variables)[counter], ".png"), 
    width = 1200, height = 600)
print({op <- par(mfrow = c(2, 4), mar = c(2, 2, 2, 0.5))
plot(c(-0.3, 0.1), c(-0.3, 0.1), 
     xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sdm_qu_fit_mean, col = "black"); lines(x = -1:1, y = -1:1)
title(paste0("SDM, Q, RMSE = ", rmse(oos[, 1], sdm_qu_fit_mean)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), 
     xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sdm_k5n_fit_mean); lines(x = -1:1, y = -1:1)
title(paste0("SDM, K5, RMSE = ", rmse(oos[, 1], sdm_k5n_fit_mean)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), 
     xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sdm_k7n_fit_mean); lines(x = -1:1, y = -1:1)
title(paste0("SDM, K7, RMSE = ", rmse(oos[, 1], sdm_k7n_fit_mean)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), 
     xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], clm_fit); lines(x = -1:1, y = -1:1)
title(paste0("PLM, RMSE = ", rmse(oos[, 1], clm_fit)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), 
     xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sar_qu_fit); lines(x = -1:1, y = -1:1)
title(paste0("SAR, Q, RMSE = ", rmse(oos[, 1], sar_qu_fit)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), 
     xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sar_k5n_fit); lines(x = -1:1, y = -1:1)
title(paste0("SAR, K5, RMSE = ", rmse(oos[, 1], sar_k5n_fit)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), 
     xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sem_qu_fit); lines(x = -1:1, y = -1:1)
title(paste0("SEM, Q, RMSE = ", rmse(oos[, 1], sem_qu_fit)))
plot(c(-0.3, 0.1), c(-0.3, 0.1), 
     xlab = "prediction", ylab = "observation", col = "white")
points(oos[, 1], sem_k5n_fit); lines(x = -1:1, y = -1:1)
title(paste0("SEM, K5N, RMSE = ", rmse(oos[, 1], sem_k5n_fit)))
par(op)})
dev.off()

}

}

}


detach("package:dplyr")
detach("package:plm")
detach("package:splm")

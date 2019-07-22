
library(splm)
library(spatialreg)

out_lag <- spml(form[[var]], data = x[c("ind", "time", variables[[var]])],
                listw = W_pre,
                model = "within", effect = "twoways", 
                lag = TRUE, spatial.error = "none")

out_err1 <- spml(form[[var]], x[c("ind", "time", variables[[var]])],
                 listw = W_pre,
                 model = "within", effect = "twoways", 
                 lag = FALSE, spatial.error = "b")

out_full <- spml(form[[var]], data = x[c("ind", "time", variables[[var]])],
                listw = W_pre,
                model = "within", effect = "twoways", 
                lag = TRUE, spatial.error = "b")

# out_err2 <- spml(form[[var]], x[c("ind", "time", variables[[5]])],
#                  listw = W_pre,
#                  model = "within", effect = "twoways", 
#                  lag = FALSE, spatial.error = "kkp")


# Compare fit -------------------------------------------------------------

summary(out_lag)
summary(out_err1)
summary(out_full)

out_lag$coefficients
out_lag$res.eff[[1]]$res.tfe
out_lag$res.eff[[1]]$res.sfe

c(lag_pred <- oos %*% out_lag$coefficients + 
    out_lag$res.eff[[1]]$res.sfe + 
  mean(out_lag$res.eff[[1]]$res.tfe) + as.numeric(out_lag$res.eff[[1]]$intercept))

c(err1_pred <- oos[, -1] %*% out_err1$coefficients[-1] + 
    out_err1$res.eff[[1]]$res.sfe + 
  mean(out_err1$res.eff[[1]]$res.tfe) + as.numeric(out_err1$res.eff[[1]]$intercept))

c(full_pred <- oos %*% out_full$coefficients[-2] + 
    out_full$res.eff[[1]]$res.sfe + 
    mean(out_full$res.eff[[1]]$res.tfe) + as.numeric(out_full$res.eff[[1]]$intercept))

# c(err2_pred <- oos[, -1] %*% out_err2$coefficients[-1] + 
#     out_err2$res.eff[[1]]$res.sfe + 
#   mean(out_err2$res.eff[[1]]$res.tfe) + as.numeric(out_err2$res.eff[[1]]$intercept))

op <- par(mfrow = c(1, 5))
plot(oos[, 1], y_pred_mean)
lines(x = -1:1, y = -1:1)
plot(oos[, 1], plm_pred)
lines(x = -1:1, y = -1:1)
plot(oos[, 1], lag_pred)
lines(x = -1:1, y = -1:1)
plot(oos[, 1], err1_pred)
lines(x = -1:1, y = -1:1)
plot(oos[, 1], full_pred)
lines(x = -1:1, y = -1:1)
par(op)

op <- par(mfrow = c(1, 5))
plot(oos[, 1] - y_pred_mean)
abline(h = 0)
plot(oos[, 1] - plm_pred)
abline(h = 0)
plot(oos[, 1] - lag_pred)
abline(h = 0)
plot(oos[, 1] - err1_pred)
abline(h = 0)
plot(oos[, 1] - full_pred)
abline(h = 0)
par(op)

sum((oos[, 1] - y_pred_mean)^2)
sum((oos[, 1] - plm_pred)^2)
sum((oos[, 1] - lag_pred)^2)
sum((oos[, 1] - err1_pred)^2)
sum((oos[, 1] - full_pred)^2)


boxplot(oos[, 1], y_pred_mean)

cowplot::plot_grid(
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = c(-0.6, 0.2)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = prd)) +
    scale_fill_viridis_c(limits = c(-0.6, 0.2)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              plm = plm_pred) %>%
    ggplot() + geom_sf(aes(fill = plm)) +
    scale_fill_viridis_c(limits = c(-0.6, 0.2)) +
    cowplot::theme_map()
)

cowplot::plot_grid(
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = prd)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map()
)

cowplot::plot_grid(
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = prd)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd16 = (y_pred_16)) %>%
    ggplot() + geom_sf(aes(fill = prd16)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd84 = (y_pred_84)) %>%
    ggplot() + geom_sf(aes(fill = prd84)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map()
)


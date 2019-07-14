
library(plm)

x <- matrix(NA, ncol = ncol(matrices[[5]]) + 2, nrow = nrow(matrices[[5]]))
x[, 1:ncol(matrices[[5]])] <- matrices[[5]]
x[, ncol(matrices[[5]]) + 1] <- rep(1:(141 - len(municipio_subset)), dates_len)
x[, ncol(matrices[[5]]) + 2] <- rep(1:dates_len, 141 - len(municipio_subset))
colnames(x) <- c(colnames(matrices[[5]]), "ind", "time")
x <- as.data.frame(x)

form1 <- forest_ch_km2 ~ 
  forest_px_km2 + pasture_px_km2 + crop_px_km2 + 
  pop_km2 + gdp_cap + cattle_dens + max_yield_brl + 
  spei_wet + spei_dry
form2 <- forest_ch_km2 ~
  forest_px_km2 + pasture_px_km2 + crop_px_km2_lag +
  pop_km2 + gdp_cap + cattle_dens + max_yield_brl_lag + 
  spei_wet + spei_dry
form3 <- forest_ch_km2 ~
  forest_px_km2 + pasture_px_km2 + crop_px_km2_lag +
  pop_km2 + cattle_dens + soy_filled_lag + spei_wet

out <- plm::plm(form3, x, effect = "twoways", 
                index = c("ind", "time"), model = "within", type = "dfirst")


# Test --------------------------------------------------------------------

# Hausman
out_f <- plm::plm(form3, x, effect = "twoways", 
                  index = c("ind", "time"), model = "within", type = "dfirst")
out_r <- plm::plm(form3, x, effect = "time", 
                  index = c("ind", "time"), model = "random")
phtest(out_f, out_r)

# LM

plmtest(form3, data = x, effect = "twoways", index = c("ind", "time"))

# PCD

pcdtest(form3, data = x, effect = "twoways", index = c("ind", "time"))

# Moran's I
moran.mc(out_f$residuals, mat2listw(kronecker(diag(dates_len), W_qu)), 1000)
moran.mc(out_f$residuals, mat2listw(kronecker(diag(dates_len), W_kn)), 1000)


# Compare fit -------------------------------------------------------------

summary(out)
out$coefficients
plm::fixef(out)

c(plm_pred <- oos[, -1] %*% out$coefficients + mean(plm::fixef(out)))
y_pred_mean
oos[, 1]

op <- par(mfrow = c(1, 2))
plot(oos[, 1], y_pred_mean)
lines(x = -1:1, y = -1:1)
plot(oos[, 1], plm_pred)
lines(x = -1:1, y = -1:1)
par(op)

op <- par(mfrow = c(1, 2))
plot(oos[, 1] - y_pred_mean)
abline(h = 0)
plot(oos[, 1] - plm_pred)
abline(h = 0)
par(op)

boxplot(oos[, 1], y_pred_mean)

cowplot::plot_grid(
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = c(-2.1, 1)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = prd)) +
    scale_fill_viridis_c(limits = c(-2.1, 1)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              plm = plm_pred) %>%
    ggplot() + geom_sf(aes(fill = plm)) +
    scale_fill_viridis_c(limits = c(-2.1, 1)) +
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


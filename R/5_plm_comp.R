
library(plm)

x <- matrix(NA, ncol = ncol(matrices[[1]]) + 2, nrow = nrow(matrices[[1]]))
x[, 1:ncol(matrices[[1]])] <- matrices[[1]]
x[, ncol(matrices[[1]]) + 1] <- rep(1:(141 - len(municipio_subset)), dates_len)
x[, ncol(matrices[[1]]) + 2] <- rep(1:dates_len, 141 - len(municipio_subset))
colnames(x) <- c(colnames(matrices[[1]]), "ind", "time")
x <- as.data.frame(x)

form1 <- forest_ch_km2 ~ 
  forest_px_km2 + pasture_px_km2 + crop_px_km2 + 
  pop_km2 + gdp_cap + cattle_dens + max_yield_brl + 
  spei_wet + spei_dry
form2 <- forest_ch_km2 ~
  forest_px_km2 + pasture_px_km2 + crop_px_km2_lag +
  pop_km2 + gdp_cap + cattle_dens + max_yield_brl_lag + 
  spei_wet + spei_dry

out <- plm::plm(form1, x, effect = "twoways", 
                index = c("ind", "time"), model = "within", type = "dfirst")


# Test --------------------------------------------------------------------

# Hausman
out_f <- plm::plm(form1, x, effect = "twoways", 
                  index = c("ind", "time"), model = "within", type = "dfirst")
out_r <- plm::plm(form1, x, effect = "time", 
                  index = c("ind", "time"), model = "random")
phtest(out_f, out_r)

# LM

plmtest(form1, data = x, effect = "twoways", index = c("ind", "time"))

# PCD

pcdtest(form1, data = x, effect = "twoways", index = c("ind", "time"))

# Moran's I
moran.mc(out_f$residuals, mat2listw(kronecker(diag(dates_len), W_qu)), 1000)
moran.mc(out_f$residuals, mat2listw(kronecker(diag(dates_len), W_kn)), 1000)


# Compare fit -------------------------------------------------------------

summary(out)
out$coefficients
plm::fixef(out)

plm_pred <- oos[, -1] %*% out$coefficients + plm::fixef(out)[11]
y_pred
oos[, 1]

op <- par(mfrow = c(1, 2))
plot(oos[, 1], y_pred)
lines(x = -1:1, y = -1:1)
plot(oos[, 1], plm_pred)
lines(x = -1:1, y = -1:1)
par(op)

op <- par(mfrow = c(1, 2))
plot(oos[, 1] - y_pred)
abline(h = 0)
plot(oos[, 1] - plm_pred)
abline(h = 0)
par(op)

cowplot::plot_grid(
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred)) %>%
    ggplot() + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = c(-0.5, 0.3)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred)) %>%
    ggplot() + geom_sf(aes(fill = prd)) +
    scale_fill_viridis_c(limits = c(-0.5, 0.3)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              plm = plm_pred) %>%
    ggplot() + geom_sf(aes(fill = plm)) +
    scale_fill_viridis_c(limits = c(-0.5, 0.3)) +
    cowplot::theme_map()
)

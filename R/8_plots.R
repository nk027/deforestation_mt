
c(plm_pred <- oos[, -1] %*% out$coefficients + plm::fixef(out))
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
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = c(-0.5, 0.1)) +
    cowplot::theme_map(),
  data %>%
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = prd)) +
    scale_fill_viridis_c(limits = c(-0.5, 0.1)) +
    cowplot::theme_map(),
  data %>%
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              plm = plm_pred) %>%
    ggplot() + geom_sf(aes(fill = plm)) +
    scale_fill_viridis_c(limits = c(-0.5, 0.1)) +
    cowplot::theme_map()
)

cowplot::plot_grid(
  data %>%
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map(),
  data %>%
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = prd)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map()
)

cowplot::plot_grid(
  data %>%
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map(),
  data %>%
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = prd)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map(),
  data %>%
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              prd16 = (y_pred_16)) %>%
    ggplot() + geom_sf(aes(fill = prd16)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map(),
  data %>%
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              prd84 = (y_pred_84)) %>%
    ggplot() + geom_sf(aes(fill = prd84)) +
    scale_fill_viridis_c(limits = c(-0.4, 0.35)) +
    cowplot::theme_map()
)

library(ggplot2)

# cowplot::plot_grid(
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2) * area_km2, 
#               prd = (y_pred_mean) * area_km2) %>% 
#     ggplot() + geom_sf(aes(fill = act)) + 
#     scale_fill_viridis_c(limits = c(-5000, 5000)) +
#     cowplot::theme_map(),
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2) * area_km2, 
#               prd = (y_pred_mean) * area_km2) %>% 
#     ggplot() + geom_sf(aes(fill = prd)) + 
#     scale_fill_viridis_c(limits = c(-5000, 5000)) +
#     cowplot::theme_map()
# )
# 
cowplot::plot_grid(
  data %>%
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = c(-0.6, 0.2)) +
    cowplot::theme_map(),
  data %>%
    filter(date == max(dates) + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred_mean)) %>%
    ggplot() + geom_sf(aes(fill = prd)) +
    scale_fill_viridis_c(limits = c(-0.6, 0.2)) +
    cowplot::theme_map()
)
# 
# cowplot::plot_grid(
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2) * area_km2, 
#               prd = (y_pred_mean) * area_km2) %>% 
#     mutate(act = ifelse(act < 0, log(-act), 0),
#            prd = ifelse(prd < 0, log(-prd), 0)) %>% 
#     ggplot() + geom_sf(aes(fill = act)) + 
#     scale_fill_viridis_c(limits = c(0, 10), direction = -1) +
#     cowplot::theme_map(),
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2) * area_km2, 
#               prd = (y_pred_mean) * area_km2) %>% 
#     mutate(act = ifelse(act < 0, log(-act), 0),
#            prd = ifelse(prd < 0, log(-prd), 0)) %>% 
#     ggplot() + geom_sf(aes(fill = prd)) + 
#     scale_fill_viridis_c(limits = c(0, 10), direction = -1) +
#     cowplot::theme_map()
# )
# 
# cowplot::plot_grid(
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2), 
#               prd = (y_pred_mean)) %>% 
#     mutate(act = ifelse(act < 0, log(-act), 0),
#            prd = ifelse(prd < 0, log(-prd), 0)) %>% 
#     ggplot() + geom_sf(aes(fill = act)) + 
#     scale_fill_viridis_c(limits = c(-10, 0), direction = 1) +
#     cowplot::theme_map(),
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2), 
#               prd = (y_pred_mean)) %>% 
#     mutate(act = ifelse(act < 0, log(-act), 0),
#            prd = ifelse(prd < 0, log(-prd), 0)) %>% 
#     ggplot() + geom_sf(aes(fill = prd)) + 
#     scale_fill_viridis_c(limits = c(-10, 0), direction = 1) +
#     cowplot::theme_map()
# )
# Plots -------------------------------------------------------------------

png("plots/rho_densities.png", width = 800, height = 400, pointsize = 18)
op <- par(mar = c(2, 2, 2, 0.5))
plot(density(results_qu[[1]]$rho_post), xlim = c(0.4, 1), ylim = c(0, 15), 
     col = "darkgreen", main = "Rho posterior densities")
for(i in 2:length(results_qu)) lines(density(results_qu[[i]]$rho_post), col = "darkgreen")
for(i in 1:length(results_kn)) lines(density(results_kn[[i]]$rho_post), col = "darkgreen")
par(op)
dev.off()

png("plots/r2_density.png", width = 1200, height = 600)
plot(density(c(sapply(results_qu, function(x) x$res_other[1, 2]),
               sapply(results_kn, function(x) x$res_other[1, 2]))), 
     main = "R2 density")
dev.off()


var <- 1
W_pre <- W_qu
date_fit <- max(dates) + 1
beta_post <- results_qu[[var]]$beta_post
rho_post <- results_qu[[var]]$rho_post
vars <- variables[[var]]
tfe <- FALSE
cfe <- TRUE
n_draws <- 1000

oos <- data %>%
  filter(date == date_fit) %>%
  ungroup() %>%
  sf:::select.sf(vars) %>% 
  sf::`st_geometry<-`(NULL) %>% 
  as.matrix(matr, rownames.force = FALSE)

# beta_post_mean <- apply(beta_post, 1, mean)
# rho_post_mean <- mean(rho_post)

y_pred <- matrix(NA, nrow = nrow(oos), ncol = n_draws)
for(i in 1:n_draws) {
  rnd <- sample(seq(1, dim(beta_post)[2]), 1)
  beta_post_draw <- beta_post[, rnd]
  rho_post_draw <- rho_post[rnd]
  
  A <- Matrix::.sparseDiagonal(nrow(W_pre)) - rho_post_draw * W_pre
  A_inv <- solve(A)
  
  B <- (1 * beta_post_draw[1] + 
       oos[, -1] %*% beta_post_draw[2:(ncol(oos))] + 
       W_pre %*% oos[, -1] %*% beta_post_draw[(1 + ncol(oos)):(2 * ncol(oos) - 1)])
  if(tfe) {
      B <- B + mean(beta_post_draw[(2 * ncol(oos)):(2 * ncol(oos) + dates_len - 2)]) # TFE
      # beta_post_draw[(2 * ncol(oos) + dates_len - 2)] # TFE
  }
  if(cfe) {
    B <- B + c(0, beta_post_draw[(2 * ncol(oos) + dates_len - 1):len(beta_post_draw)]) # CFE
  }
  
  x <- A_inv %*% B
  
  y_pred[, i] <- x[, 1]
}

y_pred_mean <- apply(y_pred, 1, quantile, c(0.16, 0.5, 0.84))[2, ]
y_pred_16 <- apply(y_pred, 1, quantile, c(0.16, 0.5, 0.84))[1, ]
y_pred_84 <- apply(y_pred, 1, quantile, c(0.16, 0.5, 0.84))[3, ]


# Analyse -----------------------------------------------------------------

plot(oos[, 1] - y_pred_mean)



# Plot --------------------------------------------------------------------

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

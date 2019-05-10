
W_pre <- W_qu
date_fit <- dates[1]
beta_post <- results_qu[[3]]$beta_post
rho_post <- results_qu[[3]]$rho_post
vars <- variables[[3]]
tfe <- TRUE
cfe <- TRUE


beta_post_mean <- apply(beta_post, 1, mean)
rho_post_mean <- mean(rho_post)

A <- Matrix::.sparseDiagonal(nrow(W_pre)) - rho_post_mean * W_pre
A_inv <- solve(A)

oos <- data %>%
  filter(date == date_fit) %>% 
  ungroup() %>%
  sf:::select.sf(vars) %>% 
  sf::`st_geometry<-`(NULL) %>% 
  as.matrix(matr, rownames.force = FALSE)

as.matrix((diag(N) - curr_rho * W) %*% y - (X %*% curr_beta))

y_pred <- A_inv %*% 
  (1 * beta_post_mean[1] + 
     oos[, -1] %*% beta_post_mean[2:(ncol(oos))] + 
     W_pre %*% oos[, -1] %*% beta_post_mean[(1 + ncol(oos)):(2 * ncol(oos) - 1)])

if(tfe) {
  y_pred <- y_pred +
    mean(beta_post_mean[(2 * ncol(oos)):(2 * ncol(oos) + dates_len - 2)]) # TFE
    # beta_post_mean[(2 * ncol(oos) + dates_len - 2)] # TFE
  if(cfe) {
    y_pred <- y_pred +
      c(0, beta_post_mean[(2 * ncol(oos) + dates_len - 1):len(beta_post_mean)]) # CFE
  }
}
y_pred


# Analyse -----------------------------------------------------------------

plot(oos[, 1] - y_pred)

curr_resid <- oos[, 1] - y_pred
SSR <- crossprod(curr_resid)
TSS <- crossprod(oos[, 1] - mean(oos[, 1]))
1 - SSR / TSS # R2


# Plot --------------------------------------------------------------------

# cowplot::plot_grid(
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2) * area_km2, 
#               prd = (y_pred) * area_km2) %>% 
#     ggplot() + geom_sf(aes(fill = act)) + 
#     scale_fill_viridis_c(limits = c(-5000, 5000)) +
#     cowplot::theme_map(),
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2) * area_km2, 
#               prd = (y_pred) * area_km2) %>% 
#     ggplot() + geom_sf(aes(fill = prd)) + 
#     scale_fill_viridis_c(limits = c(-5000, 5000)) +
#     cowplot::theme_map()
# )
# 
cowplot::plot_grid(
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred)) %>%
    ggplot() + geom_sf(aes(fill = act)) +
    scale_fill_viridis_c(limits = c(-0.5, 0.1)) +
    cowplot::theme_map(),
  data %>%
    filter(date == dates[2] + 1) %>%
    transmute(act = (forest_ch_km2),
              prd = (y_pred)) %>%
    ggplot() + geom_sf(aes(fill = prd)) +
    scale_fill_viridis_c(limits = c(-0.5, 0.1)) +
    cowplot::theme_map()
)
# 
# cowplot::plot_grid(
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2) * area_km2, 
#               prd = (y_pred) * area_km2) %>% 
#     mutate(act = ifelse(act < 0, log(-act), 0),
#            prd = ifelse(prd < 0, log(-prd), 0)) %>% 
#     ggplot() + geom_sf(aes(fill = act)) + 
#     scale_fill_viridis_c(limits = c(0, 10), direction = -1) +
#     cowplot::theme_map(),
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2) * area_km2, 
#               prd = (y_pred) * area_km2) %>% 
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
#               prd = (y_pred)) %>% 
#     mutate(act = ifelse(act < 0, log(-act), 0),
#            prd = ifelse(prd < 0, log(-prd), 0)) %>% 
#     ggplot() + geom_sf(aes(fill = act)) + 
#     scale_fill_viridis_c(limits = c(-10, 0), direction = 1) +
#     cowplot::theme_map(),
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2), 
#               prd = (y_pred)) %>% 
#     mutate(act = ifelse(act < 0, log(-act), 0),
#            prd = ifelse(prd < 0, log(-prd), 0)) %>% 
#     ggplot() + geom_sf(aes(fill = prd)) + 
#     scale_fill_viridis_c(limits = c(-10, 0), direction = 1) +
#     cowplot::theme_map()
# )

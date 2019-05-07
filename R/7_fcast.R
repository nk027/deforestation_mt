
sdm_fit <- function(
  x,
  dates_len,
  date,
  beta_post,
  rho_post,
  tfe,
  cfe
) {
  
  beta_post_mean <- apply(beta_post, 1, mean)
  rho_post_mean <- mean(rho_post)
  
  A <- Matrix::.sparseDiagonal(nrow(W_pre)) - rho_post_mean * W_pre
  A_inv <- solve(A)
  
  oos <- data %>%
    filter(date == date) %>% 
    ungroup() %>%
    sf:::select.sf(forest_ch_km2, 
                   forest_px_km2, pasture_px_km2, crop_px_km2,
                   pop_km2, gdp_cap,
                   milk_brl_cow, cattle_dens, max_yield_brl,
                   spei_wet, spei_dry) %>% 
    sf::`st_geometry<-`(NULL) %>% 
    as.matrix(matr, rownames.force = FALSE)
  
  y_pred <- A_inv %*% 
    (1 * beta_post_mean[1] + 
       oos[, -1] %*% beta_post_mean[2:(ncol(oos))] + 
       W_pre %*% oos[, -1] %*% beta_post_mean[(1 + ncol(oos)):(2 * ncol(oos) - 1)])
  
  if(tfe) {
    y_pred <- y_pred +
      # mean(beta_post_mean[(2 * ncol(oos)):(2 * ncol(oos) + ncol(TFE) - 2)]) + # TFE
      beta_post_mean[(2 * ncol(oos) + dates_len - 2)] # TFE
    if(cfe) {
      y_pred <- y_pred +
        c(0, beta_post_mean[(2 * ncol(oos) + dates_len - 1):len(beta_post_mean)]) # CFE
    }
  }
}


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
# cowplot::plot_grid(
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2), 
#               prd = (y_pred)) %>% 
#     ggplot() + geom_sf(aes(fill = act)) + 
#     scale_fill_viridis_c(limits = c(-0.6, 0.4)) +
#     cowplot::theme_map(),
#   data %>% 
#     filter(date == dates[2] + 1) %>% 
#     transmute(act = (forest_ch_km2), 
#               prd = (y_pred)) %>% 
#     ggplot() + geom_sf(aes(fill = prd)) + 
#     scale_fill_viridis_c(limits = c(-0.6, 0.4)) +
#     cowplot::theme_map()
# )
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

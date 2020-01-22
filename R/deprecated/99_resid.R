
bayesian_resid <- function(
  x, rmse = TRUE,
  vars, results, W, dates_len,
  lag_X = TRUE, tfe = TRUE, cfe = TRUE, tfe_idx = NULL,
  n_draws = 1000) {
  
  beta_post <- results$beta_post
  rho_post <- results$rho_post
  
  resid <- matrix(NA, nrow = nrow(x), ncol = n_draws)
  rnd <- sample(seq(1, dim(beta_post)[2]), replace = TRUE, n_draws)
  
  y <- x[, 1]
  X <- x[, -1]
  if(lag_X) {X <- cbind(X, W %*% X)}
  X <- cbind(1, X)
  
  for(i in 1:n_draws) {
    beta_post_draw <- beta_post[, rnd[i]]
    rho_post_draw <- rho_post[rnd[i]]
    
    A <- Matrix::.sparseDiagonal(nrow(W)) - rho_post_draw * W
    
    beta <- beta_post_draw[1:ncol(X)]
    
    resid[, i] <- ((A %*% y - X %*% beta) + 
                     if(tfe) {
                       if(is.null(tfe_idx)) {
                         mean(beta_post_draw[(ncol(X) + 1):(ncol(X) + 1 + dates_len - 2)])
                       } else {
                         c(0, beta_post_draw[(ncol(X) + 1):(ncol(X) + 1 + dates_len - 2)])[tfe_idx]
                       }
                     } else {0} + 
                     if(cfe) {
                       c(0, beta_post_draw[(ncol(X) + 1 + dates_len - 1):length(beta_post_draw)])
                     })[, 1]
  }
  
  if(rmse) {
    return(sqrt(crossprod(apply(resid, 1, mean)) / length(y))[[1]])
  }
  
  return(resid)
}

bayesian_resid2 <- function(
  x, rmse = TRUE,
  vars, results, dates_len,
  tfe = TRUE, cfe = TRUE, tfe_idx = NULL,
  n_draws = 1000) {
  
  beta_post <- results$beta_post

  resid <- matrix(NA, nrow = nrow(x), ncol = n_draws)
  rnd <- sample(seq(1, dim(beta_post)[2]), replace = TRUE, n_draws)
  
  y <- x[, 1]
  X <- x[, -1]
  X <- cbind(1, X)
  
  for(i in 1:n_draws) {
    beta_post_draw <- beta_post[, rnd[i]]

    beta <- beta_post_draw[1:ncol(X)]
    
    resid[, i] <- ((y - X %*% beta) + 
                     if(tfe) {
                       if(is.null(tfe_idx)) {
                         mean(beta_post_draw[(ncol(X) + 1):(ncol(X) + 1 + dates_len - 2)])
                       } else {
                         c(0, beta_post_draw[(ncol(X) + 1):(ncol(X) + 1 + dates_len - 2)])[tfe_idx]
                       }
                     } else {0} + 
                     if(cfe) {
                       c(0, beta_post_draw[(ncol(X) + 1 + dates_len - 1):length(beta_post_draw)])
                     })[, 1]
  }
  
  if(rmse) {
    return(sqrt(crossprod(apply(resid, 1, mean)) / length(y))[[1]])
  }
  
  return(resid)
}


# rep(c("", "-lim"), each = 5))
tfe <- cfe <- TRUE

rmats <- list()

counter <- which(names(variables) == "base")
for(counter in seq_along(variables)) {

  rmat <- matrix(NA, nrow = 8, ncol = length(dates) + 1)
  colnames(rmat) <- c(dates, max(dates) + 1)
  rownames(rmat) <- paste0(rep(c("sdm-qu", "sdm-k5", "sdm-k7", 
                                 "sar-qu", "sar-k5", "sar-k7",
                                 "sem-qu", "clm")))#, 2)),
  
for(date_fit in c(dates, max(dates) + 1)) {
  
  tfe_idx <- if(date_fit %in% dates) {which(dates == date_fit)} else {NULL}
  
  oos <- prep_fit(data, date_fit, variables[[counter]])
  
  sdm_qu_rmse <- bayesian_resid(oos, rmse = TRUE, variables[[counter]], sdm_qu[[counter]],
                                W_qu, dates_len, lag_X = TRUE, 
                                tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)

  sdm_k5n_rmse <- bayesian_resid(oos, rmse = TRUE,  variables[[counter]], sdm_k5[[counter]],
                                 W_k5n, dates_len, lag_X = TRUE, 
                                 tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)

  sdm_k7n_rmse <- bayesian_resid(oos, rmse = TRUE,  variables[[counter]], sdm_k7[[counter]],
                                 W_k7n, dates_len, lag_X = TRUE, 
                                 tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)

  sar_qu_rmse <- bayesian_resid(oos, rmse = TRUE,  variables[[counter]], sar_qu[[counter]],
                                W_qu, dates_len, lag_X = FALSE, 
                                tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)

  sar_k5n_rmse <- bayesian_resid(oos, rmse = TRUE,  variables[[counter]], sar_k5[[counter]],
                                 W_k5n, dates_len, lag_X = FALSE, 
                                 tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)

  sar_k7n_rmse <- bayesian_resid(oos, rmse = TRUE,  variables[[counter]], sar_k7[[counter]],
                                 W_k7n, dates_len, lag_X = FALSE, 
                                 tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)

  sem_qu_rmse <- bayesian_resid2(oos, rmse = TRUE,  variables[[counter]], sem_qu[[counter]],
                                 dates_len, 
                                 tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
  clm_rmse <- bayesian_resid2(oos, rmse = TRUE,  variables[[counter]], clm[[counter]],
                                 dates_len, 
                                 tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
  
  rmat[1:8, as.character(date_fit)] <- c(sdm_qu_rmse, sdm_k7n_rmse, sdm_k5n_rmse, 
                                        sar_qu_rmse, sar_k7n_rmse, sar_k5n_rmse, 
                                        sem_qu_rmse, clm_rmse)
}
  
  rmats[[counter]] <- rmat
  write.csv(rmats[[counter]], 
            paste0("txt/rmse_yearly_", names(variables)[[counter]], ".csv"))
  

}
  

# Plots -------------------------------------------------------------------

library(ggplot2)

counter <- 1
# for(counter in seq_along(variables)) {

df <- as.data.frame(rmats[[counter]])
# df <- as.data.frame(rmats[[counter]][c(1, 3, 4, 6, 8), ])
df$model <- toupper(rownames(rmats[[counter]]))
# df$model <- toupper(rownames(rmats[[counter]][c(1, 3, 4, 6, 8), ]))

df <- reshape2::melt(df)

df$model <- factor(df$model, levels = unique(df$model))
df$variable <- factor(df$variable, levels = unique(df$variable))


colours = c(rgb(  0, 102, 156, maxColorValue = 255),
            rgb(255, 255, 255, maxColorValue = 255),
            rgb(165,   0,  33, maxColorValue = 255))

palette = colorRampPalette(colours)
col_vector = palette(256)

df <- df %>%
  filter(model %in% c("SDM-QU", "SDM-K7", "SAR-QU", "SEM-QU", "CLM"))
lims <- rev(c("SDM-QU", "SDM-K7", "SAR-QU", "SEM-QU", "CLM"))
# lims <- rev(toupper(rownames(rmats[[counter]])))

label = ifelse(is.na(df$value), NA, sprintf("%.1f%%", df$value * 100))
label = round(df$value, 2)

ggplot(df, aes(x = variable, y = model)) +
  geom_tile(aes(fill = value, alpha = 0.5),
            size = 1, width = 0.975, height = 0.975) +
  geom_text(aes(label = label), colour = rgb(64, 64, 64, maxColorValue = 255)) +
  # scale_fill_gradientn(colours = col_vector, na.value = "white") +
  scale_fill_viridis_c() +
  # scale_y_discrete(expand = c(0, 0), limits = rev(toupper(rownames(rmats[[counter]])))) +
  scale_y_discrete(expand = c(0, 0),
                   limits = lims) +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  labs(title = NULL) + xlab(NULL) + ylab(NULL) +
  coord_equal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(
    text = element_text(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    plot.margin = unit(c(0.5, 1, 0, 0), "lines"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text.x = element_text(hjust = 0, angle = 90))

ggsave("plots/rmse_plot.png", width = 15, height = 7, units = "cm")
# ggsave("plots/rmse_plot.tiff", width = 15, height = 6, units = "cm")
# ggsave(paste0("plots/rmse_yearly_", names(variables)[[counter]], ".png"),
#        width = 15, height = 9, units = "cm")

# }



# Dependencies ------------------------------------------------------------

stopifnot(
  exists("data"), exists("variables"), exists("dates"), # etc., 31
  exists("prep_fit"), exists("bayesian_fit"), exists("sm_results"), # etc., 40
  require("dplyr"),
  require("plm"),
  require("splm"),
  require("spatialreg"),
  require("reshape2"),
  require("ggplot2")
)

load("data/models_twoways.rda")
# load("data/models_time.rda")
# load("data/models_none.rda")


# RMSE Matrix -------------------------------------------------------------

mat <- matrix(NA, nrow = 8, ncol = length(dates) + 1)
colnames(mat) <- c(dates, max(dates) + 1)
rownames(mat) <- paste0(rep(c("sdm-qu", "sdm-k7", "sdm-k5", "sar-qu", "sar-k5", 
                              "sem-qu", "sem-k5", "clm")))#, 2)), 
                        # rep(c("", "-lim"), each = 5))

counter <- which(names(variables) == "base")
for(date_fit in c(dates, max(dates) + 1)) {
  
  tfe_idx <- if(date_fit %in% dates) {which(dates == date_fit)} else {NULL}
  
  oos <- prep_fit(data, date_fit, variables[[counter]])
  
  
  sdm_qu_fit <- bayesian_fit(oos, variables[[counter]], results_qu[[counter]],
                             W_qu, dates_len, lag_X = TRUE, 
                             tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
  sdm_qu_fit_mean <- apply(sdm_qu_fit, 1, mean)
  
  sdm_k7n_fit <- bayesian_fit(oos, variables[[counter]], results_k7n[[counter]],
                              W_k7n, dates_len, lag_X = TRUE, 
                              tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
  sdm_k7n_fit_mean <- apply(sdm_k7n_fit, 1, mean)
  
  sdm_k5n_fit <- bayesian_fit(oos, variables[[counter]], results_k5n[[counter]],
                              W_k5n, dates_len, lag_X = TRUE, 
                              tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
  sdm_k5n_fit_mean <- apply(sdm_k5n_fit, 1, mean)
  
  clm_fit <- plm_fit(oos, results_plm[[counter]], tfe, cfe, tfe_idx = tfe_idx)
  
  sar_qu_fit <- splm_fit(oos, results_lag_qu[[counter]], W_qu, 
                         tfe, cfe, tfe_idx = tfe_idx)
  
  sar_k5n_fit <- splm_fit(oos, results_lag_k5n[[counter]], W_k5n, 
                          tfe, cfe, tfe_idx = tfe_idx)
  
  sem_qu_fit <- splm_fit(oos, results_err_qu[[counter]], W_qu, 
                         tfe, cfe, tfe_idx = tfe_idx)
  
  sem_k5n_fit <- splm_fit(oos, results_err_k5n[[counter]], W_k5n, 
                          tfe, cfe, tfe_idx = tfe_idx)
  
  mat[1:8, as.character(date_fit)] <- sapply(
    list(sdm_qu_fit_mean, sdm_k7n_fit_mean, sdm_k5n_fit_mean, 
         sar_qu_fit, sar_k5n_fit, sem_qu_fit, sem_k5n_fit, clm_fit), 
    rmse, oos[, 1])
  
}

# counter <- which(names(variables) == "base_lim")
# for(date_fit in c(dates, max(dates) + 1)) {
#   
#   tfe_idx <- if(date_fit %in% dates) {which(dates == date_fit)} else {NULL}
#   
#   oos <- prep_fit(data, date_fit, variables[[counter]])
#   
#   
#   sdm_qu_fit <- bayesian_fit(oos, variables[[counter]], results_qu[[counter]],
#                              W_qu, lag_X = TRUE, tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
#   sdm_qu_fit_mean <- apply(sdm_qu_fit, 1, mean)
#   
#   sdm_k5n_fit <- bayesian_fit(oos, variables[[counter]], results_k5n[[counter]],
#                               W_k5n, lag_X = TRUE, tfe = tfe, cfe = cfe, tfe_idx = tfe_idx)
#   sdm_k5n_fit_mean <- apply(sdm_k5n_fit, 1, mean)
#   
#   clm_fit <- plm_fit(oos, results_plm[[counter]], tfe, cfe, tfe_idx = tfe_idx)
#   
#   sar_qu_fit <- splm_fit(oos, results_lag_qu[[counter]], W_qu, tfe, cfe, tfe_idx = tfe_idx)
#   
#   sar_k5n_fit <- splm_fit(oos, results_lag_k5n[[counter]], W_k5n, tfe, cfe, tfe_idx = tfe_idx)
#   
#   sem_qu_fit <- splm_fit(oos, results_err_qu[[counter]], W_qu, tfe, cfe, tfe_idx = tfe_idx)
#   
#   sem_k5n_fit <- splm_fit(oos, results_err_k5n[[counter]], W_k5n, tfe, cfe, tfe_idx = tfe_idx)
#   
#   mat[6:10, as.character(date_fit)] <- sapply(
#     list(sdm_qu_fit_mean, sdm_k5n_fit_mean, sar_qu_fit, sem_qu_fit, clm_fit), 
#     rmse, oos[, 1])
# }


df <- as.data.frame(mat)
# df <- as.data.frame(mat[c(1, 3, 4, 6, 8), ])
df$model <- toupper(rownames(mat))
# df$model <- toupper(rownames(mat[c(1, 3, 4, 6, 8), ]))

df <- reshape2::melt(df)

df$model <- factor(df$model, levels = unique(df$model))
df$variable <- factor(df$variable, levels = unique(df$variable))


colours = c(rgb(  0, 102, 156, maxColorValue = 255),
            rgb(255, 255, 255, maxColorValue = 255),
            rgb(165,   0,  33, maxColorValue = 255))

palette = colorRampPalette(colours)
col_vector = palette(256)

df <- df %>%
  filter(!model %in% c("SDM-K7", "SAR-K5", "SEM-K5"))

label = ifelse(is.na(df$value), NA, sprintf("%.1f%%", df$value * 100))
label = round(df$value, 2)

ggplot(df, aes(x = variable, y = model)) +
  geom_tile(aes(fill = value, alpha = 0.5),
            size = 1, width = 0.975, height = 0.975) +
  geom_text(aes(label = label), colour = rgb(64, 64, 64, maxColorValue = 255)) +
  # scale_fill_gradientn(colours = col_vector, na.value = "white") +
  scale_fill_viridis_c() +
  # scale_y_discrete(expand = c(0, 0), limits = rev(toupper(rownames(mat)))) +
  scale_y_discrete(expand = c(0, 0),
                    limits = rev(toupper(rownames(mat[c(1, 3, 4, 6, 8), ])))) +
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
    axis.text.x = element_text(hjust = 0))

ggsave("plots/rmse_plot.tiff", width = 15, height = 6, units = "cm")
# ggsave("plots/rmse_plot_full.png", width = 15, height = 9, units = "cm")


detach("package:dplyr")
detach("package:plm")
detach("package:splm")
detach("package:spatialreg")
detach("package:reshape2")
detach("package:ggplto2")

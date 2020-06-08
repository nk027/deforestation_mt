
# Dependencies ------------------------------------------------------------

stopifnot(
  exists("data"), exists("variables"), exists("dates"), # etc., 55
  exists("predict.clm"), exists("predict.sar"), # etc., 52
  require("dplyr"),
  require("spatialreg"),
  require("reshape2"),
  require("ggplot2")
)


# RMSE Matrix -------------------------------------------------------------

load(file = paste0("data/est_base_qu.rda"))

mat <- matrix(NA, nrow = 4, ncol = length(dates) + 1)
colnames(mat) <- c(dates, max(dates) + 1)
rownames(mat) <- paste0(rep(c("sdm", "sar", "slx", "clm")))

tfe <- cfe <- TRUE
vars <- variables[["base"]]
mdls <- list(out_sdm, out_sar, out_slx, out_clm)

# in-sample
for(i in 1:4) {
  res <- apply(residuals(mdls[[i]]), 1, mean)
  mat[i, -ncol(mat)] <- sapply(seq(1, mdls[[i]]$meta$N, 141), function(x) {
    sqrt(crossprod(res[x:(x + 140)]) / 141)
  })
}

# out-of-sample
data_oos <- get_matrix(data, vars, 2017)

for(i in 1:4) {
  pred <- apply(predict(mdls[[i]], newdata = data_oos), 1, mean)
  # TFE
  pred <- pred + mean(data_oos[, 1] - pred)
  mat[i, ncol(mat)] <- sqrt(crossprod(data_oos[, 1] - pred) / 141)
}

mat


# Plot --------------------------------------------------------------------

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

label = ifelse(is.na(df$value), NA, sprintf("%.1f%%", df$value * 100))
label = round(df$value, 2)

ggplot(df, aes(x = variable, y = model)) +
  geom_tile(aes(fill = value, alpha = 0.5),
            size = 1, width = 0.975, height = 0.975) +
  geom_text(aes(label = label), colour = rgb(64, 64, 64, maxColorValue = 255)) +
  # scale_fill_gradientn(colours = col_vector, na.value = "white") +
  scale_fill_viridis_c() +
  # scale_y_discrete(expand = c(0, 0), limits = rev(toupper(rownames(mat)))) +
  scale_y_discrete(expand = c(0, 0)) +
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
ggsave("plots/rmse_plot.png", width = 15, height = 6, units = "cm")


detach("package:dplyr")
detach("package:spatialreg")
detach("package:reshape2")
detach("package:ggplot2")


# Dependencies ------------------------------------------------------------

stopifnot(
  file.exists("data/data.rds"),
  require("Matrix"),
  require("sf"),
  require("dplyr"),
  require("spatialreg")
)

source("R/50_estimation.R")
source("R/51_supp.R")
source("R/52_helpers.R")


# Prep data ---------------------------------------------------------------

data <- readRDS("data/data.rds")

dates <- 2006:2017
dates_len <- length(dates)
# Weights
Ws <- list(
  qu = get_weights(data, type = "queen"),
  k5 = get_weights(data, type = "knear", k = 5),
  k7 = get_weights(data, type = "knear", k = 7),
  k9 = get_weights(data, type = "knear", k = 9)
)


# Prep sampler ------------------------------------------------------------

n_rho <- 500
n_draw <- 25000L
n_burn <- 5000L
sigma_a <- 2
sigma_b <- 1
beta_mean <- 0
beta_var <- 10 ^ 6
rho_a <- 1.01


# Execute -----------------------------------------------------------------

model <- list("extended" = c("forest_ch_km2",
  "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
  "cattle_dens_lag_log", "soy_filled_lag",
  "pop_km2_lag_log", "spei_dry"))
weights <- "qu"
re_grid <- FALSE
agr_interact <- FALSE
time_interact <- FALSE

# Estimate ----------------------------------------------------------------



x <- get_matrix(data,
  if(agr_interact) {c(model[[1]], "biome_a")} else {model[[1]]}, dates)

if(agr_interact) {
  y <- x[, 1]
  x <- x[, -1]
  int <- int2 <- x[, grepl("(pasture|crop|cattle|soy)", colnames(x))]
  colnames(int) <- paste0(colnames(int), "_a")
  colnames(int2) <- paste0(colnames(int2), "_b")
  ind <- x[, grepl("biome_a", colnames(x))]
  x <- x[, !grepl("(pasture|crop|cattle|soy|_a)", colnames(x))]
  x <- cbind(y, x, int * ind, int2 * abs(ind - 1))
}
if(time_interact) {
  y <- x[, 1]
  x <- x[, -1]
  int <- int2 <- x[, grepl("(pasture|crop|cattle|soy)", colnames(x))]
  colnames(int) <- paste0(colnames(int), "_pre")
  colnames(int2) <- paste0(colnames(int2), "_post")
  ind <- c(rep(1, nrow(x) / 2), rep(0, nrow(x) / 2))
  x <- x[, !grepl("(pasture|crop|cattle|soy|_a)", colnames(x))]
  x <- cbind(y, x, int * ind, int2 * abs(ind - 1))
}
var_names <- colnames(x)[-1]

if(!exists("rho_grid") || re_grid) {
  rho_grid <- get_grid(NULL, W_pre = Ws[[weights]],
    y = get_matrix(data, "forest_ch_km2", dates),
    N = nrow(x), n_rho = n_rho, type = "eigen")
  saveRDS(rho_grid, paste0("data/rho_grid_", weights, ".rds"))
}

out_sdm <- sar(x, Ws[[weights]], LX = TRUE,
  n_draw = n_draw, n_burn = n_burn, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time = length(dates),
  sigma_a = sigma_a, sigma_b = sigma_b,
  beta_mean = beta_mean, beta_var = beta_var, rho_a = rho_a,
  grid = rho_grid, verbose = TRUE)
out_sar <- sar(x, Ws[[weights]], LX = FALSE,
  n_draw = n_draw, n_burn = n_burn, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time = length(dates),
  sigma_a = sigma_a, sigma_b = sigma_b,
  beta_mean = beta_mean, beta_var = beta_var, rho_a = rho_a,
  grid = rho_grid, verbose = TRUE)
out_slx <- clm(x, Ws[[weights]], LX = TRUE,
  n_draw = n_draw, n_burn = n_burn, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time = length(dates),
  sigma_a = sigma_a, sigma_b = sigma_b,
  beta_mean = beta_mean, beta_var = beta_var,
  verbose = TRUE)
out_clm <- clm(x, LX = FALSE,
  n_draw = n_draw, n_burn = n_burn, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time = length(dates),
  sigma_a = sigma_a, sigma_b = sigma_b,
  beta_mean = beta_mean, beta_var = beta_var,
  verbose = TRUE)

save(out_sdm, out_sar, out_slx, out_clm,
  file = paste0("data/est_", names(model)[[1]], "_", weights,
    if(agr_interact) {"_int"}, if(time_interact) {"_split"}, ".rda"))


# Estimate ----------------------------------------------------------------

library("coda")

summary.sar <- summary.clm <- function(x,
  var_names = x$meta$var_names, p = 0.95) {
  pos <- !colnames(x$beta) %in% c("ife", "tfe")
  out <- t(apply(x$beta[, pos], 2, function(y) {
    hpd <- HPDinterval(mcmc(y), prob = p)
    c("low" = hpd[1], "mean" = mean(y), "up" = hpd[2])
  }))
  if(!is.null(var_names)) {
    rn <- rownames(out)
    rownames(out)[rn == "beta"] <- paste0(var_names, "_dir")
    rownames(out)[rn == "theta"] <- paste0(var_names, "_ind")
  }
  cbind(out, sign(out[, 1]) == sign(out[, 3]))
}
te.sar <- function(x, var_names = x$meta$var_names, p = 0.95) {
  rhos <- c(mean(x$rho), HPDinterval(mcmc(x$rho), p = p))[c(2, 1, 3)]
  betas <- t(apply(x$beta, 2, function(y) {
    hpd <- HPDinterval(mcmc(y), prob = p)
    c("low" = hpd[1], "mean" = mean(y), "up" = hpd[2])
  }))
  total <- matrix(NA_real_, nrow(betas), ncol(betas))
  for(i in seq_along(rhos)) {
    B <- solve(diag(x$meta$N) - rhos[i] * x$meta$W)
    total[, i] <- sum(B) / x$meta$N * betas[, i]
  }
  total
}
plot.sar <- plot.clm <- function(x,
  var_names = x$meta$var_names, p = 0.95) {
  x_sm <- summary(x, var_names, p)
  op <- par(mfrow = c(5, 5), mar = c(2, 2, 2, 0.5))
  for(i in seq(nrow(x_sm))) {
    densplot(mcmc(x$beta[, i]), main = rownames(x_sm)[i])
    abline(v = 0, col = "#800080")
    abline(v = x_sm[i, c(1, 3)], col = "darkgray", lty = 3)
    abline(v = x_sm[i, 2], col = "#008080", lty = 2)
  }
  par(op)
}
summary(out_sdm)
eff <- effects.sar(out_sdm)
plot(out_sdm, var_names)

summary(out_sar, var_names)
summary(out_slx, var_names)
summary(out_clm, var_names)

# Plots ---

library("tmap")

tm <- data %>% filter(date %in% 2006:2017) %>%
  mutate(forest_ch = forest_ch / 100) %>%
  tm_shape() +
  tm_borders(alpha = 1, col = "#e3e3e3") +
  tm_fill("forest_ch", midpoint = 0, palette = "RdGy",
    title = "Forest change (km²)",
    breaks = c(-50000, -40000, -30000, -20000, -10000, -2500,
      2500, 10000, 20000) / 100) +
  tm_facets(by = "date") +
  tm_layout(legend.outside = TRUE, outer.margins = 0,
    legend.text.size = 0.45, bg.color = "transparent",
    legend.outside.position = "right", legend.outside.size = .175)
# print(tm)
tmap_save(tm, "outputs/forest_change.png",
  height = 4, width = 5, bg = "transparent")
tmap_save(tm, "outputs/forest_change.pdf",
  height = 4, width = 5, bg = "transparent")


map <- st_read("data/municipios") %>%
  mutate(mt = CD_GEOCMU > 5050000 & CD_GEOCMU < 5200000)
bra <- st_union(map)
mt <- st_union(map %>% filter(mt))
biome <- st_read("data/biomes_brazil") %>%
  select(name = Name) %>% st_transform(st_crs(mt))
biome_mt <- st_intersection(biome, mt)

tm <- tm_shape(bra) +
  tm_borders() +
tm_shape(biome_mt) +
  tm_fill("name", alpha = 1, title = "Biome", legend.show = FALSE,
    palette = c("#8dd3c7", "#ffffb3", "#bebada")) +
tm_shape(biome %>%
  mutate(name = ifelse(grepl("Cerr|Pant|Amaz", name), name, "Other"))) +
  tm_fill("name", alpha = 0.5, legend.show = FALSE,
    palette = c("#8dd3c7", "#ffffb3", "#e3e3e3", "#bebada")) +
tm_add_legend(type = "fill", size = 3,
  col = c("#8dd3c7", "#ffffb3", "#e3e3e3", "#bebada"),
  labels = c("Amazônia", "Cerrado", "Other", "Pantanal"), title = "Biome") +
tm_shape(mt) +
  tm_borders(lwd = 2) +
  tm_scale_bar(c(0, 500, 1000), position = "left", text.size = 0.9) +
tm_layout(outer.margins = 0, bg.color = "transparent",
  legend.outside.position = "right",
  legend.text.size = 0.9, legend.title.size = 1.2)
# print(tm)
tmap_save(tm, "outputs/brazil_location.png",
  height = 4, width = 5, bg = "transparent")
tmap_save(tm, "outputs/brazil_location.pdf",
  height = 4, width = 5, bg = "transparent")

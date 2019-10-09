
library("dplyr")
library("sf")

get_matr <- function(x, variables, dates) {

  x %>%
    filter(date %in% dates) %>%
    ungroup() %>%
    sf:::select.sf(variables) %>%
    sf::`st_geometry<-`(NULL) %>%
    as.matrix(matr, rownames.force = FALSE)
}

get_W <- function(x, type = c("queen", "knear"), k = 5) {

  type <- match.arg(type)
  nb <- x %>% dplyr::filter(date == 2015) %>% sf::as_Spatial()

  if(type == "queen") {
    return(
      spdep::listw2mat(
        spdep::nb2listw(
          spdep::poly2nb(nb, row.names = nb$code, queen = TRUE), style = "W"))
    )
  } else if(type == "knear") {
    return(
      spdep::listw2mat(
        spdep::nb2listw(
          spdep::knn2nb(
            spdep::knearneigh(sp::coordinates(nb), k = k))))
    )
  }
}


data <- readRDS("data/data.rds")

dates <- seq(2006, 2016)
dates_len <- length(dates)

variables <- list(
  full = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
    "pop_km2_lag", "gdp_cap_lag", "cattle_dens_lag", "soy_filled_lag",
    "spei_wet_lag", "spei_dry_lag"),
  lim1 = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
    "cattle_dens_lag", "soy_filled_lag",
    "spei_wet_lag", "spei_dry_lag"),
  lim2 = c("forest_ch_km2",
    "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
    "pop_km2_lag", "spei_wet_lag"),
  lim3 = c("forest_ch_km2",
     "forest_px_km2_lag", "pasture_px_km2_lag", "crop_px_km2_lag",
     "cattle_dens_lag", "soy_filled_lag")
)

matrices <- list()
for(i in seq_along(variables)) {
  matrices[[i]] <- get_matr(data, variables[[i]], dates = dates)
}
names(matrices) <- names(variables)


Ws <- list(
  queen = get_W(data, type = "queen"),
  k5 = get_W(data, type = "knear", k = 5),
  k7 = get_W(data, type = "knear", k = 7)
)

fixed_effects <- list( # TFE, CFE
  full = c(TRUE, TRUE),
  time = c(TRUE, FALSE),
  none = c(FALSE, FALSE)
)

functions <- list(
  sdm = sdm_panel,
  sar = sar_panel,
  sem = sem_panel,
  clm = clm_panel
)

# MCMC
n_iter <- 25000
n_save <- 10000
n_griddy <- 2000
# Priors
rho_a <- 1.01
sigma_a <- 0.01
sigma_b <- 0.01
beta_mean <- 0
beta_var <- 10 ^ 8

calls <- list()

j <- 1
for(i_fe in seq_along(fixed_effects)) {
  for(i_var in seq_along(variables)) {
    for(i_W in seq_along(Ws))
      for(i_funs in seq_along(functions)) {
        calls[[j]] <- list(
          fun = functions[[i_funs]],
          matr = matrices[[i_var]],
          W = Ws[[i_W]],
          fe = fixed_effects[[i_fe]]
        )
        j <- j + 1
      }
  }
}

rm(i, j, i_fe, i_var, i_W, i_funs, get_W, get_matr)
save.image("data/setup_cluster.R")

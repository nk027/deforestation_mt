
# e.g. get_matrix(x, c("forest_ch_km2", "forest_px_km2"), 2010:2015)
get_matrix <- function(x, variables, dates) {
  x %>%
    filter(date %in% dates) %>%
    ungroup() %>%
    sf:::select.sf(variables) %>%
    sf::`st_geometry<-`(NULL) %>%
    as.matrix(matr, rownames.force = FALSE)
}

# Get a spatial weights matrix (queen contiguity or k-nearest neighbours)
# e.g. get_weights(x, "queen")
get_weights <- function(x, type = c("queen", "knear"), k = 5) {
  if(!requireNamespace("spdep", quietly = TRUE)) {stop("Requires `spdep`")}
  type <- match.arg(type)
  nb <- x %>% dplyr::filter(date == 2010) %>% sf::as_Spatial()
  if(type == "queen") {
    return(spdep::listw2mat(spdep::nb2listw(spdep::poly2nb(
      nb, row.names = nb$code, queen = TRUE), style = "W")))
  } else if(type == "knear") {
    if(!requireNamespace("sp", quietly = TRUE)) {stop("Requires `sp`")}
    return(spdep::listw2mat(spdep::nb2listw(spdep::knn2nb(
      spdep::knearneigh(sp::coordinates(nb), k = k)))))
  }
}


effects.sar <- function(x, n_draw = 100, names) {

  if(missing(n_draw)) {n_draw <- x$meta$n_save}
  draws <- sample(x$meta$n_save, n_draw, replace = FALSE)

  rhos <- x$rho[draws]
  betas <- x$beta[draws, ]
  W <- x$meta$W
  N <- x$meta$N
  LX <- x$meta$LX
  index <- colnames(x$beta)
  k <- sum(colnames(x$beta) == "beta") + 1

  eff_dir <- eff_ind <- matrix(NA, nrow = n_draw, ncol = k)
  dimnames(eff_dir)[[2]] <- dimnames(eff_ind)[[2]] <-
    if(!missing(names)) {names} else {index[seq(k)]}

  for(i in seq_len(n_draw)) {
    B <- solve(Matrix::.sparseDiagonal(N) - rhos[i] * W)
    eff_dir[i, ] <- sum(diag(B)) / N * betas[i, index %in% c("alpha", "beta")] +
      if(LX) {
        c(0, sum(diag(B %*% W)) / N * betas[i, index == "theta"])
      } else {0}
    eff_tot <- sum(B) / N * betas[i, index %in% c("alpha", "beta")] +
      if(LX) {
        c(0, sum(B %*% W) / N * betas[i, index == "theta"])
      } else {0}
    eff_ind[i, ] <- eff_tot - eff_dir[i, ]
  }

  return(list("direct" = eff_dir, "indirect" = eff_ind))
}


logLik.sar <- function(x, fun = mean) {
  rho <- fun(x$rho)
  beta <- apply(x$beta, 2, fun)
  sigma <- fun(x$sigma)

  A <- Matrix::.sparseDiagonal(x$meta$N) - rho * x$meta$W
  ldet <- log(det(A))
  ESS <- as.numeric(crossprod(A %*% x$meta$y - x$meta$X %*% beta))
  N <- x$meta$N

  ldet - ((N / 2) * log(2 * pi)) - (N / 2) * log(sigma) - (ESS / (2 * sigma))
}


logLik.clm <- function(x, fun = mean) {
  beta <- apply(x$beta, 2, fun)
  sigma <- fun(x$sigma)
  N <- x$meta$N

  -(N / 2) * log(2 * pi) - (N / 2) * log(sigma) - as.numeric(crossprod(x$meta$y - x$meta$X %*% beta)) / (2 * sigma)
}


BIC.sar <- BIC.clm <- function(x, fun) {-2 * logLik(x) + x$meta$K * log(x$meta$N)}


predict.sar <- function(x, n_draw = 100, newdata) {

  if(missing(n_draw)) {n_draw <- x$meta$n_save}
  draws <- sample(x$meta$n_save, n_draw, replace = FALSE)
  N <- nrow(newdata)

  rhos <- x$rho[draws]
  betas <- x$beta[draws, ]
  W <- x$meta$W[seq(N), seq(N)]
  X <- build_X(newdata, const = TRUE,
    tfe = x$meta$tfe, ife = x$meta$ife, n_time = 1,
    W = if(x$meta$LX) {W} else {NULL})
  if(x$meta$tfe) {
    X <- cbind(X[, colnames(X) != "ife"],
      matrix(0, nrow = nrow(X), ncol = sum(colnames(betas) == "tfe")),
      X[, colnames(X) == "ife"])
  }

  pred <- matrix(NA, nrow = N, ncol = n_draw)
  for(i in seq_len(n_draw)) {
    A <- Matrix::.sparseDiagonal(N) - rhos[i] * W
    pred[, i] <- as.numeric(solve(A, X %*% betas[i, ]))
  }

  return(pred)
}


predict.clm <- function(x, n_draw, newdata) {

  if(missing(n_draw)) {n_draw <- x$meta$n_save}
  draws <- sample(x$meta$n_save, n_draw, replace = FALSE)
  N <- nrow(newdata)

  betas <- x$beta[draws, ]
  W <- x$meta$W[seq(N), seq(N)]
  X <- build_X(newdata, const = TRUE,
    tfe = x$meta$tfe, ife = x$meta$ife, n_time = 1, W = W)
  if(x$meta$tfe) {
    X <- cbind(X[, colnames(X) != "ife"],
      matrix(0, nrow = nrow(X), ncol = sum(colnames(betas) == "tfe")),
      X[, colnames(X) == "ife"])
  }

  pred <- matrix(NA, nrow = N, ncol = n_draw)
  for(i in seq_len(n_draw)) {
    pred[, i] <- as.numeric(X %*% betas[i, ])
  }

  return(pred)
}


residuals.sar <- function(x, n_draw = 100) {

  if(missing(n_draw)) {n_draw <- x$meta$n_save}
  draws <- sample(x$meta$n_save, n_draw, replace = FALSE)

  rhos <- x$rho[draws]
  betas <- x$beta[draws, ]

  resid <- matrix(NA, nrow = x$meta$N, ncol = n_draw)
  for(i in seq_len(n_draw)) {
    resid[, i] <- as.numeric(
      (Matrix::.sparseDiagonal(x$meta$N) - rhos[i] * x$meta$W) %*% x$meta$y -
        (x$meta$X %*% betas[i, ]))
  }

  return(resid)
}


residuals.clm <- function(x, n_draw = 100) {

  if(missing(n_draw)) {n_draw <- x$meta$n_save}
  draws <- sample(x$meta$n_save, n_draw, replace = FALSE)

  betas <- x$beta[draws, ]

  resid <- matrix(NA, nrow = x$meta$N, ncol = n_draw)
  for(i in seq_len(n_draw)) {
    resid[, i] <- as.numeric(x$meta$y - (x$meta$X %*% betas[i, ]))
  }

  return(resid)
}


rmse <- function(x, ...) {UseMethod("rmse", x)}

rmse.sar <- rmse.clm <- function(x, n_draw) {

  apply(resid(x, n_draw), 2, function(y) {sqrt(crossprod(y) / x$meta$N)})
}


hpdi <- function(x, ...) {UseMethod("hpdi", x)}

hpdi.sar <- function(x, probs = c(0.9, 0.95, 0.99), n_draw = 100, names) {

  effs <- effects(x, n_draw, names)
  colnames(effs$direct) <- paste0(colnames(effs$direct), "_dir")
  colnames(effs$indirect) <- paste0(colnames(effs$indirect), "_ind")
  coefs <- cbind(effs$direct, effs$indirect,
    rho = x$rho[sample(x$meta$n_save, nrow(effs$direct), replace = FALSE)])

  return(.hpdi(coefs, probs))
}

hpdi.clm <- function(x, probs = c(0.9, 0.95, 0.99)) {

  index <- !colnames(x$beta) %in% c("tfe", "ife")
  coefs <- x$beta[, index]

  return(.hpdi(coefs, probs))
}

.hpdi <- function(coefs, probs) {

  hpdis <- vapply(probs, function(y) {
    coda::HPDinterval(coda::as.mcmc(coefs), y)
  }, matrix(0, ncol(coefs), 2))

  out <- cbind(
    apply(hpdis, 3, function(y) cbind(y[, "lower"]))[, rev(seq_along(probs))],
    apply(hpdis, 3, function(y) cbind(y[, "upper"])))
  dimnames(out) <- list(colnames(coefs), c(rev(1 - probs), probs))

  return(out)
}

hpdi_zero <- function(x) {

  not_zero <- abs(apply(sign(x), 1, sum)) / 2
  sign <- apply(sign(x), 1, function(y) names(table(y))[which.max(table(y))])

  return(structure(paste0(ifelse(not_zero > 0, sign, 0), "_", not_zero, "*"),
    names = names(not_zero)))
}


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

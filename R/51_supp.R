
build_X <- function(
  x, const = TRUE,
  tfe = FALSE, ife = FALSE, n_time,
  W = NULL) {
  
  N <- nrow(x)
  X <- X_pre <- x[, -1]
  colnames(X) <- rep("beta", ncol(X))
  if(const) {X <- cbind("alpha" = 1, X)}
  if(!is.null(W)) {
    WX <- W %*% X_pre
    colnames(WX) <- rep("theta", ncol(WX))
    X <- cbind(X, WX)
  }
  if(tfe | ife) {
    if(missing(n_time)) {stop("Please provide the number of time periods.")}
    n_ind <- N / n_time
    if(tfe) {
      TFE <- kronecker(diag(n_time), matrix(1, n_ind))[, -1]
      colnames(TFE) <- rep("tfe", ncol(TFE))
      X <- cbind(X, TFE)
    }
    if(ife) {
      IFE <- kronecker(matrix(1, n_time), diag(n_ind))[, -1]
      colnames(IFE) <- rep("ife", ncol(IFE))
      X <- cbind(X, IFE)
    }
  }
  
  X
}


beta_prob <- function(rho, a) {
  1 / beta(a, a) *
    ((1 + rho) ^ (a - 1) * (1 - rho) ^ (a - 1)) /
    (2 ^ (2 * a - 1))
}


rmvn <- function(n, mean, sigma) {
  
  # Spectral  ---
  sigma <- eigen(sigma, symmetric = TRUE)
  m <- length(sigma[["values"]])
  R <- t(sigma[["vectors"]] %*%
      (t(sigma[["vectors"]]) * sqrt(pmax(sigma[["values"]], 0))))
  out <- matrix(rnorm(n * m), nrow = n, ncol = m, byrow = TRUE) %*% R
  out <- sweep(out, 2, mean, "+")
  
  return(out)
}


get_grid <- function(
  W, W_pre = NULL, y, N,
  n_rho, rho_min = -1, rho_max = 1,
  type = c("exact",
    "eigen", "Matrix_J", "spam", "LU", "MC", "cheb", "moments"),
  spline = TRUE) {
  
  type <- match.arg(type)
  if(type != "exact") {
    ldet_env <- new.env()
    ldet_env[["listw"]] <- spdep::mat2listw(if(is.null(W_pre)) {W} else {W_pre})
    ldet_env[["can.sim"]] <- spatialreg::can.be.simmed(ldet_env[["listw"]])
    ldet_env[["verbose"]] <- FALSE
    ldet_env[["family"]] <- "SAR"
    ldet_env[["n"]] <- N
    eval(call(paste0(type, "_setup"), ldet_env))
  }
  if(missing(W) | is.null(W)) {W <- kronecker(diag(N / nrow(W_pre)), W_pre)}
  
  rhos <- seq(rho_min, rho_max, length.out = n_rho + 2)[-c(1, n_rho + 2)]
  
  ldets <- vector("numeric", n_rho)
  A <- vector("list", n_rho)
  B_trace <- B_sum <- BW_trace <- BW_sum <- vector("numeric", n_rho)
  
  for(i in seq_along(rhos)) {
    A[[i]] <- Matrix:::.sparseDiagonal(N) - rhos[i] * W
  }
  
  # Cheaper log|A| if W is created via a Kronecker product
  if(is.null(W_pre)) {
    ldets <- if(type == "exact") {
      vapply(A, function(x) {log(det(x))}, numeric(1L))
    } else {
      vapply(rhos, do_ldet, numeric(1L), env = ldet_env)
    }
  } else {
    ldets <- if(type == "exact") {
      vapply(rhos, function(x, W_pre, N) {
        log(det(Matrix:::.sparseDiagonal(N) - x * W_pre))
      }, numeric(1L), W_pre, N = nrow(W_pre))
    } else {
      vapply(rhos, do_ldet, numeric(1L), env = ldet_env)
    }
    ldets <- log(exp(ldets) ^ as.integer(N / nrow(W_pre)))
  }
  
  if(spline) {
    n_rho <- n_rho * 3
    spl <- spline(y = ldets, x = rhos, n = n_rho)
    ldets <- spl[["y"]]
    rhos <- spl[["x"]]
  }
  
  Ay <- Matrix(0, n_rho, N)
  for(i in seq_along(rhos)) {
    if(spline) {
      A_i <- Matrix:::.sparseDiagonal(N) - rhos[i] * W
    } else {A_i <- A[[i]]}
    Ay[i, ] <- A_i %*% y
  }
  
  list("rhos" = rhos, "ldets" = ldets, "Ay" = Ay, "n_rho" = n_rho)
}

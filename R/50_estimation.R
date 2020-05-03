
sar <- function(
  x, W, LX = FALSE,
  n_draw = 1000L, n_burn = 0L, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time,
  sigma_a = 10, sigma_b = 1,
  beta_mean = 0, beta_var = 10 ^ 8,
  rho_a = 1.01,
  grid = NULL,
  verbose = TRUE) {
  
  # Setup ----------
  
  cl <- match.call()
  start_time <- Sys.time()
  
  n_save <- as.integer((n_draw - n_burn) / n_thin)
  
  y <- x[, 1] # Dependent in first column
  N <- length(y)
  
  # Check W
  msg <- "Please make sure W has the right dimension."
  W_pre <- NULL
  if(tfe || ife) {
    if(N == nrow(W)) {
      W_pre <- W[seq(1, N / n_time), seq(1, N / n_time)]
    } else if(as.integer(N / nrow(W)) %% 1 == 0) {
      if(missing(n_time)) {
        n_time <- N / nrow(W)
      } else if(n_time != N / nrow(W)) {stop(msg)}
      W_pre <- W
      W <- kronecker(Matrix::.sparseDiagonal(n_time), W)
    } else {stop(msg)}
  } else if(N != nrow(W)) {stop(msg)}
  
  X <- build_X(x, const = TRUE, tfe, ife, n_time, W = if(LX) {W} else {NULL})
  K <- ncol(X)
  k <- sum(colnames(X) %in% c("alpha", "beta"))
  
  
  # Grid ----------
  if(is.null(grid)) {
    message("Calculating grid... Consider providing `get_grid()` to `grid`.")
    grid <- get_grid(W, W_pre, y, N, n_rho = 200L, type = "eigen")
  }
  
  
  # Priors ----------
  
  beta_pr_mean <- matrix(beta_mean, K, 1)
  beta_pr_var <- diag(K) * beta_var
  beta_pr_var_inv <- diag(K) / beta_var
  
  
  # Storage ----------
  
  beta_store <- matrix(NA, nrow = n_save, ncol = K)
  colnames(beta_store) <- colnames(X)
  sigma_store <- rho_store <- vector("numeric", n_save)
  ll <- vector("numeric", n_save)
  
  # Starting values
  # beta_draw <- as.numeric(mvtnorm::rmvnorm(1L, mean = beta_pr_mean,
  #   sigma = beta_pr_var))
  beta_draw <- as.numeric(rmvn(1L, mean = beta_pr_mean, sigma = beta_pr_var))
  
  sigma_draw <- 1 / rgamma(1, sigma_a / 2, sigma_b / 2)
  j <- as.integer(grid[["n_rho"]] / 2L)
  rho_draw <- grid[["rhos"]][j] # Middle
  Ay <- (Matrix:::.sparseDiagonal(N) - rho_draw * W) %*% y
  
  XX <- crossprod(X)
  Xy <- crossprod(X, y)
  Wy <- W %*% y
  XWy <- crossprod(X, Wy)
  
  if(verbose) {pb <- txtProgressBar(min = 0, max = n_draw, style = 3)}
  
  for(i in (1 - n_burn):(n_draw - n_burn)) {
    # Beta
    V <- solve(beta_pr_var_inv + 1 / sigma_draw * XX)
    b <- V %*% (beta_pr_var %*% beta_pr_mean +
        1 / sigma_draw * crossprod(X, Ay))
    # beta_draw <- as.numeric(mvtnorm::rmvnorm(1L, mean = b, sigma = as.matrix(V)))
    beta_draw <- as.numeric(rmvn(1L, mean = b, sigma = V))
    
    # Sigma
    ESS_draw <- as.double(crossprod(Ay - X %*% beta_draw))
    sigma_draw <- 1 / rgamma(1, sigma_a + N / 2, sigma_b + ESS_draw / 2)

    # Rho
    b0 <- V %*% (beta_pr_var_inv %*% beta_pr_mean + 1 / sigma_draw * Xy)
    bd <- V %*% (beta_pr_var_inv %*% beta_pr_mean + 1 / sigma_draw * XWy)
    
    e0 <- y - X %*% b0
    ed <- Wy - X %*% bd
    
    epe0 <- as.double(crossprod(e0))
    eped <- as.double(crossprod(ed))
    epe0d <- as.double(crossprod(ed, e0))
    
    z0 <- -(N - K) / 2 *
      log(epe0 - 2 * grid[["rhos"]] * epe0d + grid[["rhos"]] ^ 2 * eped)
    den <- grid[["ldets"]] + z0 + log(beta_prob(grid[["rhos"]], rho_a))
    ex <- exp(den - max(den))
    i_sum <- sum((grid[["rhos"]][-1] - grid[["rhos"]][-grid[["n_rho"]]]) *
        (ex[-1] + ex[-length(ex)]) / 2)
    z1 <- abs(ex / i_sum)
    dens <- cumsum(z1)
    rnd <- runif(1) * sum(z1)
    j <- max(which(dens <= rnd))
    if(length(j) != 1) { # Reuse the last
      j <- which(grid[["rhos"]] == rho_draw)
    }
    rho_draw <- grid[["rhos"]][j]
    Ay <- (Matrix:::.sparseDiagonal(N) - rho_draw * W) %*% y
    
    # Store
    if(i > 0 && i %% n_thin == 0) {
      
      beta_store[(i / n_thin), ] <- beta_draw
      sigma_store[(i / n_thin)] <- sigma_draw
      rho_store[(i / n_thin)] <- rho_draw
      ll[(i / n_thin)] <- grid[["ldets"]][j] - ESS_draw / (2 * sigma_draw) +
        beta_prob(rho_draw, rho_a)
    }
    
    if(verbose) {setTxtProgressBar(pb, (i + n_burn))}
  }
  
  timer <- Sys.time() - start_time
  
  if(verbose) {
    close(pb)
    cat("Finished after ", format(round(timer, 2)), ".\n", sep = "")
  }
  
  # Outputs ----------
  
  out <- list(
    "beta" = beta_store,
    "sigma" = sigma_store,
    "rho" = rho_store,
    "ll" = ll,
    "priors" = list(
      "sigma_a" = sigma_a, "sigma_b" = sigma_b,
      "beta_mean" = beta_mean, "beta_var" = beta_var,
      "rho_a" = rho_a
    ),
    "meta" = list(
      "timer" = timer, "y" = y, "X" = X, "W" = W, "LX" = LX, "N" = N, "K" = K,
      "tfe" = tfe, "ife" = ife,
      "n_draw" = n_draw, "n_burn" = n_burn, "n_save" = n_save,
      "n_thin" = n_thin
    )
  )
  class(out) <- "sar"
  
  return(out)
}


clm <- function(
  x, W = NULL, LX = FALSE,
  n_draw = 1000L, n_burn = 0L, n_thin = 1L,
  tfe = TRUE, ife = TRUE, n_time,
  sigma_a = 10, sigma_b = 1,
  beta_mean = 0, beta_var = 10 ^ 8,
  verbose = TRUE) {
  
  # Setup ----------
  
  cl <- match.call()
  start_time <- Sys.time()
  
  n_save <- (n_draw - n_burn) / n_thin
  
  y <- x[, 1] # Dependent in first column
  N <- length(y)
  
  if(LX) {
    # Check W
    msg <- "Please make sure W has the right dimension."
    W_pre <- NULL
    if(tfe || ife) {
      if(N == nrow(W)) {
        W_pre <- W[seq(1, N / n_time), seq(1, N / n_time)]
      } else if(as.integer(N / nrow(W)) %% 1 == 0) {
        if(missing(n_time)) {
          n_time <- N / nrow(W)
        } else if(n_time != N / nrow(W)) {stop(msg)}
        W_pre <- W
        W <- kronecker(diag(n_time), W)
      } else {stop(msg)}
    } else if(N != nrow(W)) {stop(msg)}
  }
  
  X <- build_X(x, const = TRUE, tfe, ife, n_time, W = if(LX) {W} else {NULL})
  K <- ncol(X)
  k <- sum(colnames(X) %in% c("alpha", "beta"))
  
  
  # Priors ----------
  
  beta_pr_mean <- matrix(beta_mean, K, 1)
  beta_pr_var <- diag(K) * beta_var
  beta_pr_var_inv <- diag(K) / beta_var
  
  
  # Storage ----------
  
  beta_store <- matrix(NA, nrow = n_save, ncol = K)
  colnames(beta_store) <- colnames(X)
  sigma_store <- vector("numeric", n_save)
  ll <- vector("numeric", n_save)
  
  # Starting values
  # beta_draw <- as.numeric(mvtnorm::rmvnorm(1L, mean = beta_pr_mean,
  #   sigma = beta_pr_var))
  beta_draw <- as.numeric(rmvn(1L, mean = beta_pr_mean, sigma = beta_pr_var))
  sigma_draw <- 1 / rgamma(1, sigma_a / 2, sigma_b / 2)
  
  XX <- crossprod(X)
  Xy <- crossprod(X, y)
  
  if(verbose) {pb <- txtProgressBar(min = 0, max = n_draw, style = 3)}
  
  for(i in (1 - n_burn):(n_draw - n_burn)) {
    # Beta
    V <- solve(beta_pr_var_inv + 1 / sigma_draw * XX)
    b <- V %*% (beta_pr_var %*% beta_pr_mean + 1 / sigma_draw * Xy)
    # beta_draw <- as.numeric(mvtnorm::rmvnorm(1L, mean = b, sigma = as.matrix(V)))
    beta_draw <- as.numeric(rmvn(1L, mean = b, sigma = V))
    
    # Sigma
    ESS_draw <- as.double(crossprod(y - X %*% beta_draw))
    sigma_draw <- 1 / rgamma(1, sigma_a + N / 2, sigma_b + ESS_draw / 2)
    
    # Store
    if(i > 0 && i %% n_thin == 0) {
      
      beta_store[(i / n_thin), ] <- beta_draw
      sigma_store[(i / n_thin)] <- sigma_draw
      ll[(i / n_thin)] <- -ESS_draw / (2 * sigma_draw)
    }
    
    if(verbose) {setTxtProgressBar(pb, (i + n_burn))}
  }
  
  timer <- Sys.time() - start_time
  
  if(verbose) {
    close(pb)
    cat("Finished after ", format(round(timer, 2)), ".\n", sep = "")
  }
  
  # Outputs ----------
  
  out <- list(
    "beta" = beta_store,
    "sigma" = sigma_store,
    "ll" = ll,
    "priors" = list(
      "sigma_a" = sigma_a, "sigma_b" = sigma_b,
      "beta_mean" = beta_mean, "beta_var" = beta_var
    ),
    "meta" = list(
      "timer" = timer, "y" = y, "X" = X, "W" = W, "LX" = LX, "N" = N, "K" = K,
      "tfe" = tfe, "ife" = ife,
      "n_draw" = n_draw, "n_burn" = n_burn, "n_save" = n_save,
      "n_thin" = n_thin
    )
  )
  class(out) <- "clm"
  
  return(out)
}

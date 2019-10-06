
# Dependencies ------------------------------------------------------------

stopifnot(
  nzchar(system.file(package = "sf")),
  nzchar(system.file(package = "dplyr")),
  nzchar(system.file(package = "spdep")),
  nzchar(system.file(package = "MASS")),
  nzchar(system.file(package = "Matrix")),
  nzchar(system.file(package = "matrixcalc"))
)


# SDM panel ---------------------------------------------------------------

sdm_panel <- function(
  x, # Data
  W_pre, 
  dates_len, # Number of time periods
  lag_X = TRUE, # To-do
  tfe = TRUE, cfe = TRUE, 
  rho_a = 1.01, sigma_a = 10, sigma_b = 1, 
  beta_mean = 0, beta_var = 10 ^ 8, # Priors
  n_iter = 2000,
  n_save = 1000,
  n_griddy = 200) {
  
  W <- kronecker(diag(dates_len), W_pre)
  
  y <- x[, 1]
  X_pre <- x[, -1]
  
  X <- cbind(1, X_pre)
  
  if(lag_X) {X <- cbind(X, W %*% X_pre)}
  if(tfe) {
    TFE <- kronecker(diag(dates_len), matrix(1, nrow(X_pre) / dates_len, 1))
    X <- cbind(X, TFE[, -1])
  }
  if(cfe) {
    CFE <- kronecker(matrix(1, dates_len, 1), diag(nrow(X_pre) / dates_len))
    X <- cbind(X, CFE[, -1])
  }
  
  K <- ncol(X)
  N <- nrow(X_pre)
  k <- ncol(X_pre)
  
  
  # Priors ------------------------------------------------------------------
  
  # Proper, but uninformative
  beta_pr_mean <- matrix(beta_mean, K, 1)
  beta_pr_var <- diag(K) * beta_var
  beta_pr_var_inv <- solve(beta_pr_var)
  # plot(density(rnorm(1e6, beta_pr_mean[1], beta_pr_var[1,1] ^ -1)))
  
  # Beta prior on rho
  beta_prob <- function(rho, a) {
    1 / beta(a, a) * 
      ((1 + rho) ^ (a - 1) * (1 - rho) ^ (a - 1)) / 
      (2 ^ (2 * a - 1))
  }
  # plot(beta_prob(seq(-1, 1, length.out = 1e5), rho_a), type = "l")
  
  
  # Rho sampling --------------------------------------------------------------
  n_acc <- 0
  
  n_burn <- n_iter - n_save
  
  # Storage
  beta_post <- matrix(0, K, n_save)
  sigma_post <- vector("numeric", n_save)
  rho_post <- vector("numeric", n_save)
  
  R2_post <- R2_bar_post <- RMSE_post <- vector("numeric", n_save)
  AIC_post <- BIC_post <- vector("numeric", n_save)
  
  direct_post <- indirect_post <- total_post <- matrix(0, k + 1, n_save)
  
  
  # Griddy Gibbs ------------------------------------------------------------
  
  # To-do: Doesn't need to be calculated every time
  list_det <- get_ln_det(W, length.out = n_griddy + 2)
  
  rhos <- list_det$rho[-c(1, n_griddy + 2)]
  ln_det <- list_det$ln_det[-c(1, n_griddy + 2), ]
  
  Ays <- matrix(0, N, n_griddy)
  
  A_inv_diags <- A_inv_tots <- A_inv_W_diags <- A_inv_W_tots <- 
    vector("numeric", n_griddy)
  
  time <- Sys.time()
  cat("Pre-calculating Griddy Gibbs.\n")
  for(i in seq_len(n_griddy)) {
    A <- Matrix::.sparseDiagonal(N) - rhos[i] * W
    Ays[, i] = as.numeric(A %*% y)
    A_inv <- solve(A)
    A_inv_W <- A_inv %*% W
    
    A_inv_diags[i] <- sum(diag(A_inv))
    A_inv_tots[i] <- sum(A_inv)
    A_inv_W_diags[i] <- sum(diag(A_inv_W))
    A_inv_W_tots[i] <- sum(A_inv_W)
  }
  cat("Done after ", format(Sys.time() - time), ".\n", sep = "")
  
  
  # MCMC ------------------------------------------------------------------
  
  curr_beta <- MASS::mvrnorm(1, beta_pr_mean, beta_pr_var)
  curr_sigma <- 1 / rgamma(1, sigma_a / 2, sigma_b / 2)
  curr_rho <- 0
  
  XX <- crossprod(X)
  Xy <- crossprod(X, y)
  Wy <- W %*% y
  XWy <- crossprod(X, Wy)
  curr_Ay <- y - curr_rho * Wy
  
  
  time <- Sys.time()
  cat("Starting MCMC.\n")
  for(i in seq_len(n_iter)) {
    
    # Beta
    V <- solve(beta_pr_var_inv + 1 / curr_sigma * XX)
    b <- V %*% (beta_pr_var %*% beta_pr_mean + 
                  1 / curr_sigma * crossprod(X, curr_Ay))
    curr_beta <- MASS::mvrnorm(1, b, V)
    
    # Sigma
    curr_Xb <- X %*% curr_beta
    curr_ESS <- crossprod(curr_Ay - curr_Xb)
    curr_sigma <- 1 / rgamma(1, sigma_a + N / 2, 
                             sigma_b + as.double(curr_ESS) / 2)
    
    # Rho
    b0 <- V %*% (beta_pr_var_inv %*% beta_pr_mean + 1 / curr_sigma * Xy)
    bd <- V %*% (beta_pr_var_inv %*% beta_pr_mean + 1 / curr_sigma * XWy)
    
    e0 <- y - X %*% b0
    ed <- Wy - X %*% bd
    
    epe0 <- as.double(crossprod(e0))
    eped <- as.double(crossprod(ed))
    epe0d <- as.double(crossprod(ed, e0))
    
    z <- -(N - K) / 2 * log(epe0 - 2 * rhos * epe0d + rhos ^ 2 * eped)
    den <- ln_det + z + log(beta_prob(rhos, rho_a))
    den_adj <- den - max(den)
    ex <- exp(den_adj)
    i_sum <- sum((rhos[-1] + rhos[-length(rhos)]) * (ex[-1] - ex[-length(ex)]) / 2)
    z <- abs(ex / i_sum)
    dens <- cumsum(z)
    rnd <- runif(1) * sum(z)
    ind <- max(1, which(dens <= rnd))
    
    # Adjust
    curr_rho <- rhos[ind]
    curr_Ay <- Ays[, ind]
    curr_A_inv_diags <- A_inv_diags[ind]
    curr_A_inv_tots <- A_inv_tots[ind]
    curr_A_inv_W_diags <- A_inv_W_diags[ind]
    curr_A_inv_W_tots <- A_inv_W_tots[ind]
    
    # Store
    if(i > n_burn) {
      
      s <- i - n_burn
      beta_post[, s] <- as.matrix(curr_beta)
      sigma_post[s] <- curr_sigma
      rho_post[s] <- curr_rho
      
      # Spatial effects
      if(lag_X) {
        direct_post[, s] <- curr_A_inv_diags / N * curr_beta[1:(k + 1)] +
          c(0, curr_A_inv_W_diags / N * curr_beta[(k + 2):(2 * k + 1)])
        total_post[, s] <- curr_A_inv_tots / N * curr_beta[1:(k + 1)] +
          c(0, curr_A_inv_W_tots / N * curr_beta[(k + 2):(2 * k + 1)])
        indirect_post[, s] <- total_post[, s] - direct_post[, s]
      } else {
        direct_post[, s] <- curr_A_inv_diags / N * curr_beta[1:(k + 1)]
        total_post[, s] <- curr_A_inv_tots / N * curr_beta[1:(k + 1)]
        indirect_post[, s] <- total_post[, s] - direct_post[, s]
      }
      
      # R^2
      curr_resid <- as.matrix((diag(N) - curr_rho * W) %*% y - (X %*% curr_beta))
      SSR <- crossprod(curr_resid)
      TSS <- crossprod(y - mean(y))
      R2_post[s] <- 1 - SSR / TSS
      RMSE_post[s] <- sqrt(SSR / N)
      
      SSR_adj <- SSR / (N - K)
      TSS_adj <- TSS / (N - 1)
      R2_bar_post[s] <- 1 - SSR_adj / TSS_adj
      
      # AIC & BIC
      ll <- sum(dnorm(curr_resid, 0, 1, log = TRUE))
      df_ll <- K + 1
      BIC_post[s] <- -2 * ll + log(N) * df_ll
      AIC_post[s] <- -2 * ll + 2 * df_ll
    }
  }
  cat("Done after ", format(Sys.time() - time), ".\n", sep = "")
  
  
  # Geweke convergence diagnostics ------------------------------------------
  
  full_chain <- cbind(t(beta_post), rho_post)
  mh_draws <- coda::mcmc(full_chain)
  geweke_conv <- coda::geweke.diag(mh_draws)$z
  cat("Geweke convergence diagnostic indicate convergence: ",
      all(abs(geweke_conv) < 3), ".\n", sep = "")
  converged <- all(abs(geweke_conv) < 3)
  
  
  # Posteriors --------------------------------------------------------------
  
  direct_post_mean <- apply(direct_post, 1, mean)
  indirect_post_mean <- apply(indirect_post, 1, mean)
  total_post_mean <- apply(total_post, 1, mean)
  
  direct_post_sd <- apply(direct_post, 1, sd)
  indirect_post_sd <- apply(indirect_post, 1, sd)
  total_post_sd <- apply(total_post, 1, sd)
  
  # More pseudo stuff
  RMSE <- median(RMSE_post)
  R2 <- median(R2_post)
  R2_bar <- median(R2_bar_post)
  
  AIC <- median(AIC_post)
  BIC <- median(BIC_post)
  
  
  # Print -------------------------------------------------------------------
  
  # Output as table, post_mean / post_sd ~ Bayesian t-values
  res_effects <- data.frame(
    variables = c(c("const", colnames(X_pre))),
    direct = direct_post_mean, 
    direct_t = direct_post_mean / direct_post_sd,
    indirect = indirect_post_mean,
    indirect_t = indirect_post_mean / indirect_post_sd
  )
  
  res_other <- data.frame(
    variables = c("R2", "R2_bar", "AIC", "BIC", "RMSE", "Obs"),
    value = c(R2, R2_bar, AIC, BIC, RMSE, N)
  )
  
  
  # Save --------------------------------------------------------------------
  
  out <- list(
    "variables" = colnames(X_pre),
    "res_effects" = res_effects,
    "res_other" = res_other,
    "rho_post" = rho_post,
    "beta_post" = beta_post,
    "sigma_post" = sigma_post,
    "direct_post" = direct_post,
    "indirect_post" = indirect_post,
    "converged" = converged
  )
  
}


# Pace & Barry MC approximation to log|I - rho W| -------------------------

get_ln_det <- function( # To-do: interface with spatialreg
  W, 
  length.out = 200,
  rmin = -1, # set t0 1e-5 for 0 < rho < 1
  rmax = 1,
  n_order = 50,
  n_iter = 30) {
  
  library(matrixcalc)
  
  W = as.matrix(W)
  N = dim(W)[1]
  
  # Exact moments from 1 to n_exact
  TD = matrix(c(0, sum(W ^ 2) / 2), length(c(0, sum(W ^ 2) / 2)), 1)
  n_exact = length(TD)
  
  # Stochastic moments
  mavmomi = matrix(0, n_order, n_iter)
  
  for(j in 1:n_iter) {
    U = matrix(rnorm(N, 0, 1), N, 1)
    V = U
    UU = t(U) %*% U
    for(i in 1:n_order) {
      WV = W %*% V
      mavmomi[i, j] = N * ((t(U) %*% WV) / (i * UU))
    }
  }
  mavmomi[1:n_exact, ] = TD[, matrix(1, n_iter, 1)]
  
  # Averages across iterations
  avmomi = as.matrix(rowMeans(mavmomi))
  
  # Alpha matrix
  alpha = seq(rmin, rmax, length.out = length.out)
  valpha = vandermonde.matrix(alpha, length(alpha))
  alomat = -valpha[, (2:(n_order + 1))]
  
  # Estimate ln |I - rho W| using mixture of exact and stochastic moments
  # Exact from 1 to n_exact, stochastic from (n_exact + 1) to n_order
  
  ln_det_mat = alomat %*% avmomi
  
  srvs = t(alomat %*% avmomi)
  std_err = c(t(sqrt((rowMeans(srvs * srvs) - rowMeans(srvs) ^ 2) / n_iter)))
  
  ci = c(ln_det_mat - 1.96 * std_err,
         ln_det_mat + 1.96 * std_err)
  
  out = list("ln_det" = ln_det_mat,
             "rho" = alpha,
             "std_err" = std_err,
             "ci" = ci)
  
  detach("package:matrixcalc")
  
  return(out)
  # return(cbind(ln_det_mat, alpha))
}


# Get matrix from sf data -------------------------------------------------

get_matr <- function(x, variables, dates) {
  
  x %>% 
    filter(date %in% dates) %>% 
    ungroup() %>%
    sf:::select.sf(variables) %>% 
    sf::`st_geometry<-`(NULL) %>% 
    as.matrix(matr, rownames.force = FALSE)
}


# spdep weights matrix ----------------------------------------------------

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


# Helpers -----------------------------------------------------------------

formula_ify <- function(x) { # To convert this for plm & splm
  as.formula(paste0(x[1], " ~ ", paste(x[-1], collapse = " + ")), env = globalenv())
}


# To be used in 5_panel.R

source("R/7_functions.R")


# Prep --------------------------------------------------------------------

sdm_panel <- function(
  x, W_pre, dates_len,
  tfe = TRUE, cfe = TRUE, 
  n_iter = 2000,
  n_save = 1000,
  n_griddy = 200) {

W <- kronecker(diag(dates_len), W_pre)

y <- x[, 1]
X_pre <- x[, -1]

X <- cbind(1, X_pre, W %*% X_pre)

if(tfe) {
  TFE <- kronecker(diag(dates_len), matrix(1, nrow(X_pre) / dates_len, 1))
  X <- cbind(X, TFE[, -1])
  if(cfe) {
    CFE <- kronecker(diag(nrow(X_pre) / dates_len), matrix(1, dates_len, 1))
    X <- cbind(X, CFE[, -1])
  }
}

K <- ncol(X)
N <- nrow(X_pre)
k <- ncol(X_pre)


# Priors ------------------------------------------------------------------

# Proper, but uninformative
beta_pr_mean <- matrix(0, K, 1)
beta_pr_var <- diag(K) * 10 ^ 8
beta_pr_var_inv <- solve(beta_pr_var)

# plot(density(rnorm(1e6, beta_pr_mean[1], beta_pr_var[1,1] ^ -1)))

# Proper, but uninformative
sigma_a <- 0.01
sigma_b <- 0.01

# Beta prior on rho
beta_prob <- function(rho, a) {
  1 / beta(a, a) * 
    ((1 + rho) ^ (a - 1) * (1 - rho) ^ (a - 1)) / 
    (2 ^ (2 * a - 1))
}
rho_a <- 1.01

# plot(beta_prob(seq(-1, 1, length.out = 1e5), rho_a), type = "l")


# Rho sampling --------------------------------------------------------------

prop_scale <- 1
prop_adj <- 1.1

n_acc <- 0

n_burn <- n_iter - n_save

# Storage
beta_post <- matrix(0, K, n_save)
sigma_post <- vector("numeric", n_save)
rho_post <- vector("numeric", n_save)

R2_post <- R2_bar_post <- vector("numeric", n_save)
AIC_post <- BIC_post <- vector("numeric", n_save)

direct_post <- indirect_post <- total_post <- matrix(0, k + 1, n_save)


# Griddy Gibbs ------------------------------------------------------------

list_det <- get_ln_det(W, length.out = n_griddy + 2)

rhos <- list_det$rho[-c(1, n_griddy + 2)]
ln_det <- list_det$ln_det[-c(1, n_griddy + 2), ]

Ays <- matrix(0, N, n_griddy)

A_inv_diags <- A_inv_tots <- 
  A_inv_W_diags <- A_inv_W_tots <- 
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


# Gibbs Sampler -----------------------------------------------------------

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
  x <- exp(den_adj)
  i_sum <- sum((rhos[-1] + rhos[-length(rhos)]) * (x[-1] - x[-length(x)]) / 2)
  z <- abs(x / i_sum)
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
    direct_post[, s] <- curr_A_inv_diags / N * curr_beta[1:(k + 1)] +
      c(0, curr_A_inv_W_diags / N * curr_beta[(k + 2):(2 * k + 1)])
    total_post[, s] <- curr_A_inv_tots / N * curr_beta[1:(k + 1)] +
      c(0, curr_A_inv_W_tots / N * curr_beta[(k + 2):(2 * k + 1)])
    indirect_post[, s] <- total_post[, s] - direct_post[, s]
    
    # R^2
    curr_resid <- as.matrix((diag(N) - curr_rho * W) %*% y - (X %*% curr_beta))
    SSR <- crossprod(curr_resid)
    TSS <- crossprod(y - mean(y))
    R2_post[s] <- 1 - SSR / TSS
    
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
cat("Geweke convergence diagnostics: ", 
    paste(round(geweke_conv, 1), collapse = ", "),
    "\nConverged: ", all(abs(geweke_conv) < 3), "\n", sep = "")
converged <- all(abs(geweke_conv) < 3)


# Posteriors --------------------------------------------------------------

# beta_post_mean <- apply(beta_post, 1, mean)
# sigma_post_mean <- mean(sigma_post)
# rho_post_mean <- mean(rho_post)
# rho_post_sd <- sd(rho_post)

direct_post_mean <- apply(direct_post, 1, mean)
indirect_post_mean <- apply(indirect_post, 1, mean)
total_post_mean <- apply(total_post, 1, mean)

direct_post_sd <- apply(direct_post, 1, sd)
indirect_post_sd <- apply(indirect_post, 1, sd)
total_post_sd <- apply(total_post, 1, sd)

# More pseudo stuff
R2 <- median(R2_post)
R2_bar <- median(R2_bar_post)

AIC <- median(AIC_post)
BIC <- median(BIC_post)

# CI
# credible_interval <- function(x, prob = 0.05) {
#   quantile(x, c(prob, 0.5, 1 - prob))
# }

# beta_post_ci <- apply(beta_post, 1, credible_interval)
# rho_post_ci <- credible_interval(rho_post)
# 
# direct_post_ci <- apply(direct_post, 1, credible_interval)
# indirect_post_ci <- apply(direct_post, 1, credible_interval)
# total_post_ci <- apply(direct_post, 1, credible_interval)


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
  variables = c("R2", "R2_bar", "AIC", "BIC", "Obs"),
  value = c(R2, R2_bar, AIC, BIC, N)
)


# Save --------------------------------------------------------------------

out <- list(
  "variables" = colnames(X_pre),
  "res_effects" = res_effects,
  "res_other" = res_other,
  "rho_post" = rho_post,
  "beta_post" = beta_post,
  "sigma_post" = sigma_post,
  # "direct_post_ci" = direct_post_ci,
  # "indirect_post_ci" = indirect_post_ci,
  "converged" = converged
)

}

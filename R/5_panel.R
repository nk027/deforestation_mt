
library(MASS)
library(dplyr)
source("R/7_ln_det.R")
source("R/7_w_matr.R")

data <- readRDS("data/data.rds")

dates <- c(2005, 2016)
dates_len <- length(dates[1]:dates[2])
   

# Prep --------------------------------------------------------------------

names(data)

matr <- data %>%
  filter(date >= dates[1], date <= dates[2]) %>% 
  ungroup() %>%
  sf:::select.sf(forest_ch_km2, forest_px_km2, pasture_px_km2, crop_px_km2, 
                 max_yield_brl, pop_km2, gdp_cap, 
                 spei_dry, spei_wet, 
                 milk_brl_cow, cattle_dens) %>% 
  mutate(max_yield_brl = max_yield_brl / 1000,
         milk_brl_cow = milk_brl_cow / 1000,
         geometry = NULL) %>% 
  as.matrix(matr, rownames.force = FALSE)

W <- get_W(data)
W_ext <- kronecker(diag(dates_len), W)

y <- matr[, 1]
X <- matr[, -1]

TFE <- kronecker(diag(dates_len), matrix(1, nrow(X) / dates_len, 1))

X_ext <- cbind(1, X, W %*% X, TFE[, -1])

K <- ncol(X_ext)
N <- nrow(X)
k <- ncol(X)


# Priors ------------------------------------------------------------------

# Proper, but uninformative
beta_pr_mean <- matrix(0, K, 1)
beta_pr_var <- diag(K) * 10 ^ 8
beta_pr_var_inv <- solve(beta_pr_var)

# Proper, but uninformative
sigma_a <- 0.001
sigma_b <- 0.001

# Beta prior on rho
beta_prob <- function(rho, a) {
  1 / beta(a, a) * 
    ((1 + rho) ^ (a - 1) * (1 - rho) ^ (a - 1)) / 
    (2 ^ (2 * a - 1))
}
rho <- 1.01


# Sample rho --------------------------------------------------------------

prop_scale <- 1
prop_adj <- 1.1

n_acc <- 0

n_iter <- 2000
n_save <- 1000
n_burn <- n_iter - n_save

# Storage
post_beta <- matrix(0, K, n_save)
post_sigma <- matrix(0, 1, n_save)
post_rho <- matrix(0, 1, n_save)

rho_tmp <- matrix(0, 1, n_iter)

post_dir <- post_ind <- post_tot <- matrix(0, k + 1, n_save)


# Griddy Gibbs ------------------------------------------------------------

n_griddy <- 1000
list_det <- ln_det(W, length.out = n_griddy + 2)

rhos <- list_det$rho[-c(1, n_griddy + 2)]
ln_det <- list_det$ln_det[-c(1, n_griddy + 2), ]

Ays <- matrix(0, N, n_griddy)

A_inv_diags <- A_inv_tots <- 
  A_inv_W_diags <- A_inv_W_tots <- 
  vector("numeric", n_griddy)

time <- Sys.time()
cat("Pre-calculating Griddy Gibbs.\n")
for(i in seq_len(n_griddy)) {
  A <- .sparseDiagonal(N) - rhos[i] * W
  Ays[, i] = c(A %*% y)
  A_inv <- solve(A)
  A_inv_W <- A_inv %*% W
  
  A_inv_diags[i] <- sum(diag(A_inv))
  A_inv_tots[i] <- sum(A_inv)
  A_inv_W_diags[i] <- sum(diag(A_inv_W))
  A_inv_W_tots[i] <- sum(A_inv_W)
}
cat("Done after ", format(Sys.time() - time), ".\n", sep = "")


# Gibbs Sampler -----------------------------------------------------------

curr_beta <- mvrnorm(1, beta_pr_mean, beta_pr_var)
curr_sigma <- 1 / rgamma(1, sigma_a / 2, sigma_b / 2)
curr_rho <- 0

XX <- crossprod(X)
Xy <- crossprod(X, y)
Wy <- W %*% y
XWy <- crossprod(X, Wy)
curr_Ay <- y - curr_rho * Wy

for(i in seq_len(n_iter)) {
  
  # Beta
  V <- solve(beta_pr_var_inv + 1 / curr_sigma * XX)
  b <- V %*% (beta_pr_var %*% beta_pr_mean + 
                1 / curr_sigma * crossprod(X, curr_Ay))
  curr_beta <- mvrnorm(1, b, V)
  
  # Sigma
  curr_Xb <- X %*% curr_beta
  curr_ESS <- crossprod(curr_Ay - curr_Xb)
  curr_sigma <- 1 / rgamma(1, sigma_a + n / 2, 
                           sigma_b + as.double(curr_ESS) / 2)
  
  # Rho
  b0 <- V %*% (beta_pr_var_inv %*% beta_pr_mean + 1 / curr_sigma * Xy)
  bd <- V %*% (beta_pr_var_inv %*% beta_pr_mean + 1 / curr_sigma * XWy)
  
  e0 <- Y - X %*% b0
  ed <- WY - X %*% bd
  
  epe0 <- as.double(crossprod(e0))
  eped <- as.double(crossprod(ed))
  epe0d <- as.double(crossprod(ed, e0))
  
  z <- -(N - K) / 2 * log(epe0 - 2 * rhos * epe0d + rhos ^ 2 * eped)
  den <- ln_det + z + log(beta_prob(rhos, rho_a))
  den_adj <- den - max(den)
  x <- exp(den_adj)
  i_sum <- sum((rhos[-1] + y[-length(y)]) * (x[-1] - x[-length(x)]) / 2)
  z <- abs(x / i_sum)
  dens <- cumsum(z)
  rnd <- runif(1) * sum(z)
  ind <- max(x(1, which(den <= rnd)))
  
  # Adjust
  curr_rho <- rhos[ind]
  curr_Ay <- AYs[, ind]
  curr_A_inv_diags <- A_inv_diags[ind]
  curr_A_inv_tots <- A_inv_tots[ind]
  curr_A_inv_W_diags <- A_inv_W_diags[ind]
  curr_A_inv_W_tots <- A_inv_W_tots[ind]
  
  # Store
  if(i > n_burn) {
    
    s <- n_iter - n_burn
    post_beta[, s] <- as.matrix(curr_beta)
    post_sigma[s] <- curr_sigma
    post_rho[s] <- curr_rho
    
    post_dir[, s] <- curr_A_inv_diags / N * curr_beta[1:(k + 1)] +
      c(0, curr_A_inv_W_diags / N * curr_beta[(k + 2):(2 * k + 1)])
    post_tot[, s] <- curr_A_inv_tots / N * curr_beta[1:(k + 1)] +
      c(0, curr_A_inv_W_tots / N * curr_beta[(k + 2):(2 * k + 1)])
    post_ind[, s] <- post_tot[, s] - post_dir[, s]
  }
}

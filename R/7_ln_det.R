# Pace & Barry MC approximation to log|I - rho W|
ln_det <- function(
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
  
  # return(cbind(ln_det_mat, alpha))
}
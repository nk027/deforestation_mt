
library("spatialreg")
library("matrixcalc")

# Pace & Barry MC approximation to log|I - rho W|
get_ln_det <- function(
  W, 
  length.out, 
  rmin = -1, rmax = 1, 
  n_order = 50, n_iter = 30) {
  
  W <- as.matrix(W)
  N <- dim(W)[1]
  
  # Exact moments from 1 to n_exact
  TD <- matrix(c(0, sum(W ^ 2) / 2), length(c(0, sum(W ^ 2) / 2)), 1)
  n_exact <- length(TD)
  
  # Stochastic moments
  mavmomi <- matrix(0, n_order, n_iter)
  
  for(j in 1:n_iter) {
    U <- matrix(rnorm(N, 0, 1), N, 1)
    V <- U
    UU <- t(U) %*% U
    for(i in 1:n_order) {
      WV <- W %*% V
      mavmomi[i, j] <- N * ((t(U) %*% WV) / (i * UU))
    }
  }
  mavmomi[1:n_exact, ] <- TD[, matrix(1, n_iter, 1)]
  
  # Averages across iterations
  avmomi <- as.matrix(rowMeans(mavmomi))
  
  # Alpha matrix
  alpha <- seq(rmin, rmax, length.out = length.out)
  valpha <- vandermonde.matrix(alpha, length(alpha))
  alomat <- -valpha[, (2:(n_order + 1))]
  
  # Estimate ln |I - rho W| using mixture of exact and stochastic moments
  # Exact from 1 to n_exact, stochastic from (n_exact + 1) to n_order
  
  ln_det_mat <- c(alomat %*% avmomi)
  
  srvs <- t(alomat %*% avmomi)
  std_err <- c(t(sqrt((rowMeans(srvs * srvs) - rowMeans(srvs) ^ 2) / n_iter)))
  
  ci = c(ln_det_mat - 1.96 * std_err,
         ln_det_mat + 1.96 * std_err)
  
  out = list("ln_det" = ln_det_mat, "rho" = alpha,
             "std_err" = std_err, "ci" = ci)
}


# Setup -------------------------------------------------------------------

W_qu <- readRDS("data/W_qu.rds")

sp_env <- new.env()u
sp_env$listw <- spdep::mat2listw(W_qu)
sp_env$can.sim <- can.be.simmed(spdep::mat2listw(W_qu))
sp_env$verbose <- TRUE
sp_env$n <- nrow(W_qu)
sp_env$family <- "SAR"


# Execute -----------------------------------------------------------------

jim <- get_ln_det(Ws$queen, length.out = 2000L)

bob1 <- bob2 <- exact <- vector("numeric", length(jim[["ln_det"]]))

for(i in seq_along(jim[["ln_det"]])) {
  eigen_setup(sp_env)
  bob1[i] <- do_ldet(jim[["rho"]][i], env = sp_env)
  mcdet_setup(sp_env)
  bob2[i] <- do_ldet(jim[["rho"]][i], env = sp_env)
  exact[i] <- log(det(diag(sp_env$n) - jim[["rho"]][i] * W_qu))
}

ylim <- c(min(jim[["ln_det"]], bob1, bob2), 
          max(jim[["ln_det"]], bob1, bob2))

op <- par(mar =  c(2, 2, 2, 0.5))
png("plots/ln_det_full.png", width = 720)
plot(jim[["rho"]], exact, type = "line", ylim = ylim,
     xlab = "Rho", ylab = "Log-Det", main = "Log-Det Comparison")
lines(jim[["rho"]], jim[["ln_det"]], col = "orange")
lines(jim[["rho"]], bob2, col = "darkblue")
lines(jim[["rho"]], bob1, col = "darkgreen")
legend("bottomleft", inset = .02, 
       legend = c("exact", "bob_eigen", "bob_mc", "jim"),
       col = c("black", "orange", "darkgreen", "darkblue"), 
       lty = 1, cex = 0.8)
dev.off()
par(op)


op <- par(mar =  c(2, 2, 2, 0.5))
png("plots/ln_det_diff.png", width = 720)
plot(jim[["rho"]], exact - bob1, type = "line", ylim = ylim,
     xlab = "Rho", ylab = "Diff Log-Det", main = "Log-Det Comparison",
     col = "darkgreen")
lines(jim[["rho"]], exact - jim[["ln_det"]], col = "orange")
legend("bottomleft", inset = .02, 
       legend = c("jim", "bob_eigen"),
       col = c("orange", "darkgreen"), 
       lty = 1, cex = 0.8)
dev.off()
par(op)


prep_fit <- function(x, date_fit, variables) {
  
  x %>% 
    filter(date == date_fit) %>%
    ungroup() %>%
    sf:::select.sf(variables) %>% 
    sf::`st_geometry<-`(NULL) %>% 
    as.matrix(rownames.force = FALSE)
  
}


bayesian_fit <- function(
  x, counter, 
  variables, results, W, 
  lag_X = TRUE, tfe = TRUE, cfe = TRUE,
  n_draws = 1000) {
  
  library(dplyr)
  
  beta_post <- results[[counter]]$beta_post
  rho_post <- results[[counter]]$rho_post
  vars <- variables[[counter]]
  
  y_pred <- matrix(NA, nrow = nrow(x), ncol = n_draws)
  rnd <- sample(seq(1, dim(beta_post)[2]), replace = TRUE, n_draws)
  for(i in 1:n_draws) {
    beta_post_draw <- beta_post[, rnd[i]]
    rho_post_draw <- rho_post[rnd[i]]
    
    A <- Matrix::.sparseDiagonal(nrow(W)) - rho_post_draw * W
    A_inv <- solve(A)
    
    X_beta <- (1 * beta_post_draw[1] + # Constant
                 x[, -1] %*% beta_post_draw[2:(ncol(x))] + # X \beta
                 W %*% x[, -1] %*% beta_post_draw[(1 + ncol(x)):(2 * ncol(x) - 1)]) # WX \theta
    if(tfe) {
      X_beta <- X_beta + 
        mean(beta_post_draw[(2 * ncol(x)):(2 * ncol(x) + dates_len - 2)])
      # beta_post_draw[(2 * ncol(x) + dates_len - 2)] # Option 2
    }
    if(cfe) {
      X_beta <- X_beta + 
        c(0, beta_post_draw[(2 * ncol(x) + dates_len - 1):len(beta_post_draw)])
    }
    
    y_pred[, i] <- (A_inv %*% X_beta)[, 1]
  }
  
  return(y_pred)
}


plm_fit <- function(x, results, tfe, cfe) {
  
  plm_pred <- x[, -1] %*% results$coefficients
  if(tfe) {plm_pred <- plm_pred + mean(plm::fixef(results, effect = "time")})
  if(cfe) {plm_pred <- plm_pred + plm::fixef(results, effect = "individual")}
  
  return(plm_pred)
}

splm_fit <- function(x, results, W, tfe, cfe) {
  
  excl <- c()
  if("lambda" %in% names(results$coefficients)) { # SAR
    A <- Matrix::.sparseDiagonal(nrow(W)) - results$coefficients[1] * W
    A_inv <- solve(A)
    excl <- c(excl, which(names(results$coefficients) == "lambda"))
  }
  if("rho" %in% names(results$coefficients)) {
    excl <- c(excl, which(names(results$coefficients) == "rho"))
  }
  
  splm_pred <- as.numeric(results$res.eff[[1]]$intercept) + # Constant
    x[, -1] %*% results$coefficients[-excl] # X \beta
  if(tfe) {splm_pred <- splm_pred + mean(results$res.eff[[1]]$res.tfe)}
  if(cfe) {splm_pred <- splm_pred + results$res.eff[[1]]$res.sfe}
  
  if("lambda" %in% names(results$coefficients)) { # SAR
    splm_pred <- (A_inv %*% splm_pred)
  }
}


# Dependencies ------------------------------------------------------------

stopifnot(
  nzchar(system.file(package = "sf")),
  nzchar(system.file(package = "dplyr")),
  nzchar(system.file(package = "plm")),
  nzchar(system.file(package = "splm")),
  nzchar(system.file(package = "spatialreg"))
)


# Model fit ---------------------------------------------------------------

prep_fit <- function(x, date_fit, vars) {
  
  x %>% 
    filter(date == date_fit) %>%
    ungroup() %>%
    sf:::select.sf(vars) %>% 
    sf::`st_geometry<-`(NULL) %>% 
    as.matrix(rownames.force = FALSE)
  
}


bayesian_fit <- function(
  x, 
  vars, results, W, dates_len,
  lag_X = TRUE, tfe = TRUE, cfe = TRUE, tfe_idx = NULL,
  n_draws = 1000) {
  
  beta_post <- results$beta_post
  rho_post <- results$rho_post

  y_pred <- matrix(NA, nrow = nrow(x), ncol = n_draws)
  rnd <- sample(seq(1, dim(beta_post)[2]), replace = TRUE, n_draws)
  
  for(i in 1:n_draws) {
    beta_post_draw <- beta_post[, rnd[i]]
    rho_post_draw <- rho_post[rnd[i]]
    
    A <- Matrix::.sparseDiagonal(nrow(W)) - rho_post_draw * W
    A_inv <- solve(A)
    
    X_beta <- (1 * beta_post_draw[1] + # Constant
                 x[, -1] %*% beta_post_draw[2:(ncol(x))] + # X \beta
                 W %*% x[, -1] %*% 
                 beta_post_draw[(1 + ncol(x)):(2 * ncol(x) - 1)]) # WX \theta
    if(tfe) {X_beta <- X_beta + 
      if(is.null(tfe_idx)) {
        mean(beta_post_draw[(2 * ncol(x)):(2 * ncol(x) + dates_len - 2)])
        # beta_post_draw[(2 * ncol(x) + dates_len - 2)] # Option 2
      } else {
        c(0, beta_post_draw[(2 * ncol(x)):(2 * ncol(x) + dates_len - 2)])[tfe_idx]
      }
    }
    if(cfe) {
      X_beta <- X_beta + 
        c(0, beta_post_draw[(2 * ncol(x) + dates_len - 1):len(beta_post_draw)])
    }
    
    y_pred[, i] <- (A_inv %*% X_beta)[, 1]
  }
  
  return(y_pred)
}


plm_fit <- function(x, results, tfe, cfe, tfe_idx = NULL) {
  
  intercept <- names(results$coefficients)[1] == "(Intercept)"
  
  plm_pred <- if(intercept) {
    x[, -1] %*% results$coefficients[-1] + results$coefficients[1]
  } else {
    x[, -1] %*% results$coefficients
  }
  if(tfe) {plm_pred <- plm_pred + 
    if(is.null(tfe_idx)) {
      # mean(plm::fixef(results, effect = "time"))
      0
    } else {
      # plm::fixef(results, effect = "time")[tfe_idx]
      0
    }
  }
  if(cfe) {
    if(!is.null(tfe_idx)) {
    plm_pred <- plm_pred + 
      plm::fixef(results, 
                 effect = "individual")[seq(1 + (tfe_idx - 1) * len(plm_pred),
                                            tfe_idx * len(plm_pred))]
    }
  }
  
  return(c(plm_pred))
}


splm_fit <- function(x, results, W, tfe, cfe, tfe_idx = NULL) {
  
  if(!tfe & !cfe) {
    if(!is.null(results$arcoef)) {
      A <- Matrix::.sparseDiagonal(nrow(W)) - results$arcoef * W
      A_inv <- solve(A)
    }
    splm_pred <- x[, -1] %*% results$coefficients[-1] + results$coefficients[1]
    if(!is.null(results$arcoef)) {splm_pred <- (A_inv %*% splm_pred)[, 1]}
    return(c(splm_pred))
  }
  
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
  if(tfe) {splm_pred <- splm_pred + 
    if(is.null(tfe_idx)) {
      mean(results$res.eff[[1]]$res.tfe)
    } else {
      results$res.eff[[1]]$res.tfe[tfe_idx]
    }
  }
  if(cfe) {splm_pred <- splm_pred + results$res.eff[[1]]$res.sfe}
  
  if("lambda" %in% names(results$coefficients)) { # SAR
    splm_pred <- (A_inv %*% splm_pred)[, 1]
  }
  
  return(c(splm_pred))
}


# Outputs -----------------------------------------------------------------

sm_results <- function(x) {
  
  x$res_effects[2] <- round(x$res_effects[2], 7)
  x$res_effects[4] <- round(x$res_effects[4], 7)
  x$res_effects[3] <- ifelse(abs(x$res_effects[3]) > 2.576, " ***", 
                             ifelse(abs(x$res_effects[3]) > 1.96, " **",
                                    ifelse(abs(x$res_effects[3]) > 1.645, " *", "")))
  x$res_effects[5] <- ifelse(abs(x$res_effects[5]) > 2.576, " ***", 
                             ifelse(abs(x$res_effects[5]) > 1.96, " **",
                                    ifelse(abs(x$res_effects[5]) > 1.645, " *", "")))
  
  tibble("variable" = c(as.character(x$res_effects[[1]]), 
                        "Rho", "R2", "AIC", "BIC"), 
         "direct" = c(paste0(x$res_effects[[2]], x$res_effects[[3]]),
                      round(mean(x$rho_post), 3),
                      round(x$res_other[1, 2], 3),
                      round(x$res_other[3, 2], 1),
                      round(x$res_other[4, 2], 1)),
         "indirect" = c(paste0(x$res_effects[[4]], x$res_effects[[5]]),
                        "", "", "", ""))
}


bayesian_t <- function(x, ps = c(0.99, 0.95, 0.9)) {
  
  directs <- matrix(NA, nrow = length(ps), ncol = dim(x$direct_post)[1] - 1)
  for(i in seq_along(ps)) {
    directs[i, ] <- apply(get_hpdi(x$direct_post[-1, ], ps[i]), 1,
                          function(x) {0 > x[1] & 0 < x[2]})
  }
  
  indirects <- matrix(NA, nrow = length(ps), ncol = dim(x$direct_post)[1] - 1)
  for(i in seq_along(ps)) {
    indirects[i, ] <- apply(get_hpdi(x$indirect_post[-1, ], ps[i]), 1,
                            function(x) {0 > x[1] & 0 < x[2]})
  }
  
  rhos <- vector("logical", length(ps))
  for(i in seq_along(ps)) {
    tmp <- get_hpdi(x$rho_post, ps[i])
    rhos[i] <- 0 > tmp[1] & 0 < tmp[2]
  }
  
  dimnames(directs) <- dimnames(indirects) <- list(ps)
  names(rhos) <- ps
  
  list("direct_p" = !directs, "indirect_p" = !indirects, "rhos" = !rhos)
}


get_hpdi <- function(x, p = 0.95) {
  
  if(is.null(dim(x))) return(coda::HPDinterval(coda::mcmc(x), prob = p))
  
  
  coda::HPDinterval(coda::mcmc(t(x)), prob = p)
  
}


print_vars <- function(x) {
  paste0(x[1], " ~ ", paste(x[-1], collapse = " + "))
}


ssr <- function(x, y, ...) {round(sum((x - y)^2), 3)}


rmse <- function(x, y, N = len(x)) {round(sqrt(sum(x - y)^2 / N), 3)}


table_ise <- function(x, vars, stars = TRUE) {
  
  if(is(x, "plm")) return(table_ise.plm(x, vars, stars))
  if(is(x, "splm")) return(table_ise.splm(x, vars, stars))
  
  t1 <- c(x$res_effects[-1, "direct_t"], mean(x$rho_post) / sd(x$rho_post))
  t2 <- c(x$res_effects[-1, "indirect_t"])
  if(stars) {
    # Crappy LeSage
    # t1 <- ifelse(abs(t1) > 2.576, " ***", 
    #              ifelse(abs(t1) > 1.96, " **",
    #                     ifelse(abs(t1) > 1.645, " *", "")))
    # t2 <- ifelse(abs(t2) > 2.576, " ***", 
    #              ifelse(abs(t2) > 1.96, " **",
    #                     ifelse(abs(t2) > 1.645, " *", "")))
    
    # Dank HPDI
    b_t <- bayesian_t(x)
    
    star <- c("3" = " ***", "2" = " **", "1" = " *", "0" = "")
    
    t1 <- star[as.character(colSums(b_t$direct_p))]
    t2 <- star[as.character(colSums(b_t$indirect_p))]
    
    # Add rho to directs
    t1 <- c(t1, star[as.character(sum(b_t$rhos))])
  }
  
  data.frame(
    "variables" = c(vars[-1], "Rho", "R2", "SSR"),
    "direct" = round(c(x$res_effects[-1, "direct"], 
                       mean(x$rho_post), x$res_other[1, 2], x$ssr[1,1]), 3),
    "direct_t" = c(t1, NA, NA),
    "indirect" = round(c(x$res_effects[-1, "indirect"], NA, NA, NA), 3),
    "indirect_t" = c(t2, NA, NA, NA)
  )
}


table_ise.plm <- function(x, vars, stars = TRUE) {
  y <- summary(x)
  intercept <- names(x$coefficients)[1] == "(Intercept)"
  
  t1 <- if(intercept) {coef(y)[-1, "t-value"]} else {coef(y)[, "t-value"]}
  if(stars) {
    t1 <- ifelse(abs(t1) > 2.576, " ***", 
                 ifelse(abs(t1) > 1.96, " **",
                        ifelse(abs(t1) > 1.645, " *", "")))
  }
  
  data.frame(
    "variables" =  c(vars[-1], "Rho", "R2", "SSR"),
    "value" = round(c(if(intercept) {coef(x)[-1]} else {coef(x)}, 
                      NA, plm::r.squared(x), sum(x$residuals^2)), 3),
    "value_t" = c(t1, NA, NA, NA)
  )
}


table_ise.splm <- function(x, vars, stars = TRUE) {
  y <- summary(x)
  
  t1 <- c(y$CoefTable[-1, "t-value"], y$CoefTable[1, "t-value"])
  if(stars) {
    t1 <- ifelse(abs(t1) > 2.576, " ***", 
                 ifelse(abs(t1) > 1.96, " **",
                        ifelse(abs(t1) > 1.645, " *", "")))
  }
  
  data.frame(
    "variables" =  c(vars[-1], "Rho", "R2", "SSR"),
    "value" = round(c(x$coefficients[-1], 
                      x$coefficients[1], y$rsqr, y$tss), 3),
    "value_t" = c(t1, NA, NA)
  )
}

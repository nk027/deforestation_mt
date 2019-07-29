
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
    t1 <- ifelse(abs(t1) > 2.576, " ***", 
                 ifelse(abs(t1) > 1.96, " **",
                        ifelse(abs(t1) > 1.645, " *", "")))
    t2 <- ifelse(abs(t2) > 2.576, " ***", 
                 ifelse(abs(t2) > 1.96, " **",
                        ifelse(abs(t2) > 1.645, " *", "")))
  }
  
  data.frame(
    "variables" = c(vars[-1], "Rho", "R2", "SSR"),
    "direct" = c(sign(x$res_effects[-1, "direct"]), 
                 mean(x$rho_post), x$res_other[1, 2], x$ssr[1,1]),
    "direct_t" = c(t1, NA, NA),
    "indirect" = c(sign(x$res_effects[-1, "indirect"]), NA, NA, NA),
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
    "value" = c(if(intercept) {sign(coef(x)[-1])} else {sign(coef(x))}, 
                NA, plm::r.squared(x), sum(x$residuals^2)),
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
    "value" = c(sign(x$coefficients[-1]), 
                x$coefficients[1], y$rsqr, y$tss),
    "value_t" = c(t1, NA, NA)
  )
}

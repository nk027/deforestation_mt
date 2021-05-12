
# Dependencies ------------------------------------------------------------

stopifnot(
  file.exists("data/data.rds"),
  require("Matrix"),
  require("sf"),
  require("dplyr"),
  require("spatialreg"),
  require("coda")
)

source("R/51_supp.R")
source("R/52_helpers.R")


# Analyse -----------------------------------------------------------------

data <- readRDS("data/data.rds")

models <- load("data/est_extended_qu.rda")

summary(out_sdm)
summary(out_sar)
summary(out_slx)
summary(out_clm)

plot(out_sdm)

eff_sdm <- effects(out_sdm, n_draw = 10000)


# Magnitude results -----

km2 <- data %>% filter(date == 2017) %>%
  select(area_m2) %>% .$area_m2 / 1e6
def <- function(prediction) {
  prediction * km2 / 100 # Deforestation from ha/km^2 to km^2
}


# Baseline ---
x <- out_sdm

X <- X2a <- X2b <- X3a <- X3b <- tail(x$meta$X, 141L)
beta <- apply(x$beta, 2, mean)
W <- x$meta$W[1:141, 1:141]
rho <- mean(x$rho)

# Croplands (indirect and both)
X2a[, 1 + length(x$meta$var_names) + which(grepl("crop", x$meta$var))] <-
  X[, 1 + length(x$meta$var_names) + which(grepl("crop", x$meta$var))] * .9
X2b[, c(1 + which(grepl("crop", x$meta$var)),
  1 + length(x$meta$var_names) + which(grepl("crop", x$meta$var)))] <-
  X[, c(1 + which(grepl("crop", x$meta$var)),
    1 + length(x$meta$var_names) + which(grepl("crop", x$meta$var)))] * .9
# Cattle (direct and both)
X3a[, 1 + which(grepl("cattle", x$meta$var))] <-
  log(exp(X[, 1 + which(grepl("cattle", x$meta$var))]) * .9)
X3b[, c(1 + which(grepl("cattle", x$meta$var)),
  1 + length(x$meta$var_names) + which(grepl("cattle", x$meta$var)))] <-
  log(exp(X[, c(1 + which(grepl("cattle", x$meta$var)),
    1 + length(x$meta$var_names) + which(grepl("cattle", x$meta$var)))]) * .9)

sum(fit <- X %*% beta) # Plain
sum(fit_2a <- X2a %*% beta) # Less indirect croplands
sum(fit_2b <- X2b %*% beta) # Less croplands
sum(fit_3a <- X3a %*% beta) # Less direct cattle (count)
sum(fit_3b <- X3b %*% beta) # Less cattle (count)

pred_true <- x$meta$y
pred <- solve(diag(141) - rho * W, fit)
pred_2a <- solve(diag(141) - rho * W, fit_2a)
pred_2b <- solve(diag(141) - rho * W, fit_2b)
pred_3a <- solve(diag(141) - rho * W, fit_3a)
pred_3b <- solve(diag(141) - rho * W, fit_3b)

summary(pred)
summary(pred_2a)
summary(pred_2b)
summary(pred_3a)
summary(pred_2b)


# Classical ---
x <- out_clm

X <- X2a <- X3a <- tail(x$meta$X, 141L)
beta <- apply(x$beta, 2, mean)

# Croplands
X2a[, 1 + which(grepl("crop", x$meta$var))] <-
  X[, 1 + which(grepl("crop", x$meta$var))] * .9
# Cattle
X3a[, 1 + which(grepl("cattle", x$meta$var))] <-
  log(exp(X[, 1 + which(grepl("cattle", x$meta$var))]) * .9)

summary(pred <- X %*% beta) # Plain
summary(pred_2a <- X2b %*% beta) # Less croplands
summary(pred_3a <- X3a %*% beta) # Less cattle (count)



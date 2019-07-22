
# Chow test ---------------------------------------------------------------

matrices1 <- list()
matrices2 <- list()
results_qu1 <- list()
results_kn1 <- list()
results_qu2 <- list()
results_kn2 <- list()

# Breaks: 2008, 2010
dates1 <- 2005:2008
dates1_len <- length(dates1)
dates2 <- 2009:2015
dates2_len <- length(dates2)

for(counter in seq_along(variables)) {
  matrices1[[counter]] <- get_matr(data, variables[[counter]], dates = dates1)
  results_qu1[[counter]] <- sdm_panel(matrices1[[counter]], W_qu, dates1_len)
  results_kn1[[counter]] <- sdm_panel(matrices1[[counter]], W_kn, dates1_len)
  
  matrices2[[counter]] <- get_matr(data, variables[[counter]], dates = dates2)
  results_qu2[[counter]] <- sdm_panel(matrices2[[counter]], W_qu, dates2_len)
  results_kn2[[counter]] <- sdm_panel(matrices2[[counter]], W_kn, dates2_len)
}

i <- 1
chow <- 
  (results_qu[[i]]$ssr - (results_qu1[[i]]$ssr + results_qu2[[i]]$ssr)) / (length(variables[[i]]) - 1) /
  (results_qu1[[i]]$ssr + results_qu2[[i]]$ssr) / (nrow(matrices1[[i]]) + nrow(matrices2[[i]]) - 2 * (length(variables[[i]]) - 1))

significance <- qf(0.95, (length(variables[[i]]) - 1), (nrow(matrices1[[i]]) + nrow(matrices2[[i]]) - 2 * (length(variables[[i]]) - 1)))
# chow > significance
chow
i <- i + 1


# PLM tests ---------------------------------------------------------------

library(plm)

# Hausman
out_f <- plm::plm(formula_ify(variables[[counter]]), df_plm, 
                  effect = effect, model = "within", type = "dfirst")
out_r <- plm::plm(formula_ify(variables[[counter]]), df_plm, 
                  effect = effect, model = "random")
phtest(out_f, out_r)

# LM
plmtest(formula_ify(variables[[counter]]), df_plm, effect = effect)

# PCD
pcdtest(formula_ify(variables[[counter]]), df_plm, effect = effect)


# spdep tests -------------------------------------------------------------

library(spdep)

# Moran's I
moran.mc(out_f$residuals, mat2listw(kronecker(diag(dates_len), W_qu)), 1000)
moran.mc(out_f$residuals, mat2listw(kronecker(diag(dates_len), W_k5n)), 1000)
moran.mc(out_f$residuals, mat2listw(kronecker(diag(dates_len), W_k7n)), 1000)

# Moran's I for multiple subsets
print_I <- function(type, x) {
  p <- x$p.value
  p <- if(p < 0.01) "***" else if(p < 0.05) "**" else if(p < 0.10) "*" else ""
  cat(type, " - I = ", x$statistic, p, "\n", sep = "")
}

for(i in dates[1]:dates[len(dates) - 1]) {
  for(j in dates[2]:dates[len(dates)]) {
    if(j > i) {
      cat("\n\t", i, " - ", j, "\n", sep = "")
      x <- data$forest_ch_km2[data$date >= i & data$date <= j]
      print_I("qu",
              moran.mc(x, mat2listw(kronecker(diag(len(i:j)), W_qu)), 1000)
      )
      print_I("k5",
              moran.mc(x, mat2listw(kronecker(diag(len(i:j)), W_kn)), 1000)
      )
      print_I("k7", 
              moran.mc(x, mat2listw(kronecker(diag(len(i:j)), W_kn7)), 1000)
      )
    }
  }
}


library(dplyr)
library(spdep)
source("R/7_functions.R")

data <- readRDS("data/data.rds")


# Moran's -----------------------------------------------------------------

W_qu <- get_W(data, type = "queen")
W_kn <- get_W(data, type = "knear", k = 5)
W_kn7 <- get_W(data, type = "knear", k = 7)

dates <- c(2002, 2017)
dates_len <- length(dates[1]:dates[2])

print_I <- function(type, x) {
  p <- x$p.value
  p <- if(p < 0.01) "***" else if(p < 0.05) "**" else if(p < 0.10) "*" else ""
  cat(type, " - I = ", x$statistic, p, "\n", sep = "")
}

for(i in 2002:2016) {
  for(j in 2003:2017) {
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

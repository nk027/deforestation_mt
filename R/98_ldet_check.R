
library("spatialreg")

bivand <- new.env()
bivand$listw <- spdep::mat2listw(Ws$queen)
bivand$can.sim <- FALSE
bivand$verbose <- TRUE
bivand$n <- 141
bivand$family <- "SDM"

jacobianSetup("eigen", env = bivand)
jacobianSetup("LU", env = bivand)

eigen_setup(bivand)
mcdet_setup(bivand)

x <- get_ln_det(Ws$queen)

eig <- mc <- vector("numeric", length(x[[1]]))

for(i in seq_along(x[[1]])) {
  eigen_setup(bivand)
  eig[i] <- do_ldet(x[[2]][i], env = bivand)
  mcdet_setup(bivand)
  mc[i] <- do_ldet(x[[2]][i], env = bivand)
}

plot(x[[1]], type = "line")
lines(eig, col = "darkblue")
lines(mc, col = "darkgreen")

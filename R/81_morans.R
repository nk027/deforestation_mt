
# Dependencies ------------------------------------------------------------

stopifnot(
  # Needs parts of 55
  exists("data"), exists("dates"), exists("dates_len"), exists("W_qu"),
  require("spdep")
)


# Get Moran's statistics --------------------------------------------------

dep <- data$forest_ch_km2[data$date >= dates[1] & 
                          data$date <= dates[dates_len]]
listw <- mat2listw(kronecker(diag(dates_len), W_qu))

(mor <- moran.test(dep, listw))

(mor_mc <- moran.mc(dep, listw, 10000L))

(mor_loc <- localmoran(dep, listw))

summary(mor_loc)

# spdep weights matrix
get_W <- function(x, type = c("queen", "knear"), k = 5) {
  
  library(spdep)
  library(dplyr)
  
  type <- match.arg(type)
  
  nb <- x %>% dplyr::filter(date == 2015) %>% as_Spatial()
  
  if(type == "queen") {
    return(
      listw2mat(
        nb2listw(
          poly2nb(nb, row.names = nb$code, queen = TRUE),
          style = "W"))
    )
  } else if(type == "knear") {
    # Thanks spdep
    return(
      listw2mat(
        nb2listw(
          knn2nb(
            knearneigh(
              sp::coordinates(nb), 
              k = k))))
    )
  }
  
}

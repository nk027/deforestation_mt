# get matrix from sf data
get_matr <- function(x, variables, dates) {
  
  library(dplyr)
  
  matr <- x %>% 
    filter(date >= dates[1], date <= dates[2]) %>% 
    ungroup() %>%
    sf:::select.sf(variables) %>% 
    sf::`st_geometry<-`(NULL) %>% 
    as.matrix(matr, rownames.force = FALSE)
}

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

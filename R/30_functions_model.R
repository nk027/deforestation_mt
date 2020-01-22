
# Dependencies ------------------------------------------------------------

stopifnot(
  nzchar(system.file(package = "sf")),
  nzchar(system.file(package = "dplyr")),
  nzchar(system.file(package = "spdep")),
  nzchar(system.file(package = "MASS")),
  nzchar(system.file(package = "Matrix")),
  nzchar(system.file(package = "matrixcalc"))
)


# Get matrix from sf data -------------------------------------------------

get_matr <- function(x, variables, dates) {

  x %>%
    filter(date %in% dates) %>%
    ungroup() %>%
    sf:::select.sf(variables) %>%
    sf::`st_geometry<-`(NULL) %>%
    as.matrix(matr, rownames.force = FALSE)
}


# spdep weights matrix ----------------------------------------------------

get_W <- function(x, type = c("queen", "knear"), k = 5) {

  type <- match.arg(type)

  nb <- x %>% dplyr::filter(date == 2015) %>% sf::as_Spatial()

  if(type == "queen") {
    return(
      spdep::listw2mat(
        spdep::nb2listw(
          spdep::poly2nb(nb, row.names = nb$code, queen = TRUE), style = "W"))
    )
  } else if(type == "knear") {
    return(
      spdep::listw2mat(
        spdep::nb2listw(
          spdep::knn2nb(
            spdep::knearneigh(sp::coordinates(nb), k = k))))
    )
  }

}


# Helpers -----------------------------------------------------------------

formula_ify <- function(x) { # To convert this for plm & splm
  as.formula(paste0(x[1], " ~ ", paste(x[-1], collapse = " + ")), env = globalenv())
}

library(splm)
library(spdep)
library(sf)

shp <- readRDS("data/data.rds")
nb <- shp %>% dplyr::filter(date == 2017) %>% 
  as_Spatial()

qu_nb <- poly2nb(nb, row.names = nb$code, queen = TRUE)
w_qu_nb <- nb2listw(qu_nb, style = "W", zero.policy = TRUE)
w_queen <- listw2mat(w_qu_nb)

# we can also plot weights matrices
plot(nb)
plot(qu_nb, coordinates(nb), add = TRUE, col = "green", cex = 0.5)

shp_sel <- shp %>% dplyr::filter(date %in% 2005:2016)

x <- spml(forest ~ pasture + crop + gdp + pop + area, data = shp_sel, 
          index = c("code", "date"), listw = w_qu_nb)
summary(x)
x <- spml(forest ~ pasture + crop + gdp + pop + area, data = shp_sel, 
          index = c("code", "date"), listw = w_qu_nb, spatial.error = "b")
summary(x)
x <- spml(forest ~ pasture + crop + gdp + pop + area, data = shp_sel, 
          index = c("code", "date"), listw = w_qu_nb, model = "random", 
          spatial.error = "none", lag = TRUE)
summary(x)

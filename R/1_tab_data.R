pop <- as_tibble(readODS::read_ods("data/pop.ods", skip = 4, 
                                   col_names = FALSE, col_types = NA))
names(pop) <- c("id", "name", 
                paste0("y", formatC(c(1:6, 8:9, 11:18), width = 2, flag = "0")))
tail(pop)
pop <- pop[-nrow(pop), ]
for(int in c(1, 3:18)) class(pop[[int]]) <- "integer"

gdp <- as_tibble(readODS::read_ods("data/gdp.ods", skip = 4,
                                   col_names = FALSE, col_types = NA))
names(gdp) <- c("id", "name", 
                paste0("y", formatC(c(2:16), width = 2, flag = "0")))
tail(gdp)
gdp <- gdp[-nrow(gdp), ]
class(gdp[[1]]) <- "integer"
for(dbl in 3:17) {
  gdp[[dbl]] <- gsub("6", gdp[[dbl]])
  class(gdp[[dbl]]) <- "double"
}

crop_ton <- readODS::read_ods("data/crop.ods", sheet = 3, skip = 5,
                              col_names = FALSE, col_types = NA)
crop_price <- readODS::read_ods("data/crop.ods", sheet = 5, skip = 5,
                                col_names = FALSE, col_types = NA)

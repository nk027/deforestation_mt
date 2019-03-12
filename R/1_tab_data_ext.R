library(dplyr)
library(readODS)

pop <- readRDS("data/sidra/pop_long.rds")

pop07 <- as_tibble(read_ods("data/sidra/pop2007.ods", skip = 4, 
                            col_names = FALSE, col_types = NULL))
names(pop07) <- c("code", "name", "pop")
pop07 <- pop07[-nrow(pop07), ]
for(int in c(1, 3)) class(pop07[[int]]) <- "integer"

pop10 <- as_tibble(read_ods("data/sidra/pop2010.ods", skip = 6, 
                            col_names = FALSE, col_types = NULL))
names(pop10) <- c("code", "name", "race", "pop", "pop10_urb", "pop10_rur")
pop10 <- pop10[-nrow(pop10), ]
for(int in c(1, 4:6)) class(pop10[[int]]) <- "integer"

get_state <- function(x, pattern = "[(]MT[)]") {
  out <- grep(pattern, x)
  cat("Matches in order:", all(out == out[1]:out[length(out)]), "\n")
  out
}

na_locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v) + 1]
}

pop10$code <- na_locf(pop10$code)
pop10$name <- na_locf(pop10$name)
pop10 <- pop10[get_state(pop10$name), ]
pop10_ind <- pop10[get_state(pop10$race, pattern = "Indígena"), ]
pop10 <- pop10[get_state(pop10$race, pattern = "Total"), ]
pop07 <- pop07[get_state(pop07$name), ]

pop07$date <- 2007
pop10$date <- 2010

pop <- rbind(pop, 
             pop07[c("code", "pop", "date")],
             pop10[c("code", "pop", "date")])

saveRDS(pop, "data/sidra/pop_long_full.rds")


# Forestry ----------------------------------------------------------------

forestry <- as_tibble(read_ods("data/sidra/forestry.ods", sheet = 2, skip = 5,
                               col_names = FALSE, col_types = NULL))
names(forestry) <- c("code", "name", 
                     paste0("forestry", formatC(0:17, width = 2, flag = "0")))
forestry <- forestry[-nrow(forestry), ]
for(int in c(1, 3:ncol(forestry))) class(forestry[[int]]) <- "integer"

forestry <- forestry[get_state(forestry$name), ]

long_cat <- function(x, variable) {
  x <- x[c(1, grep(variable, names(x)))]
  x <- reshape2::melt(x, id.vars = "code", stringsAsFactors = FALSE)
  x$variable <- as.character(x$variable)
  x$date <- 2000L + as.integer(substr(x$variable, nchar(x$variable) - 1, nchar(x$variable)))
  names(x)[names(x) == "value"] <- substr(x$variable, 1, nchar(x$variable) - 2)[1]
  x$variable <- NULL
  x
}

variables <- "forestry"

forestry_long <- long_cat(forestry, variables)
saveRDS(forestry_long, "data/sidra/forestry_long.rds")

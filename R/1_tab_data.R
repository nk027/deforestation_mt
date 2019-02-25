library(dplyr)


# read in

pop <- as_tibble(readODS::read_ods("data/pop.ods", skip = 4, 
                                   col_names = FALSE, col_types = NA))
names(pop) <- c("code", "name", 
                paste0("pop", formatC(c(1:6, 8:9, 11:18), width = 2, flag = "0")))
pop <- pop[-nrow(pop), ]
for(int in c(1, 3:ncol(pop))) class(pop[[int]]) <- "integer"
saveRDS(pop, "data/pop.rds")

gdp <- as_tibble(readODS::read_ods("data/gdp.ods", skip = 4,
                                   col_names = FALSE, col_types = NA))
names(gdp) <- c("code", "name", 
                paste0("gdp", formatC(c(2:16), width = 2, flag = "0")))
gdp <- gdp[-nrow(gdp), ]
class(gdp[[1]]) <- "integer"
for(dbl in 3:ncol(gdp)) {
  gdp[[dbl]] <- gsub("^([0-9]+),([0-9]+)$", "\\1.\\2", gdp[[dbl]])
  class(gdp[[dbl]]) <- "double"
}
saveRDS(gdp, "data/gdp.rds")

crop_ton <- as_tibble(readODS::read_ods("data/crop.ods", sheet = 3, skip = 5,
                                        col_names = FALSE, col_types = NA))
crop_price <- as_tibble(readODS::read_ods("data/crop.ods", sheet = 5, skip = 5,
                                          col_names = FALSE, col_types = NA))
names(crop_ton) <- names(crop_price) <- c("code", "name", 
  paste0(c("total", "cotton_a", "cotton_b", "banana",
           "potato", "cocoa", "coffee", "sugarcane",
           "mate", "soy", "sorghum", "wheat"),
         formatC(rep(0:17, each = 12), width = 2, flag = "0")))
crop_ton <- crop_ton[-nrow(crop_ton), ]
crop_price <- crop_price[-nrow(crop_price), ]
for(int in c(1, 3:ncol(crop_ton))) class(crop_ton[[int]]) <- "integer"
for(int in c(1, 3:ncol(crop_price))) class(crop_price[[int]]) <- "integer"
saveRDS(crop_ton, "data/crop_ton.rds")
saveRDS(crop_price, "data/crop_price.rds")


# transform

pop <- readRDS("data/pop.rds")
gdp <- readRDS("data/gdp.rds")
crop_ton <- readRDS("data/crop_ton.rds")
crop_price <- readRDS("data/crop_price.rds")

get_state <- function(x, pattern = "[(]MT[)]") {
  out <- grep(pattern, x$name)
  cat("Matches in order:", all(out == out [1]:out[length(out)]), "\n")
  out
}

pop <- pop[get_state(pop), ]
gdp <- gdp[get_state(gdp), ]
crop_ton <- crop_ton[get_state(crop_ton), ]
crop_price <- crop_price[get_state(crop_price), ]

# x <- as_tibble(t(gdp[c(-1, -2)]))
# names(x) <- gdp$code
# x$date <- 2002:2016
x <- as_tibble(t(pop[c(-1, -2)]))
names(x) <- pop$code
x$date <- 2000 + c(1:6, 8:9, 11:18)

library(ggplot2)

i <- 1
x[c(((i + 1) * 18 - 35):min((i * 18), 141), 142)] %>% 
reshape2::melt(id.vars = "date") %>% 
ggplot(aes(x = date, colour = variable, y = value)) +
  geom_line()
i <- i + 1
# gdp:
# Cuiabá, 5103403, grows rapidly
# Paranaíta, 5106299, rises sharply in 12, plateaus 13-15 and then falls off.

socio <- left_join(gdp, pop, by = c("code", "name"))


crops <- c("total", "cotton_a", "cotton_b", "banana", "potato", "cocoa", 
           "coffee", "sugarcane", "mate", "soy", "sorghum", "wheat")

i <- 1
summary(crop_price[grep(crops[i], names(crop_price))])
i <- i + 1
# drop total, cotton, potato, cocoa, coffee, mate, wheat

drop <- c(sapply(c("total", "cotton_a", "potato", "cocoa", "coffee", "mate", "wheat"), 
                 grep, names(crop_ton)))
crop_ton <- crop_ton[-drop]

drop <- c(sapply(c("total", "cotton_a", "potato", "cocoa", "coffee", "mate", "wheat"), 
                 grep, names(crop_price)))
crop_price <- crop_price[-drop]

names(crop_ton) <- gsub("banana", "ton_ban", names(crop_ton))
names(crop_ton) <- gsub("sugarcane", "ton_sug", names(crop_ton))
names(crop_ton) <- gsub("soy", "ton_soy", names(crop_ton))
names(crop_ton) <- gsub("sorghum", "ton_sor", names(crop_ton))

names(crop_price) <- gsub("banana", "pr_ban", names(crop_price))
names(crop_price) <- gsub("sugarcane", "pr_sug", names(crop_price))
names(crop_price) <- gsub("soy", "pr_soy", names(crop_price))
names(crop_price) <- gsub("sorghum", "pr_sor", names(crop_price))

crop <- left_join(crop_ton, crop_price, by = c("id", "name"))
saveRDS(crop, "data/crop.rds")

long_cat <- function(x, variable) {
  x <- x[c(1, grep(variable, names(x)))]
  x <- reshape2::melt(x, id.vars = "code", stringsAsFactors = FALSE)
  x$variable <- as.character(x$variable)
  x$date <- 2000 + as.integer(substr(x$variable,
                                     nchar(x$variable) - 1,
                                     nchar(x$variable)))
  names(x)[names(x) == "value"] <- substr(x$variable, 1, nchar(x$variable) - 2)[1]
  x$variable <- NULL
  x
}

variables <- c(paste0("pr_", c("ban", "sug", "soy", "sor")),
               paste0("ton_", c("ban", "sug", "soy", "sor")))

long <- vector("list", length(variables))
for(i in seq_along(variables)) {
  long[[i]] <- long_cat(crop, variables[i])
}

x <- Reduce(function(df1, df2) left_join(df1, df2, by = c("code", "date")), long)

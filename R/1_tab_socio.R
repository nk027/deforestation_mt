
library(dplyr)
library(readODS)
source("R/1_functions.R")

# Read in GDP and population (IBGE) ---------------------------------------

pop <- as_tibble(read_ods("data/sidra/pop.ods", skip = 4, 
                          col_names = FALSE, col_types = NA))
names(pop) <- c("code", "name", 
                paste0("pop", formatC(c(1:6, 8:9, 11:18), width = 2, flag = "0")))
pop <- pop[-nrow(pop), ] # Contains source
for(int in c(1, 3:ncol(pop))) class(pop[[int]]) <- "integer"


gdp <- as_tibble(read_ods("data/sidra/gdp.ods", skip = 4,
                          col_names = FALSE, col_types = NA))
names(gdp) <- c("code", "name", 
                paste0("gdp", formatC(c(2:16), width = 2, flag = "0")))
gdp <- gdp[-nrow(gdp), ] # Contains source
class(gdp[[1]]) <- "integer"
for(dbl in 3:ncol(gdp)) {
  # gdp[[dbl]] <- gsub("^([0-9]+),([0-9]+)$", "\\1.\\2", gdp[[dbl]])
  class(gdp[[dbl]]) <- "double"
}

# Subset to MT
pop <- pop[get_state(pop$name), ]
gdp <- gdp[get_state(gdp$name), ]


# Transform to long format ------------------------------------------------

# GDP
variables <- names(gdp)[c(-1, -2)]
long <- vector("list", length(variables))
for(i in seq_along(variables)) {
  long[[i]] <- long_cat(gdp, variables[i])
}
gdp_long <- Reduce(function(df1, df2) rbind(df1, df2), long)
saveRDS(gdp_long, "data/tab/gdp.rds")

# Population
variables <- names(pop)[c(-1, -2)]
long <- vector("list", length(variables))
for(i in seq_along(variables)) {
  long[[i]] <- long_cat(pop, variables[i])
}
pop_long <- Reduce(function(df1, df2) rbind(df1, df2), long)

# Add census population data for 2007 and 2010
pop07 <- as_tibble(read_ods("data/sidra/pop2007.ods", skip = 4, 
                            col_names = FALSE, col_types = NULL))
names(pop07) <- c("code", "name", "pop")
pop07 <- pop07[-nrow(pop07), ] # Contains source
for(int in c(1, 3)) class(pop07[[int]]) <- "integer"

pop10 <- as_tibble(read_ods("data/sidra/pop2010.ods", skip = 6, 
                            col_names = FALSE, col_types = NULL))
names(pop10) <- c("code", "name", "race", "pop", "pop_urb", "pop_rur")
pop10 <- pop10[-nrow(pop10), ] # Contains source
for(int in c(1, 4:6)) class(pop10[[int]]) <- "integer"

# 2010 contains an extra row with indigenous population
pop10$code <- na_locf(pop10$code)
pop10$name <- na_locf(pop10$name)
pop10 <- pop10[get_state(pop10$name), ]
pop10_ind <- pop10[get_state(pop10$race, pattern = "Indígena"), ]
pop10 <- pop10[get_state(pop10$race, pattern = "Total"), ]
pop07 <- pop07[get_state(pop07$name), ]

# Bind for the full timeseries
pop07$date <- 2007
pop10$date <- 2010
pop <- rbind(pop_long, 
             pop07[c("code", "pop", "date")],
             pop10[c("code", "pop", "date")])

saveRDS(pop, "data/tab/pop_long.rds")


# Indigenous and urban/rural ----------------------------------------------

pop10$pop_ind <- pop10_ind$pop
saveRDS(pop10[c("code", "pop", "pop_urb", "pop_rur", "pop_ind")], 
        "data/tab/pop_detail.rds")


library(dplyr)
library(readODS)
source("R/1_functions.R")


# Forestry ----------------------------------------------------------------

forestry_quant <- read_sidra("data/sidra/forestry.ods", 
                             sheet = 1, grep_vars = "3")
forestry_quant <- as_tibble(forestry_quant[get_state(forestry_quant$name), ])
saveRDS(forestry_quant, "data/tab/forestry_quant.rds")


forestry_value <- read_sidra("data/sidra/forestry.ods", 
                             sheet = 2, grep_vars = "3")
forestry_value <- as_tibble(forestry_value[get_state(forestry_value$name), ])
saveRDS(forestry_value, "data/tab/forestry_value.rds")


# Vegetables --------------------------------------------------------------

veggies_quant <- read_sidra("data/sidra/vegetables.ods", 
                               sheet = 1, grep_vars = "2")
veggies_quant <- as_tibble(veggies_quant[get_state(veggies_quant$name), ])
saveRDS(veggies_quant, "data/tab/veggies_quant.rds")


veggies_value <- read_sidra("data/sidra/vegetables.ods", 
                               sheet = 2, grep_vars = "2")
veggies_value <- as_tibble(veggies_value[get_state(veggies_value$name), ])
saveRDS(veggies_value, "data/tab/veggies_value.rds")

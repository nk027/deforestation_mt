
library(dplyr)
library(readODS)
source("R/1_functions.R")


# Animal produce ----------------------------------------------------------

animal_quant <- read_sidra("data/sidra/animal_products.ods", sheet = 1)
animal_quant <- as_tibble(animal_quant[get_state(animal_quant$name), ])
saveRDS(animal_quant, "data/tab/animal_quant.rds")


animal_value <- read_sidra("data/sidra/animal_products.ods", sheet = 2)
animal_value <- as_tibble(animal_value[get_state(animal_value$name), ])
saveRDS(forestry_quant, "data/tab/animal_value.rds")


# Herd size ---------------------------------------------------------------

milk_cows <- read_sidra("data/sidra/milk_cows.ods", sheet = 1)
milk_cows <- as_tibble(milk_cows[get_state(milk_cows$name), ])
saveRDS(milk_cows, "data/tab/milk_cows.rds")

herd_sizes <- read_sidra("data/sidra/herd_sizes.ods", sheet = 1)
# Suíno and Galináceos appear multiple times

# Of Suíno and Galináceos the total (in position 1) is kept
herd_sizes <- herd_sizes %>% adj_first("Suíno") %>% adj_first("Galináceos")
herd_sizes <- as_tibble(herd_sizes[get_state(herd_sizes$name), ])
saveRDS(herd_sizes, "data/tab/herd_sizes.rds")

# The head count of wool producing sheep is not available for the Legal Amazon
# sheep_wool <- read_sidra("data/sidra/sheep_wool", sheet = 1)
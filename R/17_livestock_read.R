
# Dependencies ------------------------------------------------------------

stopifnot(
  length(list.files("data/sidra", "animal")) > 0,
  length(list.files("data/sidra", "milk_cows")) > 0,
  length(list.files("data/sidra", "herd_sizes")) > 0,
  require(dplyr),
  nzchar(system.file(package = "readODS"))
)
source("R/16_functions_read.R")


# Animal produce ----------------------------------------------------------

animal_quant <- read_sidra("data/sidra/animal_products.ods", sheet = 1)

# Subset to Mato Grosso
animal_quant <- as_tibble(animal_quant[get_state(animal_quant$name), ])

saveRDS(animal_quant, "data/tab/animal_quant.rds")


animal_value <- read_sidra("data/sidra/animal_products.ods", sheet = 2)

# Subset to Mato Grosso
animal_value <- as_tibble(animal_value[get_state(animal_value$name), ])

saveRDS(animal_value, "data/tab/animal_value.rds")


# Herd size ---------------------------------------------------------------

# General herd sizes
herd_sizes <- read_sidra("data/sidra/herd_sizes.ods", sheet = 1)

# Suíno and Galináceos appear multiple times - the total (in position 1) is kept
herd_sizes <- herd_sizes %>% 
  adj_first("Suíno") %>% adj_first("Galináceos")

# Subset to Mato Grosso
herd_sizes <- as_tibble(herd_sizes[get_state(herd_sizes$name), ])

saveRDS(herd_sizes, "data/tab/herd_sizes.rds")


# Milk cows
milk_cows <- read_sidra("data/sidra/milk_cows.ods", sheet = 1)

# Subset to Mato Grosso
milk_cows <- as_tibble(milk_cows[get_state(milk_cows$name), ])

saveRDS(milk_cows, "data/tab/milk_cows.rds")


# The head count of wool producing sheep is not available for the Legal Amazon
# wool_sheep <- read_sidra("data/sidra/wool_sheep", sheet = 1)


detach("package:dplyr")

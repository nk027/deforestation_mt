
# Dependencies ------------------------------------------------------------

stopifnot(
  length(list.files("data/sidra", "forestry")) > 0,
  length(list.files("data/sidra", "vegetables")) > 0,
  require(dplyr),
  nzchar(system.file(package = "readODS"))
)
source("R/16_functions_read.R")


# Forestry ----------------------------------------------------------------

# Here we grab values from the 3rd level (total - categories - sub-categories)

# Tons, etc.
forestry_quant <- read_sidra("data/sidra/forestry.ods", 1, grep_vars = "3")

# Subset to Mato Grosso
forestry_quant <- as_tibble(forestry_quant[get_state(forestry_quant$name), ])

saveRDS(forestry_quant, "data/tab/forestry_quant.rds")


# Value
forestry_value <- read_sidra("data/sidra/forestry.ods", 2, grep_vars = "3")

# Subset to Mato Grosso
forestry_value <- as_tibble(forestry_value[get_state(forestry_value$name), ])

saveRDS(forestry_value, "data/tab/forestry_value.rds")


# Vegetables --------------------------------------------------------------

# Here we grab values from the 2nd level (total - categories - sub-categories)

# Tons, etc.
veggies_quant <- read_sidra("data/sidra/vegetables.ods", 1, grep_vars = "2")

# Subset to Mato Grosso
veggies_quant <- as_tibble(veggies_quant[get_state(veggies_quant$name), ])

saveRDS(veggies_quant, "data/tab/veggies_quant.rds")


# Value
veggies_value <- read_sidra("data/sidra/vegetables.ods", 2, grep_vars = "2")

# Subset to Mato Grosso
veggies_value <- as_tibble(veggies_value[get_state(veggies_value$name), ])

saveRDS(veggies_value, "data/tab/veggies_value.rds")


detach("package:dplyr")

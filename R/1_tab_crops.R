
library(dplyr)
library(readODS)
source("R/1_functions.R")


# Adjustments -------------------------------------------------------------

# Batata, Borracha and Café appear multiple times

# Batata (-doce & -inglesa) is collapsed
adj_batata <- function(x) adj_collapse(x, "Batata")

# Borracha (látex coagulado & látex líquido) is collapsed
adj_borracha <- function(x) adj_collapse(x, "Borracha")

# Of Café (Total, Arábica & Canephora) the total (in position 1) is kept
adj_cafe <- function(x) adj_first(x, "Café")


# Crop area ---------------------------------------------------------------

crop_planted_1 <- read_sidra("data/sidra/crop_area_2.ods", sheet = 1)
crop_planted_2 <- read_sidra("data/sidra/crop_area_3.ods", sheet = 1)
crop_planted_3 <- read_sidra("data/sidra/crop_area_4.ods", sheet = 1)
crop_planted_4 <- read_sidra("data/sidra/crop_area_5.ods", sheet = 1)

crop_planted <- rbind(crop_planted_1, crop_planted_2,
                      crop_planted_3, crop_planted_4)
rm(crop_planted_1, crop_planted_2, crop_planted_3, crop_planted_4)

crop_planted <- crop_planted %>% 
  adj_batata() %>% adj_borracha() %>% adj_cafe()
crop_planted <- as_tibble(crop_planted[get_state(crop_planted$name), ])
saveRDS(crop_planted, "data/tab/crop_planted.rds")


crop_harvested_1 <- read_sidra("data/sidra/crop_area_2.ods", sheet = 2)
crop_harvested_2 <- read_sidra("data/sidra/crop_area_3.ods", sheet = 2)
crop_harvested_3 <- read_sidra("data/sidra/crop_area_4.ods", sheet = 2)
crop_harvested_4 <- read_sidra("data/sidra/crop_area_5.ods", sheet = 2)

crop_harvested <- rbind(crop_harvested_1, crop_harvested_2,
                        crop_harvested_3, crop_harvested_4)
rm(crop_harvested_1, crop_harvested_2, crop_harvested_3, crop_harvested_4)

crop_harvested <- crop_harvested %>% 
  adj_batata() %>% adj_borracha() %>% adj_cafe()
crop_harvested <- as_tibble(crop_harvested[get_state(crop_harvested$name), ])
saveRDS(crop_harvested, "data/tab/crop_harvested.rds")


# Crop quantities ---------------------------------------------------------

crop_quant_1 <- read_sidra("data/sidra/crop_quantities_2.ods", sheet = 1)
crop_quant_2 <- read_sidra("data/sidra/crop_quantities_3.ods", sheet = 1)
crop_quant_3 <- read_sidra("data/sidra/crop_quantities_4.ods", sheet = 1)
crop_quant_4 <- read_sidra("data/sidra/crop_quantities_5.ods", sheet = 1)

crop_quant <- rbind(crop_quant_1, crop_quant_2, crop_quant_3, crop_quant_4)
rm(crop_quant_1, crop_quant_2, crop_quant_3, crop_quant_4)

crop_quant <- crop_quant %>% 
  adj_batata() %>% adj_borracha() %>% adj_cafe()
crop_quant <- as_tibble(crop_quant[get_state(crop_quant$name), ])
saveRDS(crop_quant, "data/tab/crop_quant.rds")


crop_value_1 <- read_sidra("data/sidra/crop_quantities_2.ods", sheet = 2)
crop_value_2 <- read_sidra("data/sidra/crop_quantities_3.ods", sheet = 2)
crop_value_3 <- read_sidra("data/sidra/crop_quantities_4.ods", sheet = 2)
crop_value_4 <- read_sidra("data/sidra/crop_quantities_5.ods", sheet = 2)

crop_value <- rbind(crop_value_1, crop_value_2, crop_value_3, crop_value_4)
rm(crop_value_1, crop_value_2, crop_value_3, crop_value_4)

crop_value <- crop_value %>% 
  adj_batata() %>% adj_borracha() %>% adj_cafe()
crop_value <- as_tibble(crop_value[get_state(crop_value$name), ])
saveRDS(crop_value, "data/tab/crop_value.rds")

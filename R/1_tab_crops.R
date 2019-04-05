
library(dplyr)
library(readODS)
source("R/1_functions.R")


# Adjustments -------------------------------------------------------------

# Batata, Borracha and Café appear multiple times
# Batata (-doce & -inglesa) is collapsed
adj_batata <- function(x) {
  batata <- x[, grep("Batata", names(x))]
  x <- x[, -grep("Batata", names(x))]
  x$Batata <- rowSums(batata, na.rm = TRUE) + 
    ifelse(Reduce(function(c1, c2) {c1 & c2}, lapply(batata, is.na)), NA, 0)
  x
}

# Borracha (látex coagulado & látex líquido) is collapsed
adj_borracha <- function(x) {
  borracha <- x[, grep("Borracha", names(x))]
  x <- x[, -grep("Borracha", names(x))]
  x$Borracha <- rowSums(borracha, na.rm = TRUE) + 
    ifelse(Reduce(function(c1, c2) {c1 & c2}, lapply(borracha, is.na)), NA, 0)
  x
}

# Of Café (Total, Arábica & Canephora) the total (in position 1) is kept
adj_cafe <- function(x) {
  cafe <- x[, grep("Café", names(x))]
  x <- x[, -grep("Café", names(x))]
  x$Café <- cafe[[1]]
  x
}


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
saveRDS("data/tab/crop_planted.rds")


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
saveRDS("data/tab/crop_harvested.rds")


# Crop quantities ---------------------------------------------------------

crop_ton_1 <- read_sidra("data/sidra/crop_quantities_2.ods", sheet = 1)
crop_ton_2 <- read_sidra("data/sidra/crop_quantities_3.ods", sheet = 1)
crop_ton_3 <- read_sidra("data/sidra/crop_quantities_4.ods", sheet = 1)
crop_ton_4 <- read_sidra("data/sidra/crop_quantities_5.ods", sheet = 1)

crop_ton <- rbind(crop_ton_1, crop_ton_2, crop_ton_3, crop_ton_4)
rm(crop_ton_1, crop_ton_2, crop_ton_3, crop_ton_4)

crop_ton <- crop_ton %>% 
  adj_batata() %>% adj_borracha() %>% adj_cafe()
crop_ton <- as_tibble(crop_ton[get_state(crop_ton$name), ])
saveRDS("data/tab/crop_ton.rds")


crop_value_1 <- read_sidra("data/sidra/crop_quantities_2.ods", sheet = 2)
crop_value_2 <- read_sidra("data/sidra/crop_quantities_3.ods", sheet = 2)
crop_value_3 <- read_sidra("data/sidra/crop_quantities_4.ods", sheet = 2)
crop_value_4 <- read_sidra("data/sidra/crop_quantities_5.ods", sheet = 2)

crop_value <- rbind(crop_value_1, crop_value_2, crop_value_3, crop_value_4)
rm(crop_value_1, crop_value_2, crop_value_3, crop_value_4)

crop_value <- crop_value %>% 
  adj_batata() %>% adj_borracha() %>% adj_cafe()
crop_value <- as_tibble(crop_value[get_state(crop_value$name), ])
saveRDS("data/tab/crop_value.rds")

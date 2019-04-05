
library(dplyr)
library(readODS)
source("R/1_functions.R")


# Forestry ----------------------------------------------------------------

forestry_quant <- read_sidra("data/sidra/forestry.ods", 
                             sheet = 1, grep_vars = "3")
forestry_value <- read_sidra("data/sidra/forestry.ods", 
                             sheet = 2, grep_vars = "3")


# Vegetables --------------------------------------------------------------

vegetables_quant <- read_sidra("data/sidra/vegetables.ods", 
                               sheet = 1, grep_vars = "2")
vegetables_value <- read_sidra("data/sidra/vegetables.ods", 
                               sheet = 2, grep_vars = "2")


library(dplyr)
source("R/2_functions.R")


# Read in tab data --------------------------------------------------------

# Socioeconomic
gdp <- readRDS("data/tab/gdp.rds")
pop <- readRDS("data/tab/pop.rds")
pop_details <- readRDS("data/tab/pop_details.rds")

socio <- full_join(full_join(
  gdp %>% transmute(code, name, date, gdp = value),
  pop %>% transmute(code, name, date, pop = value),
  by = c("code", "name", "date")),
  pop_details %>% transmute(code, name, date, pop_rur = rural, pop_urb = urban, 
                            pop_ind = indigenous),
  by = c("code", "name", "date")
)

# Livestock
animal_quant <- readRDS("data/tab/animal_quant.rds") %>% 
  transmute(code, name, date,
            milk_lt = `Leite` * 1000, eggs_pc = `Ovos de galinha` * 12 * 1000)
animal_value <- readRDS("data/tab/animal_value.rds") %>% 
  transmute(code, name, date, 
            milk_brl = `Leite` * 1000, eggs_brl = `Ovos de galinha` * 1000)
herd_sizes <- readRDS("data/tab/herd_sizes.rds") %>% 
  transmute(code, name, date, 
            cattle = `Bovino`, swine = `Suíno`, chicken = `Galináceos`)
milk_cows <- readRDS("data/tab/milk_cows.rds") %>% 
  transmute(code, name, date, milk_cow = value)

livestock <- full_join(full_join(full_join(
  animal_quant, animal_value, by = c("code", "name", "date")),
  herd_sizes, by = c("code", "name", "date")),
  milk_cows,  by = c("code", "name", "date")
)


# Crops -------------------------------------------------------------------

# Find proper crop subset via `summ_crop`, e.g.:
# summ_crop(crop_harvested, fun = mean, threshold = 200 * 13)

crop_harvested <- readRDS("data/tab/crop_harvested.rds") %>% 
  transmute(code, name, date,
            rice_hha = `Arroz`, sugar_hha = `Cana`, manioc_hha = `Mandioca`, 
            corn_hha = `Milho`, bean_hha = `Feijão`, sunfl_hha = `Girassol`,
            sorg_hha = `Sorgo`, cott_hha = `Algodão herbáceo`, soy_hha = `Soja`)
crop_planted <- readRDS("data/tab/crop_planted.rds") %>% 
  transmute(code, name, date,
            rice_pha = `Arroz`, sugar_pha = `Cana`, manioc_pha = `Mandioca`, 
            corn_pha = `Milho`, bean_pha = `Feijão`, sunfl_pha = `Girassol`,
            sorg_pha = `Sorgo`, cott_pha = `Algodão herbáceo`, soy_pha = `Soja`)
crop_quant <- readRDS("data/tab/crop_quant.rds") %>% 
  transmute(code, name, date,
            rice_ton = `Arroz`, sugar_ton = `Cana`, manioc_ton = `Mandioca`, 
            corn_ton = `Milho`, bean_ton = `Feijão`, sunfl_ton = `Girassol`,
            sorg_ton = `Sorgo`, cott_ton = `Algodão herbáceo`, soy_ton = `Soja`)
crop_value <- readRDS("data/tab/crop_value.rds") %>% 
  transmute(code, name, date,
            rice_brl = `Arroz` * 1000, sugar_brl = `Cana` * 1000, 
            manioc_brl = `Mandioca` * 1000, corn_brl = `Milho` * 1000, 
            bean_brl = `Feijão` * 1000, sunfl_brl = `Girassol` * 1000,
            sorg_brl = `Sorgo` * 1000, cott_brl = `Algodão herbáceo` * 1000, 
            soy_brl = `Soja`)

crop <- full_join(full_join(full_join(
  crop_harvested, crop_planted, by = c("code", "name", "date")),
  crop_quant, by = c("code", "name", "date")),
  crop_value, by = c("code", "name", "date")
)


# Forestry ----------------------------------------------------------------

# Explore data with `count_na`, e.g.:
# count_na(forestry_quant)
# count_na(forestry_quant, forestry_quant$name)

forestry_quant <- readRDS("data/tab/forestry_quant.rds") %>% 
  transmute(code, name, date, 
            firewood_m3 = `Lenha`, logs_m3 = `Madeira em tora`)
forestry_value <- readRDS("data/tab/forestry_value.rds") %>% 
  transmute(code, name, date, 
            firewood_brl = `Lenha` * 1000, logs_brl = `Madeira em tora` * 1000)
veggies_quant <- readRDS("data/tab/veggies_quant.rds") %>% 
  transmute(code, name, date, 
            food_ton = `Alimentícios`, oilseed_ton = `Oleaginosos`)
veggies_value <- readRDS("data/tab/veggies_value.rds") %>% 
  transmute(code, name, date, 
            food_brl = `Alimentícios` * 1000, oilseed_brl = `Oleaginosos` * 1000)

forestry <- full_join(full_join(full_join(
  forestry_quant, forestry_value, by = c("code", "name", "date")),
  veggies_quant, by = c("code", "name", "date")),
  veggies_value, by = c("code", "name", "date")
)

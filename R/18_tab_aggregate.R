
# Dependencies ------------------------------------------------------------

stopifnot(
  length(list.files("data/tab")) > 0,
  require(dplyr)
)


# Functions ---------------------------------------------------------------

summ_crop <- function(x, fun = median, threshold = 100) {
  x %>% filter(date > 2004) %>% 
    group_by(date) %>% 
    select(-code, -name) %>% 
    summarise_all(fun) %>% 
    select(-date) %>% 
    colSums() -> . 
  names(which(. >= threshold))
}

count_na <- function(x, per = x$date) {
  per_uniq <- unique(per)
  out <- matrix(NA, nrow = length(per_uniq), ncol = ncol(x) - 3,
                dimnames = list(per_uniq, names(x)[4:ncol(x)]))
  for(i in seq_along(per_uniq)) {
    out[i, ] <- sapply(x[per == per_uniq[i], 4:ncol(x)], function(x) {
      sum(is.na(x))
    })
  }
  out
}


# Read in data ------------------------------------------------------------

# Socioeconomic
gdp <- readRDS("data/tab/gdp.rds") %>% 
  transmute(code, name, date, gdp = value)
pop <- readRDS("data/tab/pop.rds") %>% 
  transmute(code, name, date, pop = value)
pop_details <- readRDS("data/tab/pop_details.rds") %>% 
  transmute(code, name, date, pop_rur = rural, pop_urb = urban, pop_ind = indigenous)

socio <- full_join(
  full_join(gdp, pop, by = c("code", "name", "date")), 
  pop_details, by = c("code", "name", "date")
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

livestock <- full_join(
  full_join(
    full_join(animal_quant, animal_value, by = c("code", "name", "date")),
    herd_sizes, by = c("code", "name", "date")),
  milk_cows,  by = c("code", "name", "date")
)


# Crops -------------------------------------------------------------------

# Use `summ_crop` to find a sensible subset of crops, e.g.:
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

crop <- full_join(
  full_join(
    full_join(crop_harvested, crop_planted, by = c("code", "name", "date")),
    crop_quant, by = c("code", "name", "date")),
  crop_value, by = c("code", "name", "date")
)


# Forestry ----------------------------------------------------------------

# Explore data with `count_na`, e.g.:
# count_na(forestry_quant)
# count_na(forestry_quant, forestry_quant$name)

forestry_quant <- readRDS("data/tab/forestry_quant.rds") %>% 
  transmute(code, name, date, 
            roundwood_m3 = `Lenha`, logs_m3 = `Madeira em tora`)
forestry_value <- readRDS("data/tab/forestry_value.rds") %>% 
  transmute(code, name, date, 
            roundwood_brl = `Lenha` * 1000, logs_brl = `Madeira em tora` * 1000)
veggies_quant <- readRDS("data/tab/veggies_quant.rds") %>% 
  transmute(code, name, date, 
            food_ton = `Alimentícios`, oilseed_ton = `Oleaginosos`)
veggies_value <- readRDS("data/tab/veggies_value.rds") %>% 
  transmute(code, name, date, 
            food_brl = `Alimentícios` * 1000, oilseed_brl = `Oleaginosos` * 1000)

forestry <- full_join(
  full_join(
    full_join(forestry_quant, forestry_value, by = c("code", "name", "date")),
    veggies_quant, by = c("code", "name", "date")),
  veggies_value, by = c("code", "name", "date")
)


# Aggregate all -----------------------------------------------------------

tab <- full_join(
  full_join(
    full_join(socio, livestock, by = c("code", "name", "date")),
    crop, by = c("code", "name", "date")),
  forestry, by = c("code", "name", "date")
)

saveRDS(tab, "data/tab/tab.rds")


detach("package:dplyr")

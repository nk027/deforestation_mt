
library(dplyr)
library(sf)

data <- shp <- readRDS("data/data.rds")
data$geometry <- NULL

check <- function(x) {
  sum(is.na(x) | x == 0)
}

data %>% 
  filter(date > 2004 & date < 2018) %>% 
  group_by(date) %>% 
  dplyr::select(soy_brl_hha, soy_brl, soy_ton, soy_hha, soy_pha) %>% 
  summarise_all(funs(check))
# From 2005 until 2017 we get a minimum/maximum of 22/46 NAs
# The variables are equivalent

municipios <- data %>% 
  filter(date > 2001 & date < 2018) %>% 
  group_by(code) %>% 
  select(soy_brl_hha) %>% 
  summarise_all(funs(check))

municipios %>%
  count(soy_brl_hha)
worst_muni <- municipios %>% 
  filter(soy_brl_hha > 12) %>% 
  .$code
bad_muni <- municipios %>% 
  filter(soy_brl_hha > 5 & soy_brl_hha < 13) %>% 
  .$code
ok_muni <- municipios %>% 
  filter(soy_brl_hha < 6 & soy_brl_hha > 0) %>% 
  .$code
# Only 13 municipios never produced soy


# Plots -------------------------------------------------------------------

shp %>% filter(date == 2010) %>% 
  mutate(bad = ifelse(code %in% worst_muni, TRUE, FALSE)) %>% 
  select(bad) %>% plot()
shp %>% filter(date == 2010) %>% 
  mutate(bad = ifelse(code %in% bad_muni, TRUE, FALSE)) %>% 
  select(bad) %>% plot()
shp %>% filter(date == 2010) %>% 
  mutate(bad = ifelse(code %in% ok_muni, TRUE, FALSE)) %>% 
  select(bad) %>% plot()

data %>% 
  filter(date > 2004 & date < 2018 & code %in% bad_muni) %>% 
  transmute(code, date, value = ifelse(is.nan(soy_brl_hha), TRUE, FALSE)) %>% 
  lattice::levelplot(data = ., value ~ code * date, 
                     col.regions = c("white", "red"))
# Many start with values, some end with values, few only inbetween

data %>% 
  filter(date > 2004 & date < 2018 & code %in% ok_muni) %>% 
  transmute(code, date, value = ifelse(is.nan(soy_brl_hha), TRUE, FALSE)) %>% 
  lattice::levelplot(data = ., value ~ code * date, 
                     col.regions = c("white", "red"))
# Messy
data %>%
  select(code, date, soy_brl_hha) %>% 
  reshape2::acast(formula = code ~ date)# %>% 
  # write.csv("soy_fill.csv")

data %>%
  filter(date > 2001 & date < 2018) %>% 
  select(code, date, soy_brl_hha) %>% 
  group_by(date) %>% 
  summarise(yearly = mean(soy_brl_hha, na.rm = TRUE)) %>% 
  .$yearly -> yearly


# Fill --------------------------------------------------------------------

# 1, easy fills

ok_check <- c(5100250, 5100359, 5101308, 5101704, 5102504, 5103106, 5103205, 
              5103858, 5104526, 5104542, 5104906, 5105507, 5105622, 5106190, 
              5106208, 5106752, 5106778, 5106851, 5107107, 5107263, 5107354, 
              5107743, 5107776, 5107859, 5107883, 5108352, 5108600)
all(c(ok_check %in% ok_muni, ok_muni %in% ok_check))

easy_fix <- data %>%
  filter(code %in% ok_muni, date > 2001 & date < 2018) %>% 
  select(code, date, soy_brl_hha) %>% 
  reshape2::acast(formula = date ~ code, value.var = "soy_brl_hha")

fill_avg <- function(x, yearly, tails = c("equivalence", "mean")) {
  
  tails = match.arg(tails)
  
  x <- x / yearly
  if(tails == "equivalence") x <- c(1, x, 1)
  if(tails == "mean") x <- c(mean(x, na.rm = TRUE), x, mean(x, na.rm = TRUE))
  
  nas <- which(is.na(x))
  non_nas <- which(!is.na(x))
  
  for(i in nas) {
    last_val <- non_nas[max(which(i - non_nas > 0))]
    next_val <- non_nas[min(which(i - non_nas < 0))]
    x[i] <- x[last_val] * 0.5 + 0.5 * x[next_val]
  }
  
  # Kick tails
  x <- x[c(-1, -length(x))]
  
  return(x * yearly)
}

easy_fixed <- apply(easy_fix, 2, fill_avg, yearly, "mean") %>% 
  reshape2::melt() %>% 
  transmute(code = Var2, date = Var1, soy_filled_1 = value)

# 2, bad fills

bad_check <- c(5101001, 5101209, 5101258, 5101407, 5102603, 5102793, 5103353, 
               5103361, 5105101, 5105150, 5105234, 5106299, 5106505, 5106653, 
               5106703, 5107404, 5107750, 5105309, 5106109, 5106273, 5106828,
               5103437)
all(c(bad_check %in% bad_muni, bad_muni %in% bad_check))

meh_fix <- data %>%
  filter(code %in% bad_muni, date > 2001 & date < 2018) %>% 
  select(code, date, soy_brl_hha) %>% 
  reshape2::acast(formula = date ~ code, value.var = "soy_brl_hha")

meh_fixed <- apply(meh_fix, 2, fill_avg, yearly, "mean") %>% 
  reshape2::melt() %>% 
  transmute(code = Var2, date = Var1, soy_filled_2 = value)

# Apply fills of 1 & 2
data <- merge(data, easy_fixed, by = c("date", "code"), all.x = TRUE)
data <- merge(data, meh_fixed, by = c("date", "code"), all.x = TRUE)

# 3, worst fills

# Integrate filled values from before
data <- data %>% 
  mutate(soy_filled = ifelse(is.na(soy_brl_hha), 
                             ifelse(is.na(soy_filled_1), 
                                    soy_filled_2, 
                                    soy_filled_1),
                             soy_brl_hha))
  
# Have a look at the worst fills, deep in the Amazon

amazon_na <- c(5103254, 5107578, 5103379, 5100805, 5106158, 5105176, 5102850,
               5108956)
amazon_exists <- c(510407, 5105150, 5105101, 5106299, 5100250)

x <- data %>% 
  filter(code %in% c(amazon_exists)) %>% 
  filter(date > 2001 & date < 2018) %>% 
  transmute(code = as.character(code), date, soy_filled)
filler <- x %>% 
  reshape2::acast(formula = date ~ code, value.var = "soy_filled")

library(ggplot2)
ggplot(x) +
  geom_line(aes(x = date, y = soy_filled, colour = code)) +
  geom_line(data = data.frame(x = 2002:2017, y = yearly), aes(x, y))

# Fill Amazon values with average of the four similar regions
fill <- tibble(date = rep(rownames(filler), length(amazon_na)), 
               soy_filled_3 = rep(rowMeans(filler), length(amazon_na)),
               code = rep(amazon_na, each = length(2002:2017)))

data <- merge(data, fill, by = c("date", "code"), all.x = TRUE)

# Work on filling
worst_check <- c(5100102, 5100805, 5101605, 5102850, 5103254, 5103379, 5103403, 
                 5103452, 5103809, 5103957, 5104500, 5105002, 5105176, 5106158, 
                 5106232, 5106315, 5107156, 5107206, 5107578, 5108402, 5108956,
                 5107297)
all(c(worst_check %in% worst_muni, worst_muni %in% worst_check))

worst_muni <- worst_muni[!worst_muni %in% amazon_na]

# Use the neighbours' values (including ones filled before)
source("R/5_functions.R")
W_qu <- get_W(shp, type = "queen")
codes <- rownames(W_qu)

fill <- matrix(NA, ncol = length(worst_muni), nrow = length(2002:2017))

# i <- 1

for(i in seq_along(worst_muni)) {
  neighbours <- codes[W_qu[codes == worst_muni[i], ] != 0]
  
  fill[, i] <- data %>% 
    filter(code %in% neighbours, date > 2001 & date < 2018) %>% 
    select(code, date, soy_filled) %>% 
    reshape2::acast(date ~ code) %>% 
    apply(1, function(x) sum(x, na.rm = TRUE) / (length(x) - sum(is.na(x))))
}

rownames(fill) <- 2002:2017
colnames(fill) <- worst_muni

fill <- reshape2::melt(fill)
colnames(fill) <- c("date", "code", "soy_filled_4")

data <- merge(data, fill, by = c("date", "code"), all.x = TRUE)

# Integrate all filled values
data <- data %>% 
  mutate(soy_filled = ifelse(is.na(soy_brl_hha), 
                             ifelse(is.na(soy_filled_1), 
                                    ifelse(is.na(soy_filled_2),
                                           ifelse(is.na(soy_filled_3),
                                                  soy_filled_4,
                                                  soy_filled_3),
                                           soy_filled_2), 
                                    soy_filled_1),
                             soy_brl_hha))

shp <- merge(shp, data[, c("code", "date", "soy_filled")], by = c("date", "code"), all.x = TRUE)

shp %>% select(code, date, soy_filled) %>% group_by(date) %>% summarise(mean(soy_filled))
data %>% select(code, date, soy_filled) %>% group_by(date) %>% summarise(mean(soy_filled))

# date <- 2002
# shp %>% 
#   filter(date == date) %>% 
#   select(soy_filled) %>% plot(); date <- date + 1

saveRDS(shp, "data/data_soyed.rds")


library(dplyr)
library(readr)


# CSV ---------------------------------------------------------------------

extr_df <- readRDS("data/geo/geo_iiasa.rds")

# Yields
yields <- read_csv("data/iiasa/yields.csv")
# SS - subsistence farm, LI - low input, HI - high input, IR - irrigated
# Relevant crops from IBGE:
# Rice, Sugarcane, Manioc, Corn, Beans, Sunflower, Sorghum, Cotton, Soy
grep("^BeaD..", names(yields))
grep("^Rice..", names(yields))
grep("^Srgh..", names(yields))
grep("^SugC..", names(yields))
grep("^Sunf..", names(yields))
grep("^Soya..", names(yields))


# Traveltime
time <- read_csv("data/iiasa/acc_travel_min.csv")


# Merge -------------------------------------------------------------------

data <- extr_df %>% 
  left_join(yields, by = c("simu" = "SIMUID")) %>% 
  left_join(time, by = c("simu" = "VALUE"))

# Some NAs for yields, fewer for travel time
data[is.na(data$MEANYLD), ]$weight
data[is.na(data$MEAN1), ]$weight
# Remove NAs
data <- data[!is.na(data$MEANYLD) & !is.na(data$MEAN1), ]
# # Recalculate weights - skipped, as NA should be 0
# reweigh <- data %>% group_by(code) %>%
#   summarise(reweigh = sum(weight))
# data_wt <- left_join(data, reweigh, by = "code") %>% 
#   mutate(weight = weight / reweigh)
# # Check reweighted values
# data_wt %>% group_by(code) %>% 
#   summarise(check = round(sum(weight), 1)) %>% .$check == 1
data_wt <- data


# Aggregate to municipios -------------------------------------------------

iiasa <- data_wt %>% 
  transmute(code, yield = weight * MEANYLD, travel = weight * MEAN1) %>% 
  group_by(code) %>% 
  summarise(iiasa_yield = sum(yield), iiasa_travel = sum(travel))

# Set year arbitrarily / wrongly to 2005
iiasa$date <- 2005L

saveRDS(iiasa, "data/geo/iiasa.rds")

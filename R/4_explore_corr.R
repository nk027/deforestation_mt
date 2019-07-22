
library(dplyr)

data <- readRDS("data/data.rds")


# Test -------------------------------------------------------------------

cor_data <- data %>% 
  ungroup() %>% 
  select(forest_ch, cerr_ch, nature_ch, forest_px, cerr_ch, nature_px,
         pasture_px, cattle, chicken, swine, 
         oilseed_ton, oilseed_brl,
         spei_dry, pop, gdp_cap, 
         ends_with("_hha"))

# Correlation test
cor_test <- function(x, cutoff = 0.8, na.0 = TRUE) {
  x$geometry <- NULL
  x <- as.matrix(x)
  if(na.0) {x[is.na(x)] <- 0}
  y <- cor(x)
  diag(y) <- 0
  which(y > cutoff | y < -cutoff, arr.ind = TRUE)
}

cor_test(cor_data, 0.8)
cor_test(cor_data, 0.9)

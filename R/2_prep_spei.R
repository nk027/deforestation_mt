
library(dplyr)

timescale <- "03"
crs_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"


# Wrangle SPEI ------------------------------------------------------------

df_spei <- readRDS(paste0("data/geo/geo_spei", timescale, ".rds"))

df_spei$date <- as.integer(gsub("^([0-9]{4})-..$", "\\1", df_spei$variable))
df_spei$month <- as.integer(gsub("^....-([0-9]{2})$", "\\1", df_spei$variable))
df_spei$variable <- NULL

df <- df_spei %>% 
  filter(date < 2018) %>% 
  group_by(date, code) %>% 
  summarise(spei_mean = mean(value), spei_sd = sd(value), 
            spei_qu2 = quantile(value, 0.2), 
            spei_qu5 = quantile(value, 0.5),
            spei_qu8 = quantile(value, 0.8)) %>% 
  mutate(spei_iqr = spei_qu8 - spei_qu2)

saveRDS(df, paste0("data/geo/spei", timescale, ".rds"))

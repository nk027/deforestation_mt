
timescale <- "03"


# Dependencies ------------------------------------------------------------

stopifnot(
  require("dplyr")
)
if(!exists("df_spei")) {
  df_spei <- readRDS(paste0("data/geo/geo_spei_", timescale, ".rds"))
}


# Wrangle SPEI ------------------------------------------------------------

df_spei$date <- as.integer(gsub("^([0-9]{4})-..$", "\\1", df_spei$variable))
df_spei$month <- as.integer(gsub("^....-([0-9]{2})$", "\\1", df_spei$variable))
df_spei$variable <- NULL

df <- df_spei %>% 
  filter(date < 2018) %>% 
  group_by(date, code) %>% 
  summarise(spei_dry = ifelse(min(value) < -2, 1, 0),
            spei_wet = ifelse(max(value) > 2, 1, 0))
  # summarise(spei_mean = mean(value), spei_sd = sd(value), 
  #           spei_qu2 = quantile(value, 0.2), 
  #           spei_qu5 = quantile(value, 0.5),
  #           spei_qu8 = quantile(value, 0.8)) %>% 
  # mutate(spei_iqr = spei_qu8 - spei_qu2)

# Store tibble with the transformed SPEI
saveRDS(df, paste0("data/geo/spei_", timescale, ".rds"))

detach("package:dplyr")

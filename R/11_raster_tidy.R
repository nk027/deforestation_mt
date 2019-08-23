
if(!exists("extr_vals")) {extr_vals <- readRDS("data/geo/geo_extract_raw.rds")}
library("dplyr")


# Transform raw extracted values ------------------------------------------

vals <- lapply(extr_vals, function(x) x[!is.na(x[[2]]), ])
# Change the format to integer / factor
vals <- lapply(vals, function(x) {
  tibble(id = as.integer(x[[1]]), 
         use = factor(x[[2]], levels = 1:13,
                      labels = c("cerrado", "fallow_cotton", "forest", 
                                 "pasture", "soy_corn", "soy_cotton", 
                                 "soy_fallow", "soy_millet", "soy_sunflower",
                                 "sugarcane", "urban", "water", "sec_veg")))
}) # Note: Câmara et al. (2019), i.e. version 3 adds "Secondary Vegetation"

df <- tibble(
  id = vals[[1]]$id,
  y01 = vals$y01$use, y02 = vals$y02$use, y03 = vals$y03$use,
  y04 = vals$y04$use, y05 = vals$y05$use, y06 = vals$y06$use,
  y07 = vals$y07$use, y08 = vals$y08$use, y09 = vals$y09$use,
  y10 = vals$y10$use, y11 = vals$y11$use, y12 = vals$y12$use,
  y13 = vals$y13$use, y14 = vals$y14$use, y15 = vals$y15$use,
  y16 = vals$y16$use, y17 = vals$y17$use
)

# Exclude gridcells in states other than Mato Grosso (1085 total)
occ <- sapply(unique(df[[1]]), function(x) sum(df[[1]] == x))
names(occ) <- unique(df[[1]])
df <- df[!df$id %in% as.integer(names(occ[occ < 1000])), ]

# # Store tibble of raw extracted values
# saveRDS(df, "data/geo/geo_extract.rds")


# Create tables from extracted values -------------------------------------

pull_vars <- function(x, formula, date = TRUE) {
  
  year <- as.character(formula[3])
  y <- reshape2::dcast(x, formula, value.var = year, fun.aggregate = length)
  
  if(is.null(y$sugarcane)) {y$sugarcane <- 0}
  if(is.null(y$sec_veg)) {y$sec_veg <- 0}
  
  z <- tibble(
    id = y$id,
    forest = y$forest,
    pasture = y$pasture,
    fallow_cotton = y$fallow_cotton, 
    soy_corn = y$soy_corn,
    soy_cotton = y$soy_cotton, 
    soy_fallow = y$soy_fallow,
    soy_millet = y$soy_millet,
    soy_sunflower = y$soy_sunflower,
    sugarcane = y$sugarcane,
    cerrado = y$cerrado,
    urban = y$urban,
    water = y$water,
    sec_veg = y$sec_veg
  )
  if(date) { # Determines wide or long format
    z$date <- as.integer(substr(year, 2, 3)) + 2000L
  } else {
    warning("Please don't.")
    names(z) <- c("id", paste(year, names(z)[-1], sep = "_"))
  }
  return(z)
}

# Long table
df01 <- pull_vars(df, id ~ y01, date = TRUE)
df02 <- pull_vars(df, id ~ y02, date = TRUE)
df03 <- pull_vars(df, id ~ y03, date = TRUE)
df04 <- pull_vars(df, id ~ y04, date = TRUE)
df05 <- pull_vars(df, id ~ y05, date = TRUE)
df06 <- pull_vars(df, id ~ y06, date = TRUE)
df07 <- pull_vars(df, id ~ y07, date = TRUE)
df08 <- pull_vars(df, id ~ y08, date = TRUE)
df09 <- pull_vars(df, id ~ y09, date = TRUE)
df10 <- pull_vars(df, id ~ y10, date = TRUE)
df11 <- pull_vars(df, id ~ y11, date = TRUE)
df12 <- pull_vars(df, id ~ y12, date = TRUE)
df13 <- pull_vars(df, id ~ y13, date = TRUE)
df14 <- pull_vars(df, id ~ y14, date = TRUE)
df15 <- pull_vars(df, id ~ y15, date = TRUE)
df16 <- pull_vars(df, id ~ y16, date = TRUE)
df17 <- pull_vars(df, id ~ y17, date = TRUE)

df_date <- as_tibble(rbind(
  df01, df02, df03, df04, df05, df06, df07, df08, df09,
  df10, df11, df12, df13, df14, df15, df16, df17
))

# Store tidy tibble of the raster values
saveRDS(df_date, "data/geo/geo_df_long.rds")


detach("package:dplyr")

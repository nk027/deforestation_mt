
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

# Store tibble of raw extracted values
saveRDS(df, "data/geo/geo_extract.rds")

detach("package:dplyr")

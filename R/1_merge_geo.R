library(dplyr)


# merge shape & raster data ----------------------------------------------

crs_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

tifs <- list.files("data/landsat", "[.]tif$")

# sp for raster::extract to work
shp <- rgdal::readOGR(dsn = "data/municipios")
shp <- sp::spTransform(shp, crs_sin)

# merge shp & r, takes very long
extr_vals <- vector("list", length(tifs))
for(i in seq_along(tifs)) {
  r <- raster::raster(paste0("data/landsat/", tifs[i]))
  extr_vals[[i]] <- raster::extract(r, shp, df = TRUE)
}
names(extr_vals) <- paste0("y", formatC(1:17, width = 2, flag = "0"))
saveRDS(extr_vals, "data/geo/geo_merged_raw.rds")

# kick out NAs and change to integer and factor
vals <- lapply(extr_vals, function(x) x[!is.na(x[[2]]), ])
vals <- lapply(vals, function(x) {
  tibble(
    id = as.integer(x[[1]]), 
    use = factor(x[[2]], 
                 levels = 1:12,
                 labels = c("cerrado", "fallow_cotton", "forest", 
                            "pasture", "soy_corn", "soy_cotton", 
                            "soy_fallow", "soy_millet", "soy_sunflower",
                            "sugarcane", "urban", "water"))
  )
})

df_vals <- tibble(
  id = vals[[1]]$id,
  y01 = vals$y01$use, y02 = vals$y02$use, y03 = vals$y03$use,
  y04 = vals$y04$use, y05 = vals$y05$use, y06 = vals$y06$use,
  y07 = vals$y07$use, y08 = vals$y08$use, y09 = vals$y09$use,
  y10 = vals$y10$use, y11 = vals$y11$use, y12 = vals$y12$use,
  y13 = vals$y13$use, y14 = vals$y14$use, y15 = vals$y15$use,
  y16 = vals$y16$use, y17 = vals$y17$use
)

hist(df_vals[[1]], breaks = 161)

occ <- sapply(unique(df_vals[[1]]), function(x) sum(df_vals[[1]] == x))
names(occ) <- unique(df_vals[[1]])
# these have a few tiles in non Mato Grosso municipalities (1125 total)
id_filter <- as.integer(names(occ[occ < 1000]))

df <- df_vals[!df_vals$id %in% id_filter, ]
# store the results
saveRDS(df, "data/geo/geo_merged_df.rds")


# transform to more usable format -----------------------------------------

pull_vars <- function(x, form, date = FALSE) {
  
  year <- as.character(form[3])
  y <- reshape2::dcast(x, form, value.var = year, fun.aggregate = length)
  
  if(is.null(y$sugarcane)) y$sugarcane <- 0
  
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
    crop = y$fallow_cotton + y$soy_corn + y$soy_cotton + 
      y$soy_fallow + y$soy_millet + y$soy_sunflower + y$sugarcane,
    other = y$cerrado + y$urban + y$water
  )
  if(date)
    z$date <- as.integer(substr(year, 2, 3)) + 2000L
  else
    names(z) <- c("id", paste(year, names(z)[-1], sep = "_"))
  
  z
}

df01 <- pull_vars(df, id ~ y01)
df02 <- pull_vars(df, id ~ y02)
df03 <- pull_vars(df, id ~ y03)
df04 <- pull_vars(df, id ~ y04)
df05 <- pull_vars(df, id ~ y05)
df06 <- pull_vars(df, id ~ y06)
df07 <- pull_vars(df, id ~ y07)
df08 <- pull_vars(df, id ~ y08)
df09 <- pull_vars(df, id ~ y09)
df10 <- pull_vars(df, id ~ y10)
df11 <- pull_vars(df, id ~ y11)
df12 <- pull_vars(df, id ~ y12)
df13 <- pull_vars(df, id ~ y13)
df14 <- pull_vars(df, id ~ y14)
df15 <- pull_vars(df, id ~ y15)
df16 <- pull_vars(df, id ~ y16)
df17 <- pull_vars(df, id ~ y17)

df_wide <- as_tibble(cbind(
  df01, df02[-1], df03[-1], df04[-1], df05[-1], df06[-1], df07[-1], df08[-1],
  df09[-1], df10[-1], df11[-1], df12[-1], df13[-1], df14[-1], df15[-1], df16[-1],
  df17[-1]
))
saveRDS(df_wide, "data/geo/geo_merged_df_wide.rds")

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
saveRDS(df_date, "data/geo/geo_merged_df_date.rds")


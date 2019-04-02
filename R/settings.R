labels <- c("cerrado", "fallow_cotton", "forest", 
            "pasture", "soy_corn", "soy_cotton", 
            "soy_fallow", "soy_millet", "soy_sunflower",
            "sugarcane", "urban", "water", "sec_veg")

colour <- c("#b3cc33", "#be94e8", "#10773e", 
            "#eeefce", "#e4a540", "#a4507d",
            "#c948a2", "#be5b1d", "#f09cde",
            "#877712", "#614040", "#1b5ee4", "#0cf8c1")
names(colour) <- labels

crs_sin <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
crs_sirgas <- "+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

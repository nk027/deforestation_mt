
long_cat <- function(x, variable) {
  x <- x[c(1, grep(variable, names(x)))]
  x <- reshape2::melt(x, id.vars = "code", stringsAsFactors = FALSE)
  x$variable <- as.character(x$variable)
  x$date <- 2000L + as.integer(substr(x$variable, nchar(x$variable) - 1, nchar(x$variable)))
  names(x)[names(x) == "value"] <- substr(x$variable, 1, nchar(x$variable) - 2)[1]
  x$variable <- NULL
  x
}


get_state <- function(x, pattern = "[(]MT[)]", v = FALSE) {
  out <- grep(pattern, x)
  if(v) suppressWarnings(cat("Matches in order:", 
                             all(out == out[1]:out[length(out)]), "\n"))
  out
}


na_locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v) + 1]
}


read_sidra <- function(
  file, sheet = 1, 
  grep_vars = c("^\\b(?!Total).*$" = "1", 
                "^[0-9]+ - .*$" = "2", 
                "^[0-9]+[.][0-9]+ - .*$" = "3")) {
  
  grep_vars <- names(match.arg(grep_vars))

  x <- readODS::read_ods(file, sheet, col_names = FALSE, col_types = NA)
  
  table <- x[[1]][1]
  variable <- x[[1]][2]
  
  year_cols <- which(!is.na(x[4, ]) & x[4, ] != "")
  years <- as.integer(x[4, year_cols])
  
  if(is.na(x[[1]][5])) {
    vars <- as.character(x[5, year_cols[1]:(year_cols[2] - 1)])
    
    # total <- grep("^Total", vars)
    var_pos <- grep(grep_vars, vars, perl = TRUE)
    var_names <- gsub("^[^A-Z]*([A-z ,À-ú]*[A-zÀ-ú]).*", "\\1", vars[var_pos])
  } else {
    var_pos <- 1
    var_names <- "value"
  }
  
  code <- suppressWarnings(as.integer(x[[1]]))
  keep <- which(!is.na(code))
  
  code <- code[keep]
  name <- x[[2]][keep]
  
  y <- x[keep, 3:ncol(x)]
  y[y == "-"] <- 0
  y[y == ".."] <- 0
  y <- suppressWarnings(vapply(y, as.numeric, numeric(nrow(y))))
  
  z <- matrix(NA, nrow = length(keep) * length(years), ncol = length(var_pos))
  for(i in seq_along(years)) {
    z[(1 + (i - 1) * length(keep)):(i * length(keep)), 1:ncol(z)] <-
      y[, year_cols[i] + var_pos - 3]
    
  }
  
  out <- data.frame(code = rep(code, length(years)),
                    name = rep(name, length(years)),
                    date = rep(years, each = length(keep)),
                    as.data.frame(z),
                    stringsAsFactors = FALSE)
  names(out)[4:ncol(out)] <- var_names
  attr(out, "table") <- table
  attr(out, "variable") <- variable
  
  out
}


adj_collapse <- function(x, grep_var) {
  y <- x[, grep(grep_var, names(x))]
  x <- x[, -grep(grep_var, names(x))]
  x[[grep_var]] <- rowSums(y, na.rm = TRUE) + 
    ifelse(Reduce(function(c1, c2) {c1 & c2}, lapply(y, is.na)), NA, 0)
  x
}


adj_first <- function(x, grep_var) {
  y <- x[, grep(grep_var, names(x))]
  x <- x[, -grep(grep_var, names(x))]
  x[[grep_var]] <- y[[1]]
  x
}


pull_vars <- function(x, formula, date = TRUE) {
  
  year <- as.character(formula[3])
  y <- reshape2::dcast(x, formula, value.var = year, fun.aggregate = length)
  
  if(is.null(y$sugarcane)) y$sugarcane <- 0
  if(is.null(y$sec_veg)) y$sec_veg <- 0
  
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
  if(date) {
    z$date <- as.integer(substr(year, 2, 3)) + 2000L
  } else {
    names(z) <- c("id", paste(year, names(z)[-1], sep = "_"))
  }
  z
}

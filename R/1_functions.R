
long_cat <- function(x, variable) {
  x <- x[c(1, grep(variable, names(x)))]
  x <- reshape2::melt(x, id.vars = "code", stringsAsFactors = FALSE)
  x$variable <- as.character(x$variable)
  x$date <- 2000L + as.integer(substr(x$variable, nchar(x$variable) - 1, nchar(x$variable)))
  names(x)[names(x) == "value"] <- substr(x$variable, 1, nchar(x$variable) - 2)[1]
  x$variable <- NULL
  x
}

get_state <- function(x, pattern = "[(]MT[)]") {
  out <- grep(pattern, x)
  suppressWarnings(cat("Matches in order:", 
                       all(out == out[1]:out[length(out)]), "\n"))
  out
}

na_locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v) + 1]
}

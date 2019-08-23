
summ_crop <- function(x, fun = median, threshold = 100) {
  x %>% filter(date > 2004) %>% 
    group_by(date) %>% 
    select(-code, -name) %>% 
    summarise_all(fun) %>% 
    select(-date) %>% 
    colSums() -> . 
  names(which(. >= threshold))
}

count_na <- function(x, per = x$date) {
  per_uniq <- unique(per)
  out <- matrix(NA, nrow = length(per_uniq), ncol = ncol(x) - 3,
                dimnames = list(per_uniq, names(x)[4:ncol(x)]))
  for(i in seq_along(per_uniq)) {
    out[i, ] <- sapply(x[per == per_uniq[i], 4:ncol(x)], function(x) {
      sum(is.na(x))
    })
  }
  out
}


library(dplyr)

df_spei <- readRDS("data/geo/geo_spei.rds")

df_spei$year <- as.integer(gsub("^([0-9]{4})-..$", "\\1", df_spei$variable))
df_spei$month <- as.integer(gsub("^....-([0-9]{2})$", "\\1", df_spei$variable))
spei_plot <- function(x, ...) {
  
  plot(density(x), ...)
  lines(density(rnorm(100000, mean = mean(x), sd = sd(x))), col = "darkgray")
  
}

for(month in 1:12) {
  x <- rbind(df_spei[[1 + month]], df_spei[[13 + month]], df_spei[[25 + month]], 
             df_spei[[37 + month]], df_spei[[49 + month]], df_spei[[61 + month]], 
             df_spei[[73 + month]], df_spei[[85 + month]], df_spei[[97 + month]],
             df_spei[[109 + month]], df_spei[[121 + month]], df_spei[[133 + month]],
             df_spei[[145 + month]], df_spei[[157 + month]], df_spei[[169 + month]], 
             df_spei[[181 + month]], df_spei[[193 + month]], df_spei[[205 + month]])
  cat(shapiro.test(x)[[2]] < 0.01, "\n")
  spei_plot(x, main = month)
  Sys.sleep(1)
}

spei_plot(df_spei$value[df_spei$month == i], main = i); i <- i + 1

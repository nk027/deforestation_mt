
library(dplyr)
library(sf)


# Wrap up all data---------------------------------------------------------

shp <- readRDS("data/geo/shp.rds")
tab <- readRDS("data/tab/tab.rds")
spei <- readRDS("data/geo/spei.rds")
iiasa <- readRDS("data/geo/iiasa.rds")

data <- full_join(shp, tab, by = c("code", "date")) %>% 
  full_join(spei, by = c("code", "date")) %>% 
  full_join(iiasa, by = c("code", "date")) %>% 
  filter(date < 2018, date > 2000)

refcols <- c("code", "name", "date")
data <- data[, c(refcols, setdiff(names(data), refcols))]

saveRDS(data, "data/data.rds")


# Generate some summaries -------------------------------------------------

vars <- read.table("txt/variables.txt", stringsAsFactors = FALSE)

summaries <- t(apply(vars, 1, function(y) c(summary(data[[y]]))))
write.table(summaries, "txt/summaries.txt", quote = FALSE, row.names = FALSE)

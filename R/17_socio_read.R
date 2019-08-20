
# Dependencies ------------------------------------------------------------

stopifnot(
  length(list.files("data/sidra", "pop")) > 0,
  length(list.files("data/sidra", "gdp")) > 0,
  require(dplyr),
  nzchar(system.file(package = "readODS"))
)
source("R/16_functions_read.R")


# Read in GDP -------------------------------------------------------------

gdp <- read_sidra("data/sidra/gdp.ods")

gdp <- as_tibble(gdp[get_state(gdp$name), ]) # Subset to Mato Grosso

saveRDS(gdp, "data/tab/gdp.rds")


# Read in and merge population --------------------------------------------

# Estimates
pop <- read_sidra("data/sidra/pop.ods")

pop <- as_tibble(pop[get_state(pop$name), ]) # Subset to Mato Grosso

# Census for 2007 and 2010
pop07 <- read_sidra("data/sidra/pop2007.ods")
pop07 <- as_tibble(pop07[get_state(pop07$name), ]) # Subset to Mato Grosso

# 2010 contains info on urban/rural and indigenous population as well
pop10 <- read_ods("data/sidra/pop2010.ods", skip = 6, 
                  col_names = FALSE, col_types = NULL)
names(pop10) <- c("code", "name", "race", "value", "urban", "rural")

pop10 <- pop10[-nrow(pop10), ] # Contains source
pop10$date <- 2010
pop10$code <- na_locf(pop10$code) # Carry over name & code
pop10$name <- na_locf(pop10$name)

pop10 <- pop10[get_state(pop10$name), ] # Subset to Mato Grosso

# Fix 0 values
pop10 <- as_tibble(lapply(pop10, function(x) {
  x[x == ".."] <- 0
  x[x == "-"] <- 0
  x
}))
# Fix column classes
for(int in c(1, 4:6)) class(pop10[[int]]) <- "numeric"

# Isolate rows with indigenous population and add them as a column
pop10_ind <- pop10[get_state(pop10$race, pattern = "Indígena"), ]
pop10 <- pop10[get_state(pop10$race, pattern = "Total"), ]
pop10$indigenous <- pop10_ind$pop

# Bind population data for the full timeseries
pop <- rbind(pop, 
             pop07[c("code", "name", "date", "value")],
             pop10[c("code", "name", "date", "value")])

saveRDS(pop, "data/tab/pop.rds")

# Store extra information separately
saveRDS(pop10[c("code", "name", "date", "urban", "rural", "indigenous")], 
        "data/tab/pop_details.rds")


detach("package:dplyr")

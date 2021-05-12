
library("raster")
library("dplyr")

files <- list.files("data/landsat")

for(f in files) {
  assign(paste0("val_", gsub("mt_20([0-9]+).*", "\\1", f)),
    values(raster(paste0("data/landsat/", f))))
}

x <- tibble(val_01, val_02, val_03, val_04, val_05, val_06, val_07, val_08,
  val_09, val_10, val_11, val_12, val_13, val_14, val_15, val_16, val_17)

labels <- c("cerrado", "fallow_cotton", "forest",
  "pasture", "soy_corn", "soy_cotton",
  "soy_fallow", "soy_millet", "soy_sunflower",
  "sugarcane", "urban", "water", "sec_veg")

for(j in seq(ncol(x))) {
  x[[j]] <- ifelse(x[[j]] %in% c(2, 5, 6, 7, 8, 9, 10), 2, x[[j]])
  x[[j]] <- ifelse(!x[[j]] %in% c(1, 2, 3, 4), 5, x[[j]])
}

library("plotly")

flow1 <- x %>% group_by(start = val_01, end = val_06) %>% summarise(count = n())
flow1$end <- flow1$end + 5
flow2 <- x %>% group_by(start = val_06, end = val_09) %>% summarise(count = n())
flow2$start <- flow2$start + 5
flow2$end <- flow2$end + 5 * 2
flow3 <- x %>% group_by(start = val_09, end = val_12) %>% summarise(count = n())
flow3$start <- flow3$start + 5 * 2
flow3$end <- flow3$end + 5 * 3
flow4 <- x %>% group_by(start = val_12, end = val_17) %>% summarise(count = n())
flow4$start <- flow4$start + 5 * 3
flow4$end <- flow4$end + 5 * 4
flow <- rbind(flow1, flow2, flow3, flow4)
flow <- flow %>% filter(start != end - 5)

colour <- rep(c("#C48410", "#C18FE3", "#10773e", "#E8D313", "#eeeeee"), 5)
labels <- c("Cerrado", "Croplands", "Forest", "Pasture", "Other")

labels <- paste0(labels, " '",
  rep(c("01", "06", "09", "12", "17"), each = length(labels)))

p <- plotly::plot_ly(
  type = "sankey",
  orientation = "h",

  node = list(
    label = labels,
    color = colour,
    pad = 5,
    thickness = 50,
    line = list(color = "black", width = 0.5)
  ),

  link = list( # 0 Indexed
    source = flow[[1]] - 1,
    target = flow[[2]] - 1,
    value = flow[[3]],
    color = paste0(colour, "44")[flow[[1]]]
  )
) %>%
  plotly::layout(
    title = "",
    font = list(
      size = 16
    )
  )
p
plotly_IMAGE(p, format = "pdf", out_file = "output.pdf")


library(dplyr)
library(sf)
library(ggplot2)
library(cowplot)
library(reshape2)

data <- readRDS("data/data.rds")


# Dependents --------------------------------------------------------------

shp <- data %>% filter(date > 2004, date < 2017)
summary(shp)
hist(shp$forest_ch, breaks = 50)
hist(shp$forest_ch[shp$forest_ch < 0], breaks = 50)
hist(shp$cerr_ch, breaks = 50)
hist(shp$nature_ch, breaks = 50)


# Crop -------------------------------------------------------------------

crops <- data %>% 
  filter(date > 2004) %>% 
  select(date, code, 31:66)

crop_names <- gsub("^(.*)(_hha)", "\\1", 
                   names(crops))[grepl("^.*_hha", names(crops))]

# Compare pha and hha
for(name in crop_names) {
  ylim <- quantile(crops[[paste0(name, "_hha")]], c(0.05, 0.95)) / 1000
  png(paste0("./plots/box_crop_ha/", name, ".png"), 
      width = 12, height = 5, units = "in", res = 300)
  print(
  crops %>% select(date, grep(paste0("^", name, "_.ha$"), names(crops))) %>% 
    mutate(date = as.character(date)) %>% 
    as_tibble() %>% select(-geometry) %>% 
    melt(id.var = "date") %>% 
    mutate(variable = factor(variable, labels = c("hha", "pha"))) %>% 
    filter(value > 0) %>% 
    ggplot(aes(colour = variable)) +
    facet_grid(. ~ date) +
    geom_boxplot(aes(group = variable, y = value / 1000), outlier.shape = NA) +
    scale_y_continuous(name = NULL, labels = function(x) paste(x, "k", sep = "")) +
    scale_x_continuous(name = NULL, labels = NULL) +
    coord_cartesian(ylim = ylim) +
    ggthemes::scale_color_stata(name = name) +
    ggthemes::theme_stata(scheme = "s1mono") + theme(legend.position = "right")
  )
  graphics.off()
}

# Compare ton and brl
for(name in crop_names) {
  ylim <- quantile(crops[[paste0(name, "_ton")]], c(0.05, 0.95)) / 1000
  png(paste0("./plots/box_crop_quant/", name, "_ton.png"), 
      width = 12, height = 5, units = "in", res = 300)
  print(
    crops %>% select(date, grep(paste0("^", name, "_ton$"), names(crops))) %>% 
      mutate(date = as.character(date)) %>% 
      as_tibble() %>% select(-geometry) %>% 
      melt(id.var = "date") %>% 
      mutate(variable = factor(variable, labels = c("ton"))) %>%
      filter(value > 0) %>% 
      ggplot(aes(colour = variable)) +
      facet_grid(. ~ date) +
      geom_boxplot(aes(group = variable, y = value / 1000), outlier.shape = NA) +
      scale_y_continuous(name = NULL, labels = function(x) paste(x, "k", sep = "")) +
      scale_x_continuous(name = NULL, labels = NULL) +
      coord_cartesian() +
      ggthemes::scale_color_stata(name = name) +
      ggthemes::theme_stata(scheme = "s1mono") + theme(legend.position = "right")
  )
  graphics.off()
}
for(name in crop_name) {
  ylim <- quantile(crops[[paste0(name, "_brl")]], c(0.05, 0.95)) / 1000
  png(paste0("./plots/box_crop_quant/", name, "_brl.png"), 
      width = 12, height = 5, units = "in", res = 300)
  print(
    crops %>% select(date, grep(paste0("^", name, "_brl$"), names(crops))) %>% 
      mutate(date = as.character(date)) %>% 
      as_tibble() %>% select(-geometry) %>% 
      melt(id.var = "date") %>% 
      mutate(variable = factor(variable, labels = c("brl"))) %>%
      filter(value > 0) %>% 
      ggplot(aes(colour = variable)) +
      facet_grid(. ~ date) +
      geom_boxplot(aes(group = variable, y = value / 1000), outlier.shape = NA) +
      scale_y_continuous(name = NULL, labels = function(x) paste(x, "k", sep = "")) +
      scale_x_continuous(name = NULL, labels = NULL) +
      coord_cartesian() +
      ggthemes::scale_color_stata(name = name) +
      ggthemes::theme_stata(scheme = "s1mono") + theme(legend.position = "right")
  )
  graphics.off()
}


for(name in crop_names) {
  png(paste0("./plots/scatter_crop_quant/", name, ".png"), 
      width = 12, height = 5, units = "in", res = 300)
  print(
    crops %>% select(date, grep(paste0("^", name, "_(ton|brl)$"), names(crops))) %>% 
      mutate(date = as.character(date)) %>% 
      as_tibble() %>% select(-geometry) %>% 
      rename(ton = 2, brl = 3) %>% 
      filter(ton > 0, brl > 0) %>% 
      ggplot(aes(x = ton / 1000000, y = brl / 1000000)) +
      geom_smooth(method = "lm", colour = "lightgray", alpha = 0.2) +
      geom_point() +
      facet_wrap(date ~ .) +
      scale_y_continuous(name = "Tons", labels = function(x) paste(x, "m", sep = "")) +
      scale_x_continuous(name = "BRL", labels = function(x) paste(x, "m", sep = "")) +
      ggtitle(name) +
      ggthemes::scale_color_stata(name = name) +
      ggthemes::theme_stata(scheme = "s1mono") + theme(legend.position = "right")
  )
  graphics.off()
}


# for(name in crop_names) {
#   png(paste0("./plots/scatter_crop_ha/", name, ".png"), 
#       width = 12, height = 5, units = "in", res = 300)
#   print(
#     crops %>% select(date, grep(paste0("^", name, "_.ha$"), names(crops))) %>% 
#       mutate(date = as.character(date)) %>% 
#       as_tibble() %>% select(-geometry) %>% 
#       rename(ton = 2, brl = 3) %>% 
#       filter(ton > 0, brl > 0) %>% 
#       ggplot(aes(x = ton / 1000000, y = brl / 1000000)) +
#       geom_smooth(method = "lm", colour = "lightgray", alpha = 0.2) +
#       geom_point() +
#       facet_wrap(date ~ .) +
#       scale_y_continuous(name = "Tons", labels = function(x) paste(x, "m", sep = "")) +
#       scale_x_continuous(name = "BRL", labels = function(x) paste(x, "m", sep = "")) +
#       ggtitle(name) +
#       ggthemes::scale_color_stata(name = name) +
#       ggthemes::theme_stata(scheme = "s1mono") + theme(legend.position = "right")
#   )
#   graphics.off()
# }

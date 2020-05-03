
# Dependencies ------------------------------------------------------------

stopifnot(
  require("dplyr"),
  require("sf"),
  require("ggplot2")
)

x <- read_sf("data/municipios/")


# Plots -------------------------------------------------------------------

df <- x %>% 
  transmute(id = as.integer(CD_GEOCMU)) %>% 
  filter(id > 5100000 & id < 5200000) # Correspond to Mato Grosso

ggplot(df) +
  geom_sf() +
  theme(
    text = element_text(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    plot.margin = unit(c(0.5, 1, 0, 0), "lines"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text.x = element_text(hjust = 0)) +
  theme_bw(base_size = 14, base_family = "Arial")

ggsave("plots/municipios.tiff", width = 20, height = 20, units = "cm")


detach("package:sf")
detach("package:dplyr")
detach("package:ggplot2")

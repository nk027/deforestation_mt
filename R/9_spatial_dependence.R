
library(ggplot2)

x <- 1:10
y <- 1:10

data <- expand.grid(x = x, y = y)

data$random <- rnorm(100)
data$spatial <- data$x * data$y
data$spatial <- (data$spatial - mean(data$spatial)) / sd(data$spatial)

ggplot(data, aes(x, y)) +
  geom_tile(aes(fill = spatial)) +
  theme_void() +
  scale_fill_viridis_c() +
  theme(legend.position = "none")

ggsave("spatial_autocorrelation_1.png", width = 8, height = 8, units = "cm")

ggplot(data, aes(x, y)) +
  geom_tile(aes(fill = random)) +
  theme_void() +
  scale_fill_viridis_c() +
  theme(legend.position = "none")

ggsave("spatial_autocorrelation_2.png", width = 8, height = 8, units = "cm")

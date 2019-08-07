
library(ggplot2)
library(ggthemes)

x <- readxl::read_xlsx("~/Dokumente/msc/BRA.xlsx", sheet = 2)
df <- data.frame(
  "date" = as.factor(2001:2018),
  "loss_kha" = t(x[5, 7:ncol(x)]) / 1000
)

barplot(df$tr_cov_loss_kha)

ggplot(df, aes(x = date, y = loss_kha)) +
  geom_bar(stat = "identity", fill = "darkgray", width = 0.8) +
  ylab("Loss in 1,000 ha") +
  xlab(NULL) +
  # xlab("Year") +
  scale_y_continuous(expand = c(0, 0), 
                     labels = c("2,000", "4,000", "6,000"),
                     breaks = seq(2000, 6000, 2000)) +
  scale_x_discrete(breaks = 2001:2018,
                   labels = paste0("'", formatC(1:18, width = 2, flag = "0"))) +
  coord_cartesian(ylim = c(0, 7000)) +
  theme_bw(base_size = 14, base_family = "Arial")
ggsave("plots/bars_tree_cover_loss.png", height = 6, width = 16, units = "cm")

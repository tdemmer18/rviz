library(tidyverse)
library(readr)
library(ggplot2)

nhl_stats <- read_csv("~/rstats/rviz/hockey/all-stars/all_star_break_stats.csv")
nhl_stats

all_stars_df <- read_csv("~/rstats/rviz/hockey/all-stars/df_all_stars.csv")
all_stars_df

Division_color <- c(Atlantic = "#5F968E", Metropolitan = "#E05858", Central = "#FFB745", Pacific = "#5CC5EF")

data <- all_stars_df %>%
  left_join(nhl_stats, by = "Player")

data %>%
  ggplot(aes(reorder(Player, PTS), PTS, fill = DIVISION)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_text(aes(label = PTS), nudge_y = -5, color = "white") +
  coord_flip() +
  scale_fill_manual(values = Division_color) +
  facet_wrap(~ DIVISION, scales = "free_y") +
  theme(rect = element_rect(fill = "#212F3C"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(colour = rgb(230, 230, 230, maxColorValue = 255), size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, colour = "white"),
        plot.subtitle = element_text(hjust = 0.5, colour = "white"),
        plot.caption = element_text(colour = "white")) +
  labs(
    title = "NHL All-Star Point Totals by Division",
    subtitle = "(Point totals through 2019-01-26)",
    x = "",
    y = "",
    caption = "@tdemmer18 | 2019-01-26"
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))


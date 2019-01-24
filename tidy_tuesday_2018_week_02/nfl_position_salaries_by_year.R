remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(readxl)
library(tidyverse)
library(gganimate)
library(gghighlight)
library(lubridate)
library(ggrepel)
library(directlabels)

# read in csv file
df <- read_excel("rstats/rviz/tidy_tuesday_2018_week_02/nfl_salary.xlsx")

# gather data and select top 15 salaries for each year and position, then average
df_gathered  <- df %>%
  na.omit() %>%
  gather(position, amount, -year) %>%
  group_by(year, position) %>%
  top_n(n = 15) %>%
  summarise(avg_salary = mean(amount)) %>%
  arrange(position)

# Create new df (abbrev_key) to join to our data so we can use abbreviations and facet_wrap by off or def:
df_positions <- as.data.frame(unique(df_gathered$position))
position_abbrev <- data.frame(abbrev = c("CB", "DL", "LB", "OL", "QB", "RB", "S", "ST", "TE", "WR"))
off_def <- data.frame(side = c("DEF", "DEF", "DEF", "OFF", "OFF", "OFF", "DEF", "DEF", "OFF", "OFF"))
abbrev_key <- bind_cols(position = df_positions, Pos_Abb = position_abbrev, side = off_def)
names(abbrev_key) <- c("position", "pos_abb", "side_of_ball")

# preview our new df that we will use to join below
abbrev_key

# join our abbrev_key to our data using a left_join
df_gathered <- df_gathered %>%
  left_join(abbrev_key, by = "position")

#preview our df
df_gathered

ggplot(df_gathered,
         aes(x = year, y = avg_salary, color = position, group = position)) +
  geom_smooth(se = FALSE, method = "loess", formula = "y ~ x") +
  geom_text_repel(data = subset(df_gathered, year == "2018"), aes(label = pos_abb), hjust = -.1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(limits = c(2011, 2018), breaks = seq(2011, 2018, 1)) +
  labs(x = "", y = "",
       title = "Average NFL salaries by position",
       subtitle = "Top 15 highest salaries for each position",
       caption = "TidyTuesday: 2018 Week 02
                  \n @tdemmer18 | 2019-01-23") +
  theme(legend.position = "none",
        rect = element_rect(fill = "#212F3C"),#0a1926
        panel.grid = element_blank(),
        text = element_text(colour = "white"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(colour = rgb(230, 230, 230, maxColorValue = 255), size = 11)) +
  gghighlight(position %in% c("Quarterback", "Running Back"), use_group_by = FALSE)


# using the geom_dl to label the last points of our graph
ggplot(df_gathered,
       aes(x = year, y = avg_salary, color = position)) +
  geom_smooth(se = FALSE, method = "loess", formula = "y ~ x") +
  geom_dl(aes(label = pos_abb), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.7)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(limits = c(2011, 2018), breaks = seq(2011, 2018, 1),
                     expand = c(0,1)) +
  labs(x = "", y = "",
       title = "Average NFL salaries by position",
       subtitle = "(Data filtered for top 15 highest salaries for each position)",
       caption = "TidyTuesday: 2018 Week 02
                  \n @tdemmer18 | 2019-01-23") +
  theme(legend.position = "none",
        rect = element_rect(fill = "#212F3C"),#0a1926
        panel.grid = element_blank(),
        text = element_text(colour = "white"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(colour = rgb(230, 230, 230, maxColorValue = 255), size = 11),
        axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  facet_wrap(~ side_of_ball)

# Playing around with the Box Plot Graph, but the graph doesn't really provide any value
df_gathered %>%
  ggplot(aes(x = position, y = avg_salary, group = position, color = position, fill = position)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  #facet_wrap(~ position, nrow = 2) +
  labs(x = "", y = "",
       title = "Average NFL Salaries by Position by Year",
       caption = "TidyTuesday: 2018 Week 02 \n @tdemmer18") +
  theme(legend.position = "none",
        rect = element_rect(fill = "#212F3C"),#0a1926
        panel.grid = element_blank(),
        text = element_text(colour = "white"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(colour = rgb(230, 230, 230, maxColorValue = 255), size = 11))

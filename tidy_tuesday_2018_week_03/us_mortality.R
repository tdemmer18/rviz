remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


library(readxl)
library(tidyverse)
library(gganimate)
library(ggrepel)


df <- read_excel("rstats/rviz/tidy_tuesday_2018_week_03/global_mortality.xlsx")

glimpse(df)

df_gathered <- df %>%
  gather(disease, percent, -country, -country_code, -year)

df_us <- df_gathered %>%
  filter(country == "United States",
         percent > 5)

df_us %>%
  ggplot(aes(x = year, y = percent, color = disease)) +
  geom_point() +
  facet_wrap(~ disease, nrow = 1) +
  theme(legend.position = "none")

df_us %>%
  ggplot(aes(x = year, y = percent, fill = disease)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ disease, nrow = 1) +
  theme(legend.position = "none")


df_us %>%
  ggplot(aes(x = year, y = percent, color = disease)) +
  geom_point() +
  facet_wrap(~ disease, nrow = 1) +
  theme(legend.position = "none") +
  transition_time(year) +
  ease_aes("linear")



df_us %>%
  ggplot(aes(x = percent, fill = disease)) +
  geom_dotplot()



### Look at countries and diabetes
df_diabetes <- df_gathered %>%
  na.omit() %>%
  filter(disease == "Diabetes (%)",
         year == "2015",
         percent > 17)
df_diabetes

df_diabetes %>%
  ggplot(aes(x = country_code, y = percent, color = country_code)) +
  geom_point() +
  geom_text(aes(label=country), vjust = 0.3)


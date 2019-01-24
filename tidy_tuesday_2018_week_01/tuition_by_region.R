# Standard workspace setup ----------
remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)


# Libraries -------------------------
library(readxl)
library(tidyverse)
library(ggthemes)
library(gganimate)
library(gifski)
library(viridis)
library(gridExtra)

# Import data -----------------------
df <- read_excel("~/rstats/rviz/tidy_tuesday_2018_week_01/us_avg_tuition.xlsx")

# View dim of data ------------------
dim(df)
## 50 rows, 13 columns

# View head of data -----------------
head(df)
## tibble with States and years listed in columns, tuition as observations



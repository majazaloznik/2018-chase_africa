# 0.0. preliminaries ##########################################################


# 0.0. preliminaries ##########################################################
library(dplyr)
library(tidyr)
library(ggplot2)

# read data
df <- readRDS("data/processed/dandelion.rds")
df_rounds <- readRDS(df_rounds, "data/processed/dandelion_rounds.rds")
df_years <- readRDS(df_years, "data/processed/dandelion_years.rds")


ggplot(df, aes(date, ihs_deworming)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 9),
              color = "red") +
  theme_minimal() +
  labs(title = "Deworming (Dandelion: 2014 - 2018)",
       x = "Date",
       y = "Number of persons treated")

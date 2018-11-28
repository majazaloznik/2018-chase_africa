# 0.0. preliminaries ##########################################################


# 0.0. preliminaries ##########################################################
library(dplyr) 
library(tidyr)
library(ggplot2)
library(gganimate)

# read data
df <- readRDS("data/processed/dandelion.rds")
df_rounds <- readRDS( "data/processed/dandelion_rounds.rds")
df_years <- readRDS("data/processed/dandelion_years.rds")


ggplot(df, aes(date, ihs_deworming)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 9),
              color = "red") +
  theme_minimal() +
  labs(title = "Deworming (Dandelion: 2014 - 2018)",
       x = "Date",
       y = "Number of persons treated") +
  transition_layers(0.3, 0.2, from_blank = FALSE)


ggplot(df, aes(date, fp_lt_3yr_1st+fp_lt_3yr_rep)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 9),
              color = "red") +
  labs(title = "3 year implant (Dandelion: 2014 - 2018)",
       x = "Date",
       y = "Number of persons treated") +
  transition_layers(0.4, 0.1, from_blank = FALSE) -> p
animate(p, fps = 10,  renderer = gifski_renderer(loop = FALSE),width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_3yr.gif"))

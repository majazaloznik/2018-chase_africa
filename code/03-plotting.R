# 0.0. preliminaries ##########################################################


# 0.0. preliminaries ##########################################################
library(dplyr) 
library(tidyr)
library(ggplot2)
library(gganimate)
library(transformr)


# read data
df <- readRDS("data/processed/dandelion.rds")
df_rounds <- readRDS( "data/processed/dandelion_rounds.rds")
df_years <- readRDS("data/processed/dandelion_years.rds")


df %>% 
  mutate(gr1 = 1) %>% 
ggplot(aes(date, fp_lt_3yr_1st+fp_lt_3yr_rep, group = gr1)) +
  geom_point() +
  geom_line(size = 1, color = "red") +
  theme_minimal() +
  labs(x = "Date",
       y = "Number of persons treated") +
  transition_reveal(gr1, along = date) -> p

animate(p, nframes = 25, renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_3yr_line.gif"))


df %>% 
  mutate(gr2 = 2) %>% 
  ggplot(aes(date, fp_lt_5yr_1st+fp_lt_5yr_rep, group = gr2)) +
  geom_point() +
  geom_line(size = 1, color = "blue") +
  # background
  geom_line(data = df %>%  mutate(gr1 = 1) , size = 1, color = "red",
            aes(date, fp_lt_3yr_1st+fp_lt_3yr_rep, group = gr1)) +
  theme_minimal() +
  labs(x = "Date",
       y = "Number of persons treated") +
  transition_reveal(gr2, along = date) -> p
animate(p,  nframes = 25, renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)

anim_save(paste0("docs/presentations/figures/", "fp_lt_3yr_add_5yr_line.gif"))






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
animate(p, fps = 10,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_3yr.gif"))

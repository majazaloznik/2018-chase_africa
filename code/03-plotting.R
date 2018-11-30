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

# 1. simple trendline gifs ###################################################

df %>%
  mutate(gr1 = 1) %>%
  ggplot(aes(date, fp_lt_3yr_1st+fp_lt_3yr_rep, group = gr1, color = as.factor(gr1))) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Date of clinic",
       y = "Number of persons treated") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  scale_colour_manual("Contraceptive type", values = c("red"), labels = c("3-year implant")) +
  ylim(0,220) +
  transition_reveal(gr1, along = date) -> p
animate(p,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_3yr_line.gif"))


df %>%
  mutate(gr2 = 2) %>%
  ggplot(aes(date, fp_lt_5yr_1st+fp_lt_5yr_rep, group = gr2)) +
  geom_line(size = 1, aes( color = as.factor(gr2))) +
  # background
  geom_line(data = df %>%  mutate(gr1 = 1) , size = 1,
            aes(date, fp_lt_3yr_1st+fp_lt_3yr_rep, group = gr1, color =as.factor(gr1))) +
  theme_minimal() +
  scale_colour_manual("Contraceptive type", values = c("red", "blue"), labels = c("3-year implant", "5-year implant"))+
  labs(x = "Date of clinic",
       y = "Number of persons treated") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  ylim(0,220) +
  transition_reveal(gr2, along = date)  -> p
animate(p,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_3yr_add_5yr_line.gif"))


df %>%
  mutate(fp_lt_3y = fp_lt_3yr_1st+fp_lt_3yr_rep) %>%
  select(date, fp_lt_3y) %>%
  ggplot(aes(date, fp_lt_3y, group = 1)) +
  geom_line(size = 1, aes( color = as.factor(1))) + # layer 1
  geom_point(size = 1) + # layer 2
  geom_smooth(span = 0.3, color = "red") + # layer 3
  theme_minimal() +
  scale_colour_manual("Contraceptive type", values = c("black"), labels = c("3-year implant"))+
  labs(x = "Date of clinic",
       y = "Number of persons treated") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  ylim(0,220) +
  transition_layers(3,1,TRUE, from_blank = FALSE)  -> p
animate(p, nframes = 50)

animate(p,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_3yr_add_smooth.gif"))



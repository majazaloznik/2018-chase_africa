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
col1 <- "#85A279"
col2 <-   "#DD6948"

# 1. simple trendline gifs ###################################################

# 3 year implant only
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
  scale_colour_manual("Contraceptive type", values = c(col2), labels = c("3-year implant")) +
  ylim(0,220) +
  transition_reveal(gr1, along = date) -> p
animate(p,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_3yr_line.gif"))

# 3 + 5 year implant
df %>%
  mutate(gr2 = 2) %>%
  ggplot(aes(date, fp_lt_5yr_1st+fp_lt_5yr_rep, group = gr2)) +
  geom_line(size = 1, aes( color = as.factor(gr2))) +
  # background
  geom_line(data = df %>%  mutate(gr1 = 1) , size = 1,
            aes(date, fp_lt_3yr_1st+fp_lt_3yr_rep, group = gr1, color =as.factor(gr1))) +
  theme_minimal() +
  scale_colour_manual("Contraceptive type", values = c(col2, col1), labels = c("3-year implant", "5-year implant"))+
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

# 3 year implant + smooth curve
df %>%
  mutate(fp_lt_3y = fp_lt_3yr_1st+fp_lt_3yr_rep) %>%
  select(date, fp_lt_3y) %>%
  ggplot(aes(date, fp_lt_3y, group = 1)) +
  geom_line(size = 1, aes( color = as.factor(1))) + # layer 1
  #geom_point(size = 2) + # layer 2
  geom_smooth(span = 0.3, color = col2) + # layer 3
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

animate(p,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_3yr_add_smooth.gif"))


# immunization with smooth
df %>%
  select(date, ihs_immunization) %>%
  ggplot(aes(date, ihs_immunization, group = 1)) +
  geom_line(size = 1, aes(color = as.factor(1))) + # layer 1
  # geom_point(size = 3) + # layer 2
  geom_smooth(span = 0.3, color = col2) + # layer 3
  theme_minimal() +
  scale_colour_manual("Heath service", values = c("black"), labels = c("Immunization"))+
  labs(x = "Date of clinic",
       y = "Number of persons treated") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  transition_layers(3,1,TRUE, from_blank = FALSE)  -> p

animate(p,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "ihs_immunization_add_smooth.gif"))



# depo with two smooth curves
df %>%
  mutate(fp_depo = fp_st_depo_1st+fp_st_depo_rep) %>%
  mutate(gr = 1, gr2 = 2) %>%
  select(date, fp_depo, gr, gr2) -> depo


df %>%
  mutate(fp_depo = fp_st_depo_1st+fp_st_depo_rep) %>%
  select(date, fp_depo) %>%
  mutate(gr = 1) %>%
  ggplot(aes(date, fp_depo, group = 1)) +
  geom_line(data = depo, aes(date, fp_depo, group = gr, color = as.factor(gr)), size = 1) + # layer 1
  # geom_point( data = depo, aes(date, fp_depo, group = gr),size = 3) + # layer 2
  geom_smooth( span = 0.2, aes(group = gr), color = col2, se = FALSE,  size = 1.3) + # layer 3
  geom_smooth( span = 0.7, color = col1, se = FALSE, size = 1.3) +# layer 3
  theme_minimal() +
  scale_colour_manual("Contraceptive type", values = c("black"), labels = c("Depo injection"))+
  labs(x = "Date of clinic",
       y = "Number of persons treated") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  ylim(0,150) +
  transition_layers(3,1,TRUE, from_blank = FALSE)  -> p

animate(p,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_st_depo_add_smooth_two.gif"))

# aggregated long and short term

df_rounds %>%
  select(fund_date, der_fp_lt_total, der_fp_st_total) %>%
  gather(var, val, 2:3) %>%
  arrange(fund_date) %>%
ggplot( aes(fund_date, y = val, colour = var)) +
  geom_point(size = 3) +
  geom_line( size = 1) +
  scale_color_manual(values = c(col2, col1), name = "Contraceptive type",
                     labels = c("Long term contraceptives",
                                "Short term contraceptives")) +
  theme_minimal() +
  labs(x = "Date of funding round",
       y = "Number of persons treated")+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(0,1),legend.position=c(0,1))+
  ylim(0,1500) +
  transition_reveal(var, fund_date) -> p
animate(p,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_st_funding_round.gif"))

# aggregate long and short - yearly

df_years %>%
  select(year, der_fp_lt_total, der_fp_st_total) %>%
  gather(key = var, value = freq, 2:3) %>%
  ggplot(aes( x = year, y = freq, col = var)) +
  geom_bar(width = 0.4, stat = "identity", position = position_dodge(0.5),fill = "white",
           aes(color = as.factor(var), size = as.factor(year))) +
  scale_color_manual(values = c(col2, col1), labels = c("Long term", "Short term"),
                     name = "Contraceptive type") +
  scale_size_manual(values=c(1.2,1.2,1.2,1.2,.4),guide=F) +
  theme_minimal() +
  labs(x = "Year",
       y = "Number of persons treated")
ggsave(width = 10, height = 6,   paste0("docs/presentations/figures/", "fp_lt_st_funding_year.png"))


## aggregate all family planning - yearly

df_years %>%
  select(year, starts_with("fp")) %>%
  mutate(fp_lt_5yr = fp_lt_5yr_1st + fp_lt_5yr_rep,
         fp_lt_3yr = fp_lt_3yr_1st + fp_lt_3yr_rep,
         fp_st_depo = fp_st_depo_1st + fp_st_depo_rep) %>%
  select(-fp_condoms, -fp_lt_5yr_1st, -fp_lt_5yr_rep,
         -fp_lt_3yr_1st, -fp_lt_3yr_rep,
         -fp_st_depo_1st, -fp_st_depo_rep) %>%
  gather(key = var, value = freq, 2:11) %>%
  filter(!is.na(freq))  %>%
  mutate(var = factor(var, labels = c("3 year implant", "5 year implant", "IUCD",
                                      "Depo injection", "Pills 1 month", "Pills 3 months"))) %>%
  ggplot(aes(x = year, y = freq)) +
  geom_bar(width = 0.4, stat = "identity", position = position_dodge(0.5), color = col1, fill = "white",
           aes( size = as.factor(year))) +
  scale_size_manual(values=c(1,1,1,1,.4),guide=F) +
  theme_minimal() +
  theme( title = element_text(size=18)) +
  labs(title = "{closest_state}") +
  labs(x = "Year",
       y = "Number of persons treated")+
  transition_states(var, transition_length = 2, state_length = 3) -> p

animate(p, width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_all_year.gif"))

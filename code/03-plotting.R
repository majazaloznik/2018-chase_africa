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
        legend.justification=c(1,1),legend.position=c(1,1))+
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
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  labs(x = "Year",
       y = "Number of persons treated") +
  transition_layers(transition_length = 1, layer_length = 1) -> p
animate(p, nframes = 1, renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_st_funding_year.gif"))



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
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1))+
  theme( title = element_text(size=18)) +
  labs(title = "{closest_state}") +
  labs(x = "Year",
       y = "Number of persons treated") +
  transition_states(var, transition_length = 2, state_length = 3) -> p

animate(p, width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_all_year.gif"))

# rates long to short term contraception

df %>%
  rowwise() %>%
  mutate(prop_lt_total = der_fp_lt_total/(der_fp_lt_total+ der_fp_st_total),
         prop_st_total = der_fp_st_total/(der_fp_lt_total+ der_fp_st_total)) %>%
  select(date, prop_lt_total, prop_st_total) %>%
  gather(key = var, value = freq, 2:3) -> df_sub

ggplot(data = df_sub, aes( x = date, y = freq, col = var)) +
  geom_bar(stat = "identity", aes(color = as.factor(var),  fill = as.factor(var))) +
  geom_point(data = subset(df_sub, var == "prop_st_total"), col = "black", size = 2) +
  geom_smooth(data = subset(df_sub, var == "prop_st_total"),
              aes(x = date, y =  freq), col = "black", size = 1, span = 0.4, se = FALSE)   +
  scale_color_manual(values = c(col1, col2), labels = c("Long term", "Short term"),
                     name = "Contraceptive type") +
  scale_fill_manual(values = c(col1, col2), labels = c("Long term", "Short term"),
                    name = "Contraceptive type") +
  scale_size_manual(values=c(1,1,1,1,.4),guide=F) +
  theme_minimal() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  labs(x = "Date of clinic",
       y = "Number of persons treated")+
  theme(text = element_text(size=8)) +
  transition_layers(transition_length = 1, layer_length = 2, from_blank = FALSE) -> p

animate(p,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_st_rate_clinics.gif"))


# rates long to short annual

df_rounds %>%
  rowwise() %>%
  mutate(prop_lt_total = der_fp_lt_total/(der_fp_lt_total+ der_fp_st_total),
         prop_st_total = der_fp_st_total/(der_fp_lt_total+ der_fp_st_total)) %>%
  select(fund_date, prop_lt_total, prop_st_total) %>%
  gather(key = var, value = freq, 2:3) -> df_sub

ggplot(data = df_sub, aes( x = fund_date, y = freq, col = var)) +
  geom_bar(stat = "identity",
           aes(color = as.factor(var), fill = as.factor(var))) +
  geom_point(data = subset(df_sub, var == "prop_st_total"), col = "black", size = 2) +
   geom_smooth(data = subset(df_sub, var == "prop_st_total"),
              aes(x = fund_date, y =  freq), col = "black", size = 1, span = 0.4, se = FALSE)   +
  scale_color_manual(values = c(col1, col2), labels = c("Long term", "Short term"),
                     name = "Contraceptive type") +
  scale_fill_manual(values = c(col1, col2), labels = c("Long term", "Short term"),
                    name = "Contraceptive type") +
  scale_size_manual(values=c(1,1,1,1,.4),guide=F) +
  theme_minimal() +
  labs(x = "Date of funding round",
       y = "Number of persons treated") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  transition_layers(transition_length = 1, layer_length = 2, from_blank = FALSE) -> p

animate(p,  renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_st_rate_rounds.gif"))



df_years %>%
  rowwise() %>%
  mutate(prop_lt_total = der_fp_lt_total/(der_fp_lt_total+ der_fp_st_total),
         prop_st_total = der_fp_st_total/(der_fp_lt_total+ der_fp_st_total)) %>%
  select(year, prop_lt_total, prop_st_total) %>%
  gather(key = var, value = freq, 2:3) -> df_sub

ggplot(data = df_sub, aes( x = year, y = freq, col = var)) +
  geom_bar(stat = "identity",width = 0.4,
           aes(color = as.factor(var), fill = as.factor(var), alpha= as.factor(year))) +
  scale_color_manual(values = c(col1, col2), labels = c("Long term", "Short term"),
                     name = "Contraceptive type") +
  scale_fill_manual(values = c(col1, col2), labels = c("Long term", "Short term"),
                    name = "Contraceptive type") +
  scale_size_manual(values=c(1,1,1,1,.4),guide=F) +
  theme_minimal() +
  scale_alpha_manual(values=c(1,1,1,1,.4),guide=F) +
  labs(x = "Year",
       y = "Number of persons treated") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  transition_layers(transition_length = 1, layer_length = 2, from_blank = FALSE) -> p

animate(p, nframes = 1, renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "fp_lt_st_rate_years.gif"))


## cyp trend funding round

df_rounds %>%
  arrange(fund_date) %>%
  ggplot(aes(fund_date, der_gpb_per_cyp)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(x = "Date of funding round",
       y = "GBP per CYP")+
  theme(text = element_text(size=8))+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  transition_reveal(1, fund_date) ->p

animate(p, renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "cyp_fund_rounds.gif"))


df_years %>%
  arrange(year) %>%
  ggplot(aes(year, der_gpb_per_cyp)) +
  geom_bar(stat = "identity", col = col2, fill = col1, size = 1,
           width = 0.4,
           aes( size = as.factor(year),
                alpha = as.factor(year),
                fill =as.factor(year))) +
  theme_minimal() +
  scale_alpha_manual(values=c(1,1,1,1,.4),guide=F) +
  scale_size_manual(values=c(1,1,1,1,.4),guide=F) +
  labs(x = "Year",
       y = "GBP per CYP") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  transition_layers(transition_length = 1, layer_length = 2, from_blank = FALSE) -> p

animate(p, nframes = 1, renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "cyp_fund_years.gif"))


df_rounds %>%
  rowwise() %>%
  ggplot(aes(x= der_fp_total, y = der_gpb_per_cyp)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(col = col2) +
  labs(x = "Total number of FP recipients",
       y = "GBP per CYP") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(1,1)) +
  transition_layers(transition_length = 1, layer_length = 2, from_blank = FALSE) -> p

animate(p, renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "cyp_by_fp_total.gif"))

# structure by CYP each round
df_rounds %>%
  select(fund_date,
         der_fp_lt_iucd_cyp,
         der_fp_implants_tot_cyp,
         der_fp_depo_tot_cyp,
         der_fp_pills_tot_cyp) %>%
  gather(key = contr_type, value = cyp, 2:5) %>%
  group_by(fund_date) %>%
  mutate(prop.cyp = cyp/sum(cyp)) %>%
  arrange(fund_date) %>%
  ggplot(aes(x = fund_date, y = prop.cyp, fill = contr_type)) +
  geom_area(color = "gray") +
  scale_fill_manual(values = c(col2, col1, "goldenrod2", "forestgreen"),
                    labels = c("Depo injections", "Implants", "IUCDs", "Pills"),
                    name = "Contraceptive type") +
  theme_minimal() +
  labs( x = "Date of funding round",
        y = "Proportion of total CYP provided")+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        legend.justification=c(1,1),legend.position=c(.93,.9)) +
  transition_layers(transition_length = 1, layer_length = 2, from_blank = FALSE) -> p

animate(p, nframes = 1, renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "cyp_str_rounds.gif"))

# structure by CYP each year

df_years %>%
  select(year,
         der_fp_lt_iucd_cyp,
         der_fp_implants_tot_cyp,
         der_fp_depo_tot_cyp,
         der_fp_pills_tot_cyp) %>%
  gather(key = contr_type, value = cyp, 2:5) %>%
  group_by(year) %>%
  mutate(prop.cyp = cyp/sum(cyp)) %>%
  arrange(year) %>%
  ggplot(aes(x = year, y = prop.cyp, fill = contr_type)) +
  geom_bar(stat = "identity", width = 0.4,
           aes( size = as.factor(year),
                alpha = as.factor(year) )) +
  scale_fill_manual(values = c(col2, col1, "goldenrod2", "forestgreen"),
                    labels = c("Depo injections", "Implants", "IUCDs", "Pills"),
                    name = "Contraceptive type") +
  theme_minimal() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20)) +
  labs( x = "Date of funding round",
        y = "Proportion of total CYP provided")+
  theme(text = element_text(size=8)) +
  scale_alpha_manual(values=c(1,1,1,1,.4),guide=F) +
  scale_size_manual(values=c(1,1,1,1,.1),guide=F) +
transition_layers(transition_length = 1, layer_length = 2, from_blank = FALSE) -> p

animate(p, nframes = 1, renderer = gifski_renderer(loop = FALSE), width = 1000, height = 600)
anim_save(paste0("docs/presentations/figures/", "cyp_str_years.gif"))

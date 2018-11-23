# 1. preliminaries ############################################################

library(readxl)
library(readr)
library(dplyr)
library(tidyr)

# 2. clean up dandeliion ######################################################
# import sheet 1 ##############################################################
dandelion_2014 <- read_excel("data/raw/2014-18 Dandelion data summary & CYP CP .xlsx",
                             sheet = "Dandelion data 2014", skip = 5,
                             col_types = c("date", "text", rep("numeric", 20)), col_names = 
                               c("date",
                                 "venue",
                                 "funding",
                                 "lt_iucd",
                                 "lt_5yr_1st",
                                 "lt_5yr_rep",
                                 "lt_3yr_1st",
                                 "lt_3yr_rep",
                                 "st_depo_1st",
                                 "st_depo_rep",
                                 "st_pills_1mth",
                                 "der_total_fp",
                                 "der_cyp_total",
                                 "condoms",
                                 "ihs_deworming",
                                 "ihs_immunization",
                                 "ihs_primary_hc",
                                 "ihs_hiv_test",
                                 "ihs_hiv_poz",
                                 "ihs_cancer_test",
                                 "der_total_fp_ihc",
                                 "der_total_ihc"))
                       
  
# basic clean up of sheet 1
# remove rows that are not clinics
dandelion_2014 %>% 
  filter(!is.na(date)) %>% 
  mutate(fund_var = rep(c("fund_ksh", "fund_date", "fund_gbp"),2),
         fund_round = c(1,1,1,2,2,2)) %>% 
  spread(key = fund_var, value = funding) %>% 
  mutate(fund_category = c(rep("standard", 6))) %>% 
  group_by(fund_round) %>% 
  fill(fund_ksh, fund_date, fund_gbp) %>% 
  fill(fund_ksh, fund_date, fund_gbp, .direction = "up")  %>% 
  mutate(fund_date = as.Date(fund_date, origin = "1899-12-30")) -> dandelion_2014
  

# import sheet 2 ##############################################################
dandelion_2015 <- read_excel("data/raw/2014-18 Dandelion data summary & CYP CP .xlsx",
                             sheet = "Dandelion data 2015", skip = 5,
                             col_types = c("date", "text", rep("numeric", 20)), col_names = c("date",
                                                                                    "venue",
                                                                                    "funding",
                                                                                    "lt_iucd",
                                                                                    "lt_5yr_1st",
                                                                                    "lt_5yr_rep",
                                                                                    "lt_3yr_1st",
                                                                                    "lt_3yr_rep",
                                                                                    "st_depo_1st",
                                                                                    "st_depo_rep",
                                                                                    "st_pills_1mth",
                                                                                    "der_total_fp",
                                                                                    "der_cyp_total",
                                                                                    "condoms",
                                                                                    "ihs_deworming",
                                                                                    "ihs_immunization",
                                                                                    "ihs_primary_hc",
                                                                                    "ihs_hiv_test",
                                                                                    "ihs_hiv_poz",
                                                                                    "ihs_cancer_test",
                                                                                    "der_total_fp_ihc",
                                                                                    "der_total_ihc"))


# basic clean up of sheet 2
# remove rows that are not clinics
dandelion_2015%>% 
  filter(!is.na(date)) %>% 
  mutate(fund_round = c(rep(3,5), rep(4,7), rep(5,6), rep(6,7)),
         funding = sub(".PopOff.", "", funding),
         fund_var = c("fund_ksh", "fund_date", "fund_gbp", "del", "del",
                      "fund_ksh", "fund_date", "fund_gbp", "del", "del", "del", "del",
                      "fund_ksh", "fund_date", "fund_gbp", "del", "del", "del",
                      "fund_ksh", "fund_date", "fund_gbp", "del", "del", "del", "del") )%>% 
  spread(key = fund_var, value = funding) %>% 
  mutate(fund_date = ifelse(grepl("[[:alpha:]]", fund_date),  as.Date(fund_date, "%d-%b-%y"), 
                            as.Date(as.numeric(fund_date), origin = "1899-12-30")),
         fund_date = as.Date(fund_date,origin = "1970-01-01"))  %>% 
  mutate(fund_category = c(rep("standard", 25))) %>% 
  group_by(fund_round) %>% 
  fill(fund_ksh, fund_date, fund_gbp) %>% 
  fill(fund_ksh, fund_date, fund_gbp, .direction = "up")  %>% 
  mutate(fund_gbp = as.numeric(fund_gbp), fund_ksh = as.numeric(fund_ksh)) %>% 
  select(-del) -> dandelion_2015


# import sheet 3 ##############################################################
dandelion_2016 <- read_excel("data/raw/2014-18 Dandelion data summary & CYP CP .xlsx",
                             sheet = "Dandelion data 2016", skip = 5,
                             col_types = c("date", "text", rep("numeric", 20)), col_names = c("date",
                                                                                    "venue",
                                                                                    "funding",
                                                                                    "lt_iucd",
                                                                                    "lt_5yr_1st",
                                                                                    "lt_5yr_rep",
                                                                                    "lt_3yr_1st",
                                                                                    "lt_3yr_rep",
                                                                                    "st_depo_1st",
                                                                                    "st_depo_rep",
                                                                                    "st_pills_1mth",
                                                                                    "der_total_fp",
                                                                                    "der_cyp_total",
                                                                                    "condoms",
                                                                                    "ihs_deworming",
                                                                                    "ihs_immunization",
                                                                                    "ihs_primary_hc",
                                                                                    "ihs_hiv_test",
                                                                                    "ihs_hiv_poz",
                                                                                    "ihs_cancer_test",
                                                                                    "der_total_fp_ihc",
                                                                                    "der_total_ihc"))


# basic clean up of sheet 3 
# remove rows that are not clinics
dandelion_2016 %>% 
  filter(!is.na(date)) %>% 
  mutate(fund_round = c(rep(7,9), rep(8,9), rep(9,7), rep(10,6), rep(11, 6), rep(12, 4)),
         funding = sub(".PopOff.", "", funding),
         fund_var = c("fund_ksh", "fund_date", "fund_gbp", rep("del", 6),
                      "fund_ksh", "fund_date", "fund_gbp", rep("del", 6),
                      "fund_ksh", "fund_date", "fund_gbp", rep("del", 4),
                      "fund_ksh", "fund_date", "fund_gbp", rep("del", 3),
                      "fund_ksh", "fund_date", "fund_gbp", rep("del", 3),
                      "fund_ksh", "fund_date", "fund_gbp", rep("del", 1))) %>% 
  spread(key = fund_var, value = funding)  %>%
  mutate(fund_category = c(rep("standard", 37), rep("amplify change", 4))) %>% 
  mutate(fund_gbp= ifelse(fund_gbp == 9750, 8628, fund_gbp),
         fund_date = ifelse(fund_date == 8628, NA, fund_date)) %>% 
  mutate(fund_date = as.Date(as.numeric(fund_date), origin = "1899-12-30"))%>% 
  group_by(fund_round) %>% 
  fill(fund_ksh, fund_date, fund_gbp) %>% 
  fill(fund_ksh, fund_date, fund_gbp, .direction = "up")  %>% 
  mutate(fund_gbp = as.numeric(fund_gbp), fund_ksh = as.numeric(fund_ksh)) %>% 
  select(-del) -> dandelion_2016


# import sheet 4 ##############################################################
dandelion_2017 <- read_excel("data/raw/2014-18 Dandelion data summary & CYP CP .xlsx",
                             sheet = "Dandelion data 2017", skip = 5,
                             col_types = c("date", "text", rep("numeric", 20)), col_names = c("date",
                                                                                    "venue",
                                                                                    "funding",
                                                                                    "lt_iucd",
                                                                                    "lt_5yr_1st",
                                                                                    "lt_5yr_rep",
                                                                                    "lt_3yr_1st",
                                                                                    "lt_3yr_rep",
                                                                                    "st_depo_1st",
                                                                                    "st_depo_rep",
                                                                                    "st_pills_1mth",
                                                                                    "der_total_fp",
                                                                                    "der_cyp_total",
                                                                                    "condoms",
                                                                                    "ihs_deworming",
                                                                                    "ihs_immunization",
                                                                                    "ihs_primary_hc",
                                                                                    "ihs_hiv_test",
                                                                                    "ihs_hiv_poz",
                                                                                    "ihs_cancer_test",
                                                                                    "der_total_fp_ihc",
                                                                                    "der_total_ihc"))


# basic clean up of sheet 4
# remove rows that are not clinics
dandelion_2017$venue[75] <- "not"
dandelion_2017$date[75] <- "2001-01-01"
dandelion_2017 %>% 
  filter(!is.na(date))  %>% 
  mutate(fund_round = c(rep(13,12), rep(14,12), rep(15,12), rep(16,18),
                        rep(17,3), rep(18,4), rep(19,4)),
         funding = sub(".PopOff.", "", funding),
         fund_var = c("fund_ksh", "fund_date", "fund_gbp", rep("del", 9),
                      "fund_ksh", "fund_date", "fund_gbp", rep("del", 9),
                      "fund_ksh", "fund_date", "fund_gbp", rep("del", 9),
                      "fund_ksh", "fund_date", "fund_gbp", rep("del", 15),
                      "fund_ksh", "fund_date", "fund_gbp",
                      "fund_ksh", "fund_date", "fund_gbp", rep("del", 1),
                      "fund_ksh", "fund_date", "fund_gbp", rep("del", 1)))  %>% 
  spread(key = fund_var, value = funding)%>%
  mutate(fund_category = c(rep("standard", 54), rep("amboseli", 11))) %>% 
  mutate(fund_date = as.Date(as.numeric(fund_date), origin = "1899-12-30"))%>% 
  group_by(fund_round) %>% 
  fill(fund_ksh, fund_date, fund_gbp) %>% 
  fill(fund_ksh, fund_date, fund_gbp, .direction = "up")  %>% 
  mutate(fund_gbp = as.numeric(fund_gbp), fund_ksh = as.numeric(fund_ksh)) %>% 
  filter(venue != "not") %>% 
  select(-del) -> dandelion_2017

# import sheet 5 ##############################################################
dandelion_2018 <- read_excel("data/raw/2014-18 Dandelion data summary & CYP CP .xlsx",
                             sheet = "Dandelion data 2018", skip = 5,
                             col_types = c("date", "text", rep("numeric", 33)), col_names = c("date",
                                                                                              "venue",
                                                                                              "funding",
                                                                                              "lt_iucd",
                                                                                              "lt_5yr_1st",
                                                                                              "lt_5yr_rep",
                                                                                              "lt_3yr_1st",
                                                                                              "lt_3yr_rep",
                                                                                              "st_depo_1st",
                                                                                              "st_depo_rep",
                                                                                              "st_pills_6mth",
                                                                                              "st_pills_3mth",
                                                                                              "st_pills_1mth",
                                                                                              "st_pills_1st",
                                                                                              "der_total_fp",
                                                                                              "der_cyp_total",
                                                                                              "condoms",
                                                                                              "fp_under18",
                                                                                              "fp_over18",
                                                                                              "lt_iucd_remove",
                                                                                              "fp_disabled",
                                                                                              "ihs_primary_hc",
                                                                                              "ihs_deworming",
                                                                                              "ihs_immunization",
                                                                                              "ihs_hiv_test",
                                                                                              "ihs_hiv_poz",
                                                                                              "ihs_malaria_test",
                                                                                              "ihs_malaria_poz",
                                                                                              "ihs_cancer_test",
                                                                                              "ihs_cancer_poz",
                                                                                              "ihs_hepB_test",
                                                                                              "ihs_hepB_poz",
                                                                                              "ihs_disabled",
                                                                                              "der_total_fp_ihc",
                                                                                              "der_total_ihc"))
# basic clean up of sheet 5
# remove rows that are not clinics
dandelion_2018 %>% 
  filter(!is.na(date))  %>% 
  mutate(fund_round = c(rep(20,16)),
         funding = sub(".PopOff.", "", funding),
         fund_var = c("fund_ksh", "fund_date", "fund_gbp", rep("del", 13)))  %>% 
  spread(key = fund_var, value = funding)%>%
  mutate(fund_category = c(rep("standard", 16))) %>% 
  mutate(fund_date = as.Date(as.numeric(fund_date), origin = "1899-12-30"))%>% 
  group_by(fund_round) %>% 
  fill(fund_ksh, fund_date, fund_gbp) %>% 
  fill(fund_ksh, fund_date, fund_gbp, .direction = "up")  %>% 
  mutate(fund_gbp = as.numeric(fund_gbp), fund_ksh = as.numeric(fund_ksh)) %>% 
  select(-del) -> dandelion_2018


# merge allsheets ##############################################################

bind_rows(dandelion_2018,
          dandelion_2014,
          dandelion_2015, 
          dandelion_2016,
          dandelion_2017) %>% 
  ungroup() %>% 
  arrange(date) -> dandelion

saveRDS(dandelion, "data/processed/dandelion.rds")
write_csv(dandelion, "data/processed/dandelion.csv")

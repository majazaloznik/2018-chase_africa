# 0.0. preliminaries ##########################################################
# 1.0 derive totals ###########################################################
# 2. summaries per funding round ##############################################
# 2.5 summaries per year and funding round but with only standard ones#########
# 3. variable list ############################################################

# 0.0. preliminaries ##########################################################
library(dplyr)
library(tidyr)
library(readr)

# read data
df <- readRDS("data/interim/dandelion.rds")

# 1.0 derive totals ###########################################################

df %>% 
  select(-der_cyp_total) %>% 
  mutate(der_fp_lt_total = rowSums(select(., starts_with("fp_lt")), na.rm = TRUE), 
         der_fp_st_total = rowSums(select(., starts_with("fp_st")), na.rm = TRUE)) %>% 
  mutate(der_fp_total = rowSums(select(., der_fp_lt_total, der_fp_st_total)),
         der_ihs_total = rowSums(select(., matches("^ihs_.*[^z]$")), na.rm = TRUE)) %>% 
  mutate(der_total = rowSums(select(.,der_fp_total, der_ihs_total))) %>% 
  mutate(der_fp_lt_iucd_cyp = fp_lt_iucd * 4.6,
         der_fp_lt_5yr_1st_cyp = fp_lt_5yr_1st * 3.8,
         der_fp_lt_5yr_1rep_cyp = fp_lt_5yr_rep * 3.8,
         der_fp_lt_3yr_1st_cyp = fp_lt_3yr_1st * 2.5,
         der_fp_lt_3yr_1rep_cyp = fp_lt_3yr_rep * 2.5,
         der_fp_st_depo_1st_cyp = fp_st_depo_1st * 0.25, 
         der_fp_st_depo_1rep_cyp = fp_st_depo_rep * 0.25,
         der_fp_st_pills_6mth_cyp = fp_st_pills_6mth / 2.2, 
         der_fp_st_pills_3mth_cyp = fp_st_pills_3mth / 5,
         der_fp_st_pills_1mth_cyp = fp_st_pills_1mth / 15,
         der_fp_st_pills_1st_cyp = fp_st_pills_1st / 15) %>% 
  mutate(der_fp_implants_tot_cyp = rowSums( select(., matches("^der_fp_lt_.yr.*_cyp$")), na.rm = TRUE),
         der_fp_depo_tot_cyp = rowSums( select(., matches("^der_fp_st_depo.*_cyp$")), na.rm = TRUE),
         der_fp_pills_tot_cyp = rowSums( select(., matches("^der_fp_st_pills.*_cyp$")), na.rm = TRUE),
         der_fp_lt_tot_cyp = rowSums( select(., matches("^der_fp_lt.*_cyp$")), na.rm = TRUE),
         der_fp_st_tot_cyp = rowSums( select(., matches("^der_fp_st.*_cyp$")), na.rm = TRUE)) %>% 
  mutate(der_fp_tot_cyp = rowSums( select(., der_fp_lt_tot_cyp, der_fp_st_tot_cyp), na.rm = TRUE)) %>% 
  select(-starts_with("der_total_")) -> df

# 2. summaries per year and funding round #####################################

df %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  group_by(fund_round) %>% 
  select( -date, -venue) %>% 
  mutate_at(vars(-matches("fund.*"), -year), sum) %>% 
  summarise_all(first) -> df_rounds

df_rounds %>% 
  group_by(year) %>% 
  select(-fund_date, -fund_category, - fund_round) %>% 
  summarise_all(sum)  %>% 
  mutate(der_gpb_per_person = fund_gbp/der_fp_total,
         der_gpb_per_cyp =  fund_gbp/der_fp_tot_cyp,
         der_ksh_per_person = fund_ksh/der_fp_total,
         der_ksh_per_cyp =  fund_ksh/der_fp_tot_cyp) -> df_years

df_rounds %>% 
  mutate(der_gpb_per_person = fund_gbp/der_fp_total,
         der_gpb_per_cyp =  fund_gbp/der_fp_tot_cyp,
         der_ksh_per_person = fund_ksh/der_fp_total,
         der_ksh_per_cyp =  fund_ksh/der_fp_tot_cyp) -> df_rounds

# 2.5 summaries per year and funding round but with only standard ones#########

df %>% 
  filter(fund_category == "standard") %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  group_by(fund_round) %>% 
  select( -date, -venue) %>% 
  mutate_at(vars(-matches("fund.*"), -year), sum) %>% 
  summarise_all(first) -> df_rounds_standard

df_rounds_standard %>% 
  group_by(year) %>% 
  select(-fund_date, -fund_category, - fund_round) %>% 
  summarise_all(sum)  %>% 
  mutate(der_gpb_per_person = fund_gbp/der_fp_total,
         der_gpb_per_cyp =  fund_gbp/der_fp_tot_cyp,
         der_ksh_per_person = fund_ksh/der_fp_total,
         der_ksh_per_cyp =  fund_ksh/der_fp_tot_cyp) -> df_years_standard

df_rounds_standard %>% 
  mutate(der_gpb_per_person = fund_gbp/der_fp_total,
         der_gpb_per_cyp =  fund_gbp/der_fp_tot_cyp,
         der_ksh_per_person = fund_ksh/der_fp_total,
         der_ksh_per_cyp =  fund_ksh/der_fp_tot_cyp) -> df_rounds_standard

# 3. variable list ############################################################

var_list <- data.frame(var_name = c(colnames(df),
                                    "der_gpb_per_person",
                                    "der_gpb_per_cyp",
                                    "der_ksh_per_person",
                                    "der_ksh_per_cyp"),
                       source = c(rep("original", 35), rep("derived", 26)),
                       description = c("Date of clinic",
                                       "Location of clinic", 
                                       "IUCD",
                                       "5 year implant, 1st",
                                       "5 year implant, repeat",
                                       "3 year implant, 1st",
                                       "3 year implant, repeat",
                                       "Depo injection, 1st",
                                       "Depo injection, repeat",
                                       "Pills, 6 months",
                                       "Pills, 3 months",
                                       "Pills, 1 months",
                                       "Pills, 1 months, 1st",
                                       "Condoms",
                                       "FP recipients under 18",
                                       "FP recipients over 18",
                                       "removal of IUCD",
                                       "FP recipients disabled",
                                       "Primary health care provided",
                                       "Deworming",
                                       "Immunization",
                                       "HIV/AIDS test",
                                       "HIV positive result",
                                       "Malaria test",
                                       "Malaria positive result",
                                       "Cancer screening",
                                       "Cancer positive result",
                                       "Hepatitis B test",
                                       "Hepatitis B positive result",
                                       "IHS recipients disabled",
                                       "Funding round",
                                       "Funding date",
                                       "Funds in GBP",
                                       "Funds in KSH",
                                       "Funding category",
                                       "Recipients of long term contraceptives",
                                       "Recipients of short term contraceptives",
                                       "FP recipients in total",
                                       "IHS recipients total",
                                       "All recipients total",
                                       "CYPs from IUCDs",
                                       "CYPs from 5 year implants, 1st",
                                       "CYPs from 5 year implants, rep",
                                       "CYPs from 3 year implants, 1st",
                                       "CYPs from 3 year implants, rep",
                                       "CYPs from Depo injections, 1st",
                                       "CYPs from Depo injections, rep",
                                       "CYPs from pills, 6 months",
                                       "CYPs from pills, 3 months",
                                       "CYPs from pills, 1 months",
                                       "CYPs from pills, 1 months, 1st",
                                       "CYPs from all implants",
                                       "CYPs from all injections",
                                       "CYPs from all pills",
                                       "CYPs from all long term methods",
                                       "CYPs from all short term methods",
                                       "CYPs from all methods",
                                       "Cost per person in GBP",
                                       "Cost per CYP in GBP",
                                       "Cost per person in KSH",
                                       "Cost per CYP in KSH"))


saveRDS(var_list, "data/processed/var_list.rds")
saveRDS(df, "data/processed/dandelion.rds")
saveRDS(df_rounds, "data/processed/dandelion_rounds.rds")
saveRDS(df_years, "data/processed/dandelion_years.rds")
write_csv(df, "data/processed/dandelion.csv")
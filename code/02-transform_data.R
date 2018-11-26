# 0.0. preliminaries ##########################################################




# 0.0. preliminaries ##########################################################
library(dplyr)
library(tidyr)


# read data
df <- readRDS("data/processed/dandelion.rds")


# derive totals
df %>% 
  mutate(der_fp_lt_total = rowSums(select(., starts_with("fp_lt")), na.rm = TRUE), 
         der_fp_st_total = rowSums(select(., starts_with("fp_st")), na.rm = TRUE)) %>% 
  mutate(der_fp_total = rowSums(select(., der_fp_lt_total, der_fp_st_total))) %>% 
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
  mutate(der_fp_lt_cyp = rowSums( select(., matches("^der_fp_lt.*_cyp$")))) -> df


df %>% select(matches("^der_fp_lt.*_cyp$")) %>% colnames



  rowSums(select(df, der_fp_lt_total, der_fp_st_total))

df <- data.frame(var_1 = rnorm(10,1),
           var_2 = rnorm(10,1))

df %>% 
  mutate(var_1_x = var_1 * 2,
         var_2_x = var_1 * 3 ,
         final = rowSums(., select(matches("^var.*x$"))))


df %>% 
  mutate(var_1_x = var_1 * 2,
         var_2_x = var_1 * 3) %>% 
  mutate( final = rowSums(., select(matches("^var.*x$"))))



df %>% 
  mutate(var_1_x = var_1 * 2,
         var_2_x = var_1 * 3) -> temp

temp %>% mutate(final = rowSums(., select(matches("^var.*x$"))))

select(temp, matches("^var.*x$"))

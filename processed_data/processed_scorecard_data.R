library(fixest)
library(lubridate)
library(purrr)
library(tidyverse)
library(vtable)

library(patchwork)

## Trends clean and aggregation
trends_vec <- list.files("data/",pattern = "trends_up_to_", full.names = TRUE)

trends <- map_df(trends_vec, read_csv) %>% 
  drop_na()
trends <- trends %>% mutate(date_wk_s = str_sub(monthorweek, 1, 10))
trends <- trends %>% mutate(date_wk_s = ymd(date_wk_s))
trends <- trends %>% 
  filter(date_wk_s >= ymd(20140901)) %>%
  filter( date_wk_s < ymd(20150401) | date_wk_s > ymd(20150830)) # Compares sep14 to mar15 and sep15 to mar16
trends <- trends %>% mutate(sc_implmnt = date_wk_s >= ymd(20150901)) 
trends <- trends %>% group_by(schname, keyword) %>%
  mutate(ind_mean = mean(index, na.rm = TRUE))
trends <- trends %>% group_by(schname, keyword) %>%
  mutate(ind_std = sd(index)) %>% ungroup()
  
trends <- trends %>% mutate(ind_stand = (index - ind_mean)/ind_std) # Standardize rankings
trends <- trends %>% group_by(schname, sc_implmnt) %>%
  mutate(ind_stand_pre_sc = case_when(sc_implmnt != TRUE ~ mean(ind_stand, na.rm = TRUE),
                                      FALSE ~ 0)) %>% ungroup()
trends <- trends %>% group_by(schname, sc_implmnt) %>%
  mutate(ind_stand_post_sc = case_when(sc_implmnt == TRUE ~ mean(ind_stand, na.rm = TRUE),
                                      FALSE ~ 0)) %>% ungroup()

trends <- trends %>% group_by(schname) %>% summarize(ind_z_pre = mean(ind_stand_pre_sc, na.rm = TRUE),
                                                  ind_z_post = mean(ind_stand_post_sc, na.rm = TRUE),
                                                  ind_z_shift = ind_z_post - ind_z_pre)

## Id link table clean
id_name_link <- read_csv("data/id_name_link.csv")
id_name_link <- id_name_link %>% group_by(schname) %>%
  mutate(n = n()) %>% filter(n == 1) %>%
  subset(select = -n)

## Filter scorecard and create earning category
scorecard <- read_csv("data/Most+Recent+Cohorts+(Scorecard+Elements).csv")
scorecard <- scorecard %>% filter(PREDDEG == 3)
scorecard <- scorecard %>% rename(med_earnings_10yrs = `md_earn_wne_p10-REPORTED-EARNINGS` )

scorecard <- scorecard %>%
  mutate(`med_earnings_10yrs` = as.numeric(`med_earnings_10yrs`)) %>%
  drop_na(`med_earnings_10yrs`)

quantile(scorecard$`med_earnings_10yrs`)
scorecard <- scorecard %>% mutate(earn_cat = case_when(`med_earnings_10yrs` >
                                    quantile(scorecard$`med_earnings_10yrs`)[4] ~  "high",
                                  `med_earnings_10yrs` <
                                  quantile(scorecard$`med_earnings_10yrs`)[2] ~ "low",
                                  TRUE ~ "mid")
                                  ) %>% filter(earn_cat != "mid") %>% 
  mutate(across(c(LOCALE, CONTROL), as.factor))


scorecard$CONTROL <- recode_factor(scorecard$CONTROL,
                                         "1" = "Public",
                                         "2" = "PrivateNonProfit",
                                         "3" = "PrivateForProfit")


scorecard <- scorecard %>% mutate(earn_cat = case_when(earn_cat == "high" ~ 1,
                                                       TRUE ~ 0)) %>% rename(high_earning = earn_cat )

scorecard <- scorecard %>% mutate(across(c(SAT_AVG, UGDS, GRAD_DEBT_MDN_SUPP, UG25abv), as.numeric))
scorecard <- scorecard %>% mutate(across(contains("PCIP"), as.numeric))
scorecard <- scorecard %>% mutate(across(contains("PCT"), as.numeric))
scorecard <- scorecard %>% mutate (prgm_prct_stem_lg_bus = rowSums(across(c("PCIP10",
                                                                     "PCIP11",
                                                                     "PCIP14", 
                                                                     "PCIP15",
                                                                     "PCIP22",
                                                                     "PCIP26", 
                                                                     "PCIP27", 
                                                                     "PCIP29", 
                                                                     "PCIP40", 
                                                                     "PCIP41", 
                                                                     "PCIP51",
                                                                     "PCIP52")))) # Sums % STEM program

scorecard_f <- scorecard %>%
  inner_join(id_name_link, by = c("UNITID" = "unitid", "OPEID" = "opeid")) %>%
  inner_join(trends, by = "schname") %>% subset(select = -schname)

View(scorecard_f)

View(scorecard_f %>% filter(`med_earnings_10yrs` > 80000) %>% arrange(desc(`med_earnings_10yrs`)))

m1 <- scorecard_f %>% feols(ind_z_shift ~ high_earning)
m2 <- scorecard_f %>% feols(ind_z_shift ~ high_earning + CONTROL)
m3 <- scorecard_f %>% feols(ind_z_shift ~ high_earning | CONTROL)
m4 <- scorecard_f %>% feols(ind_z_shift ~ high_earning | LOCALE)
m5 <- scorecard_f %>% feols(ind_z_shift ~ high_earning + UG25abv)
m6 <- scorecard_f %>% feols(ind_z_shift ~ high_earning + UG25abv | CONTROL)
m7 <- scorecard_f %>% feols(ind_z_shift ~ high_earning + prgm_prct_stem_lg_bus)
m8 <- scorecard_f %>% feols(ind_z_shift ~ high_earning + prgm_prct_stem_lg_bus | CONTROL)
m9 <- scorecard_f %>% feols(ind_z_shift ~ high_earning + SAT_AVG)
m10 <- scorecard_f %>% feols(ind_z_shift ~ high_earning + SAT_AVG | CONTROL)
m11 <-  scorecard_f %>% feols(ind_z_shift ~ high_earning + PCTPELL)
m12 <-  scorecard_f %>% feols(ind_z_shift ~ high_earning + PCTPELL | CONTROL)
m13 <-  scorecard_f %>% feols(ind_z_shift ~ high_earning + PCTFLOAN)
m14 <-  scorecard_f %>% feols(ind_z_shift ~ high_earning + PCTFLOAN | CONTROL)

etable(m1, m2, m3)

wald(m8, "prgm_prct")
scorecard_f %>% ggplot(aes(x = `med_earnings_10yrs`)) + geom_histogram(bins = 100)
scorecard_f %>% ggplot(aes(factor(high_earning), GRAD_DEBT_MDN_SUPP)) + geom_boxplot()
scorecard_f %>% ggplot(aes(factor(high_earning), prgm_prct_stem_lg_bus)) + geom_boxplot()
scorecard_f %>% ggplot(aes(factor(high_earning), SAT_AVG)) + geom_boxplot()
scorecard_f %>% ggplot(aes(prgm_prct_stem_lg_bus, med_earnings_10yrs)) +
  geom_point() + geom_smooth(method = "lm")



library(lubridate)
library(purrr)
library(tidyverse)


## Id link table clean
id_name_link <- read_csv("data/id_name_link.csv")
id_name_link <- id_name_link %>% group_by(schname) %>%
  mutate(n = n()) %>% filter(n == 1) %>%
  subset(select = -n)

## Trends clean and aggregation
trends_vec <- list.files("data/",pattern = "trends_up_to_", full.names = TRUE)

trends <- map_df(trends_vec, read_csv) %>% 
  drop_na()

trends <- trends %>% mutate(sch_dup_f = schname %in% id_name_link$schname) %>% 
  filter(sch_dup_f == TRUE) %>% subset(select = -sch_dup_f) # Removes duplicate school entries

trends <- trends %>% mutate(date_wk_s = str_sub(monthorweek, 1, 10))
trends <- trends %>% mutate(date_wk_s = ymd(date_wk_s))
trends <- trends %>%
  filter( date_wk_s > ymd(20140831) & date_wk_s < ymd(20150328) | date_wk_s > ymd(20150831)) # Compares Sep 14 to Mar 15 and Sep15 to Mar 16
trends <- trends %>% mutate(sc_implmnt = date_wk_s >= ymd(20150901))
trends <- trends %>% group_by(schname, keyword) %>%
  mutate(ind_mean = mean(index, na.rm = TRUE))
trends <- trends %>% group_by(schname, keyword) %>%
  mutate(ind_std = sd(index)) %>% ungroup()

trends %>% ggplot(aes(ind_std)) + geom_histogram(bins = 100) # Check if standardized rankings follow normal distribution

trends <- trends %>% mutate(ind_stand = (index - ind_mean)/ind_std) # Standardize rankings
trends <- trends %>% group_by(schname, sc_implmnt) %>% summarize(ind_z = mean(ind_stand)) # Calculate mean standardized rankings for pre and post scorecard launch
trends <- trends %>% spread(sc_implmnt, ind_z) %>% rename(ind_z_pre = "FALSE", ind_z_post = "TRUE")
trends <- trends %>% mutate(ind_z_shift = ind_z_post - ind_z_pre) # Calculate standardized ranking shift following scorecard launch

## Filter scorecard and create earning category
scorecard <- read_csv("data/Most+Recent+Cohorts+(Scorecard+Elements).csv")
scorecard <- scorecard %>% filter(PREDDEG == 3)
scorecard <- scorecard %>% rename(med_earnings_10yrs = `md_earn_wne_p10-REPORTED-EARNINGS` )

scorecard <- scorecard %>%
  mutate(`med_earnings_10yrs` = as.numeric(`med_earnings_10yrs`)) %>%
  drop_na(`med_earnings_10yrs`)

scorecard <- scorecard %>% mutate(across(c(LOCALE, CONTROL), as.factor))
scorecard$CONTROL <- recode_factor(scorecard$CONTROL,
                                         "1" = "Public",
                                         "2" = "PrivateNonProfit",
                                         "3" = "PrivateForProfit")

scorecard <- scorecard %>% rename(institution_type = CONTROL)
scorecard <- scorecard %>% mutate(across(c(SAT_AVG, UGDS, GRAD_DEBT_MDN_SUPP, UG25abv), as.numeric))
scorecard <- scorecard %>% mutate(across(contains("RET"), as.numeric))
scorecard <- scorecard %>% mutate(across(contains("PCT"), as.numeric))
scorecard <- scorecard %>% mutate(across(contains("PCIP"), as.numeric))
scorecard <- scorecard %>% mutate (prgm_prct_stem = rowSums(across(c("PCIP10",
                                                                     "PCIP11",
                                                                     "PCIP14", 
                                                                     "PCIP15",
                                                                     "PCIP26", 
                                                                     "PCIP27", 
                                                                     "PCIP29", 
                                                                     "PCIP40", 
                                                                     "PCIP41", 
                                                                     "PCIP51")))) # Sums percentage STEM program

scorecard <- scorecard %>% mutate(stem_pri = prgm_prct_stem > 0.5)

scorecard_f <- scorecard %>%
  inner_join(id_name_link, by = c("UNITID" = "unitid", "OPEID" = "opeid")) %>%
  inner_join(trends, by = "schname") %>% subset(select = -schname)

quantile(scorecard_f$`med_earnings_10yrs`)


scorecard_f <- scorecard_f %>% mutate(earn_cat = case_when(`med_earnings_10yrs` >
                                                         quantile(scorecard_f$`med_earnings_10yrs`)[4] ~  "high",
                                                       `med_earnings_10yrs` <
                                                         quantile(scorecard_f$`med_earnings_10yrs`)[2] ~ "low",
                                                       TRUE ~ "mid")) %>% filter(earn_cat != "mid")

scorecard_f <- scorecard_f %>% mutate(earn_cat = case_when(earn_cat == "high" ~ 1,
                                                       TRUE ~ 0)) %>% rename(high_earning = earn_cat)


save(scorecard_f, file =  "processed_data/processed_scorecard.RData")


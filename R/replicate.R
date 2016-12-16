# hannah's analysis, but in R!

# load data ####

# load packages 
library(tidyverse)
library(lubridate)
library(stringr)

# load helper functions
source("R/function_help.R")

# read data
hi_data <- read_csv("data/hi_data.csv")

# clean data ####

# convert colnames to lowercase
colnames(hi_data) <- col_lower(hi_data)

# get rid of redundant codes
# convert date columns to date format
# calculate length of cert (if calculable) 
# remove certain codes
hi_clean <- hi_data %>% 
  mutate(cred_cde = str_replace_all(cred_cde, "MHIF", "MHI"),
         cred_cde = str_replace_all(cred_cde, "PHIF", "PHI"),
         cred_cde = str_replace_all(cred_cde, "PSDF", "PSD"),
         effect_date = mdy(effec_dt),
         exp_date = mdy(exp_dt),
         cert_length = (exp_date - effect_date) / 365) %>% 
  filter(cred_cde != "AJHI") %>%
  filter(cred_cde != "B02") %>% 
  filter(cred_cde != "B05") %>% 
  filter(cred_cde != "B10") %>% 
  filter(cred_cde != "C02") %>% 
  filter(cred_cde != "MSD") %>% 
  filter(cred_cde != "MSDF") %>% 
  filter(cred_cde != "MSDI") %>% 
  filter(cred_cde != "PSD") %>% 
  filter(cred_cde != "SSMF") %>% 
  filter(cred_cde != "TECD") %>% 
  filter(cred_cde != "WCDI") 

# filter out rows w/ NA for start, end, and life_exp
# filter out rows w/ no life_exp code and exp date before 1994
hi_slim <- hi_clean %>% 
  select(psn_id, cred_cde, effect_date, exp_date, life_exp_cde) %>%
  filter(!(is.na(effect_date) & is.na(exp_date) & is.na(life_exp_cde))) %>%
  filter(!(is.na(life_exp_cde) & exp_date < mdy("7/1/1994")))

# look at how many rows have NA's for all three data columns
na_slim <- hi_clean %>% 
  select(psn_id, cred_cde, effect_date, exp_date, life_exp_cde) %>%
  filter((is.na(effect_date) & is.na(exp_date) & is.na(life_exp_cde))) 

# look at how many rows don't have a start date
na_start <- hi_slim %>%
  filter(is.na(effect_date))

# look at how many rows don't have a end date
na_end <- hi_slim %>% 
  filter(is.na(exp_date))


# find certs active in 2004-2016 ####

# there's definitely a faster way to do this w/ map() or nest()...
# for now, this is the verbose method
cert16 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2016,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2016))
cert15 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2015,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2015))


cert14 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2014,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2014))


cert13 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2013,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2013))


cert12 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2012,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2012))


cert11 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2011,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2011))


cert10 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2010,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2010))


cert09 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2009,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2009))

cert08 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2008,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2008))


cert07 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2007,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2007))


cert06 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2006,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2006))


cert05 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2005,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2005))


cert04 <- hi_slim %>% 
  group_by(row_number()) %>% 
  mutate(yr = 2004,
         cert = cert_during_sy(effect_date = effect_date, 
                               end_date = exp_date, 
                               life_exp_cde = life_exp_cde,
                               school_year = 2004))

# join data #####
cert_data <- bind_rows(cert04, cert05, cert06, cert07, cert08, cert09, cert10,
                       cert11, cert12, cert13, cert14, cert15, cert16)

rm(cert04, cert05, cert06, cert07, cert08, cert09, cert10,
   cert11, cert12, cert13, cert14, cert15, cert16)


# summarise data ####

cert_summary <- cert_data %>% 
  group_by(yr, cred_cde) %>% 
  summarise(n_certs = sum(cert))

# plot data ####

cert_summary %>% 
  ggplot(aes(x = yr, y = n_certs, fill = cred_cde)) +
  geom_bar(stat = "identity")



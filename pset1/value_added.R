# title:    value_added.R
# updated:  4/17/21

#========================================================================#
# set up
#========================================================================#

# install packages
library(dplyr)
library(knitr)
library(data.table)
library(tidyr)
library(tidyverse)

# set directory
ddir <- "/Users/fernramoutar/Dropbox/classes/year-2/spring/education/ps1"
setwd(ddir)

#========================================================================#
# residualization of test scores
#========================================================================#

# get N, K, C
df <- read.csv("va-data-updated.csv") %>%
  mutate(C = n_distinct(id_school,id_grade,year,id_class,id_teacher)) %>%
  mutate(test = 1) %>%
  mutate(N = sum(test)) %>%
  mutate(K = 11) %>%
  group_by(id_school,id_grade,year,id_class,id_teacher) %>% # compute within class variance
  mutate(class_resid_mean = mean(resid)) %>%
  mutate(class_resid_mse = ((resid - class_resid_mean)^2)*((N-1)/(N-K-C+1))) %>%
  ungroup() %>%
  mutate(input_var = var(resid, na.rm=TRUE)) %>% # compute total variance
  mutate(resid_var = input_var*((N-1)/(N-K))) 

# autocovariances

transition_df %>% df
  select(id_school,id_grade,year,id_class,id_teacher,class_resid_mean,C,N,K,) 

class_year <- unique(transition_df) %>% # collapse to teacher-year level
  drop_na(class_resid_mean) %>%
  spread(year, class_resid_mean) %>% # reshape to one obs. per teacher ???
  rename(crm07 = '2007') %>%
  rename(crm06 = '2006') %>%
  rename(crm05 = '2005') %>%
  rename(crm04 = '2004') %>%
  rename(crm03 = '2003') %>%
  rename(crm02 = '2002') %>%
  rename(crm01 = '2001') 

model <- lm(crm07 ~ crm06 + crm05 + crm04 + crm03 + crm02 + crm01, class_year) # ols of A_t on A_j^{-t} ???




  
  
  
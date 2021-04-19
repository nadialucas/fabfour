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
#ddir <- "/Users/fernramoutar/Dropbox/classes/year-2/spring/education/ps1"
ddir <- "/Users/nadialucas/Documents/fabfour/pset1/va-data.csv"
setwd(ddir)

#========================================================================#
# residualization of test scores
#========================================================================#

# get N, K, C
df <- read.csv("va-data-updated.csv") %>%
  mutate(C = n_distinct(id_school,id_grade,year,id_class,id_teacher)) %>%
  mutate(test = 1) %>%
  mutate(N = sum(test)) %>%
  mutate(K = 34) %>%
  group_by(id_school,id_grade,year,id_class,id_teacher) %>% # compute within class variance
  mutate(class_resid_mean = mean(resid, na.rm = TRUE)) %>%
  mutate(class_resid_mse = ((resid - class_resid_mean)^2)/(N-K-C+1)) %>%
  ungroup() %>%
  mutate(input_var = var(resid, na.rm=TRUE)) %>% # compute total variance
  mutate(resid_var = input_var*((N-1)/(N-K))) 

# autocovariances

transition_df <- df %>%
  select(id_school,id_grade,year,id_class,id_teacher,class_resid_mean,C,N,K,class_size) 

class_year <- unique(transition_df) %>% # collapse to teacher-year level
  drop_na(class_resid_mean) %>%
  arrange(id_teacher, year) %>%
  mutate(crm_lag1 = ifelse(lag(id_teacher, n=1, na.pad=TRUE) == id_teacher & lag(year, n=1, na.pad=TRUE) == year-1, lag(class_resid_mean, n=1, na.pad = TRUE), NA)) %>%
  mutate(crm_lag2 = ifelse(lag(id_teacher, n=2, na.pad=TRUE) == id_teacher & lag(year, n=2, na.pad=TRUE) == year-2, lag(class_resid_mean, n=2, na.pad = TRUE), NA)) %>%
  mutate(crm_lag3 = ifelse(lag(id_teacher, n=3, na.pad=TRUE) == id_teacher & lag(year, n=3, na.pad=TRUE) == year-3, lag(class_resid_mean, n=3, na.pad = TRUE), NA)) %>%
  mutate(crm_lag4 = ifelse(lag(id_teacher, n=4, na.pad=TRUE) == id_teacher & lag(year, n=4, na.pad=TRUE) == year-4, lag(class_resid_mean, n=4, na.pad = TRUE), NA)) %>%
  mutate(crm_lag5 = ifelse(lag(id_teacher, n=5, na.pad=TRUE) == id_teacher & lag(year, n=5, na.pad=TRUE) == year-5, lag(class_resid_mean, n=5, na.pad = TRUE), NA)) %>%
  mutate(crm_lag6 = ifelse(lag(id_teacher, n=6, na.pad=TRUE) == id_teacher & lag(year, n=6, na.pad=TRUE) == year-6, lag(class_resid_mean, n=6, na.pad = TRUE), NA)) 


model <- lm(class_resid_mean ~ crm_lag1 + crm_lag2 + crm_lag3 + crm_lag4 + crm_lag5 + crm_lag6, class_year, weights = class_size) # ols of A_t on A_j^{-t} ???

model <- lm(class_resid_mean ~ crm_lag2 -1, class_year, weights = class_size) # ols of A_t on A_j^{-t} ???



cor1 <- cor.test(class_year$class_resid_mean, class_year$crm_lag1, method="pearson", na.rm = TRUE)
cor2 <- cor.test(class_year$class_resid_mean, class_year$crm_lag2, method="pearson", na.rm = TRUE)
cor3 <- cor.test(class_year$class_resid_mean, class_year$crm_lag3, method="pearson", na.rm = TRUE)
cor4 <- cor.test(class_year$class_resid_mean, class_year$crm_lag4, method="pearson", na.rm = TRUE)
cor5 <- cor.test(class_year$class_resid_mean, class_year$crm_lag5, method="pearson", na.rm = TRUE)
cor6 <- cor.test(class_year$class_resid_mean, class_year$crm_lag6, method="pearson", na.rm = TRUE)  
  
  
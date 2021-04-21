# title:    value_added_step3.R
# updated:  4/20/21
# Nadia Lucas, Sid Sah, Fern Ramoutar, and George Vojta
# We would like to thank Tanya Rajan for helpful comments

#========================================================================#
# set up
#========================================================================#

# install packages
library(dplyr)
library(knitr)
library(data.table)
library(tidyr)
library(tidyverse)

rm(list = ls())

# set directory
#ddir <- "/Users/fernramoutar/Dropbox/classes/year-2/spring/education/ps1"
ddir <- "/Users/nadialucas/Documents/fabfour/pset1"
setwd(ddir)

# from Stata we have all the autocovariances and variances calculated so we read
# those in
var_matrix <- read.csv("var-matrix.csv") %>%
  mutate(vars = substr(c1, start = 4, stop = str_length(c1)-2)) %>%
  mutate(vars = as.numeric(vars)) %>%
  select(vars)

# dataframe at the teacher-year level
teacher_df <- read.csv("va-class-level.csv")
# dataframe at the individual student level
student_df <- read.csv("va-data-updated3.csv")

#========================================================================#
# use the variances to construct Sigma and gamma
#========================================================================#

# for each teacher construct a sigma 
sigma_A <- var_matrix[1,1]
sigma_eps <- var_matrix[2,1]
sigma_theta <- var_matrix[3,1]
sig1 <- var_matrix[4,1]
sig2 <- var_matrix[5,1]
sig3 <- var_matrix[6,1]
sig4 <- var_matrix[7,1]
sig5 <- var_matrix[8,1]
sig6 <- var_matrix[9,1]

gamma <- c(sig1, sig2, sig3, sig4, sig5, sig6)
mu_list <- c()
max_lag = 6
A_lags = c("res_lag1", "res_lag2", "res_lag3", "res_lag4", "res_lag5", "res_lag6")
# also get rid of all rows with no lags at all - wont go into tva estimates
teacher_df_wlags <- teacher_df[rowSums(is.na(teacher_df[A_lags]))<max_lag, ]
# construct Sigma as per the appendix
Sigma <- matrix(0, nrow = max_lag, ncol = max_lag)
for (i in 1:max_lag) {
  for (j in i:max_lag) {
    if (j==i) {
      Sigma[i,i] = sigma_theta
    }
    else {
      spread = j-i
      Sigma[i,j] = gamma[spread]
      Sigma[j,i] = gamma[spread]
    }
  }
}
#========================================================================#
# function to apply the tv estimate step to our dataframe
#========================================================================#
# we want to go row by row and construct the
# correctly sized Sigma and gamma matrix
# depending on how many lags are available                
value_added <- function(x){
  # get relevant lags
  mask = !is.na(x[A_lags])
  Sig = Sigma[mask==TRUE, mask==TRUE]
  gam = gamma[mask==TRUE]
  # get class size weights
  nct <- x["class_size"]
  # get the vector of As for drift
  Avec <-x[A_lags]
  Avec = Avec[!is.na(Avec)]
  if (length(gam) == 1) {
    Sig ++ sigma_eps/nct
    psi = gam/Sig
  }
  # do the final adjustment that depends on class size
  else {
    weights <- diag(rep(sigma_eps/nct, length(gam)))
    Sig = Sig++weights
    psi <- solve(Sig, gam)
  }
  
  return(t(psi)%*%Avec)
}

tv <- apply(teacher_df_wlags, 1, value_added)

teacher_df_wlags$tv <- tv
#========================================================================#
# merge back in with student-level data to check bias
#========================================================================#
student_teacher <- merge(student_df, teacher_df_wlags, by = c("id_teacher", "year"), all.x = T)

# renormalize the tvs to the same distribution as individual scores for the regression
tv_mean <- mean(teacher_df_wlags$tv, na.rm = TRUE)
tv_sd <- sd(teacher_df_wlags$tv, na.rm=TRUE)

# densit histogram
plot(density(tv-tv_mean, bw=.01), main = "Density of tv estimates")
score_sd <- sd(student_teacher$res, na.rm=TRUE)
student_teacher <- student_teacher %>%
  mutate(tv_normed = (tv - tv_mean) * (score_sd/tv_sd))

# This should give us estimates of the bias
lm(score_std ~ tv_normed -1 , data = student_teacher)
# close to 0 though so our predictive power is off

#========================================================================#
# I attempt to estimate teacher value added without any drift
#========================================================================#
teacher_df_nodrift <- teacher_df_wlags %>%
  drop_na(class_mean)
# try one with no drift
teacher_df_nodrift <- teacher_df_nodrift %>%
  group_by(id_teacher) %>%
  rowwise %>%
  mutate(A_mean =sum(res_lag1,res_lag2,res_lag3,res_lag4,res_lag5,res_lag6, na.rm=TRUE)/(year-2000)) %>%
  ungroup()

model2 = lm(class_mean ~ A_mean, data = teacher_df_nodrift )
tv_nodrift = predict(model2)
# should give estimates of tv with no drift
teacher_df_nodrift$tv_nodrift = tv_nodrift
# merge back in with student level
student_teacher_nodrift <- merge(student_df, teacher_df_nodrift, by = c("id_teacher", "year"), all.x = T)

tvnodrift_mean <- mean(teacher_df_nodrift$tv_nodrift, na.rm = TRUE)
tvnodrift_sd <- sd(teacher_df_nodrift$tv_nodrift, na.rm=TRUE)
score_sd <- sd(student_teacher_nodrift$res, na.rm=TRUE)
plot(density(tv_nodrift-tvnodrift_mean, bw=.01), main = "Density of tv estimates no drift")

# renormalize the tvs to the same distribution as individual scores for the regression
student_teacher_nodrift <- student_teacher_nodrift %>%
  mutate(tvnodrift_normed = (tv_nodrift - tvnodrift_mean) * (score_sd/tvnodrift_sd))

# This should give us estimates of the bias
lm(score_std ~ tvnodrift_normed -1 , data = student_teacher_nodrift)
# ends up being very low
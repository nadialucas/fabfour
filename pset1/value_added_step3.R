# title:    value_added_step3.R
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
ddir <- "/Users/nadialucas/Documents/fabfour/pset1"
setwd(ddir)

var_matrix <- read.csv("var-matrix.csv", stringsAsFactors=TRUE)

# currently this step is broken 
teacher_df <- read.csv("va-class-level.csv")

#========================================================================#
# use the variances to construct Sigma and gamma
#========================================================================#

# for each teacher construct a sigma 
sigma_A <- var_matrix[1]
sigma_eps <- var_matrix[2]
sigma_theta <- var_matrix[3]
sig1 <- var_matrix[4]
sig2 <- var_matrix[5]
sig3 <- var_matrix[6]
sig4 <- var_matrix[7]
sig5 <- var_matrix[8]
sig6 <- var_matrix[9]

Cov_list <- c(0, sig1, sig2, sig3, sig4, sig5, sig6)

teacher_ids <- unique(teacher_df$id_teacher)
for (id in teacher_ids) {
  tiny_dataset <- teacher_df %>%
    filter(id_teacher == id)
  Njt <- nrow(tiny_dataset)
  Sigma <- matrix(0, nrow = Njt, ncol = Njt)
  gamma <- matrix(0, nrow = Njt)
  for (i in 1:Njt) {
    nct = tiny_dataset[i,]$class_size
    class_size = row$
    gamma[i] = Cov_list[i]
    for (j in 1:Njt) {
      spread = j-i
      Sigma[i][j] = Cov_list[spread]
      if (i==j) {
        var_adjust = sigma_theta + (sigma_eps/nct)
        Sigma[i][i] ++ var_adjust
      }
    }
  }
}
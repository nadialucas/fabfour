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

var_matrix <- read.csv("var-matrix.csv")

teacher_df <- read.csv("va-class-level.csv")

#========================================================================#
# use the variances to construct Sigma and gamma
#========================================================================#

# for each teacher construct a sigma 

teacher_ids <- unique(teacher_df$id_teacher)
for (id in teacher_ids) {
  print(id)
}
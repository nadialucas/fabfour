********************************************************************************
* title:    value_added_step3.R
* updated:  4/20/21
* Nadia Lucas, Sid Sah, Fern Ramoutar, and George Vojta
* We would like to thank Tanya Rajan for helpful comments
********************************************************************************

clear all
set more off

* change for your own directory
global workingdir "/Users/nadialucas/Documents/fabfour/pset1"
import delimited using "$workingdir/va-data.csv"


egen class_id = group(id_class id_school year id_grade)
egen school_grade = group(id_school year id_grade)
egen school_year = group(id_school year)

egen class_lag_mean = mean(lag_score_std), by(class_id)
egen school_grade_lag_mean = mean(lag_score_std), by(school_grade)

* cubic in various lag scores
gen lag_scores_2 = lag_score_std^2
gen lag_scores_3 = lag_score_std^3
gen class_lag_mean2 = class_lag_mean^2
gen class_lag_mean3 = class_lag_mean^3
gen school_grade_lag_mean2 = school_grade_lag_mean^2
gen school_grade_lag_mean3 = school_grade_lag_mean^3

* class and school-year level individual covariates
egen class_hh = mean(hh_income), by(class_id)
egen class_m_edu = mean(m_education), by(class_id)
egen school_hh = mean(hh_income), by(school_year)
egen school_m_edu = mean(m_education), by(school_year)

* class size
egen class_size = count(class_id), by(class_id)


ren id_grade grade

* regression that accounts for all the covariates
reghdfe score_std lag_score_std lag_scores_2 lag_scores_3 ///
c.lag_score_std#i.grade c.lag_scores_2#i.grade c.lag_scores_3#i.grade ///
c.class_lag_mean#i.grade c.class_lag_mean2#i.grade c.class_lag_mean3#i.grade ///
c.school_grade_lag_mean#i.grade c.school_grade_lag_mean2#i.grade c.school_grade_lag_mean3#i.grade ///
class_hh class_m_edu school_hh school_m_edu i.grade i.year class_size, absorb(teacher_fes = i.id_teacher) resid(residual) nocons

gen res = residual + teacher_fes

* use residual to predict autocovariance of teachers

* first get N, K, and C for the corrections
egen students = group(id_student)
sum students
local N = r(max)

egen classes = group(class_id)
sum classes
local C = r(max)

* counted all the covariates
local K = 57


* generate sigmas

sum res 
global sigma_A = r(Var) * (`N' - 1)/(`N' - `K')


egen class_mean = mean(res), by(class_id)
gen ind_dev = res - class_mean
sum ind_dev
global sigma_epsilon = r(Var) * (`N' - 1)/(`N' - `K' - `C' + 1)

global sigma_theta = ${sigma_A} - ${sigma_epsilon}

export delimited using "$workingdir/va-data-updated3.csv", replace

* do the time-series

collapse (mean) class_hh class_m_edu class_lag_mean class_size grade school_hh school_m_edu class_mean id_teacher, by(class_id year)

tsset id_teacher year
sort id_teacher year

* generate all 6 lag variables and corresponding weights - probably could have used a loop rip
gen res_lag1 = L1.class_mean
gen res_lag2 = L2.class_mean
gen res_lag3 = L3.class_mean
gen res_lag4 = L4.class_mean
gen res_lag5 = L5.class_mean
gen res_lag6 = L6.class_mean

gen weight1 = class_size + L1.class_size
gen weight2 = class_size + L2.class_size
gen weight3 = class_size + L3.class_size
gen weight4 = class_size + L4.class_size
gen weight5 = class_size + L5.class_size
gen weight6 = class_size + L6.class_size

* get autorrelations and spit out to a matrix 
matrix variances = J(9,1,.)
matrix variances[1,1] = $sigma_A
matrix variances[2,1] = $sigma_epsilon
matrix variances[3,1] = $sigma_theta

corr class_mean res_lag1 [aw=weight1], cov
matrix variances[4,1] = r(cov_12)
corr class_mean res_lag2 [aw=weight2], cov
matrix variances[5,1] = r(cov_12)
corr class_mean res_lag3 [aw=weight3], cov
matrix variances[6,1] = r(cov_12)
corr class_mean res_lag4 [aw=weight4], cov
matrix variances[7,1] = r(cov_12)
corr class_mean res_lag5 [aw=weight5], cov
matrix variances[8,1] = r(cov_12)
corr class_mean res_lag6 [aw=weight6], cov
matrix variances[9,1] = r(cov_12)

matrix list variances

export delimited using "$workingdir/va-class-level.csv", replace

* we use this to read into R for step 3
mat2txt, matrix(variances) saving("$workingdir/var-matrix.csv") replace




clear all
set more off

import delimited using "/Users/nadialucas/Documents/fabfour/pset1/va-data.csv"


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



reghdfe score_std i.class_id c.lag_score_std#i.grade c.lag_scores_2#i.grade c.lag_scores_3#i.grade ///
c.class_lag_mean#i.grade c.class_lag_mean2#i.grade c.class_lag_mean3#i.grade ///
c.school_grade_lag_mean#i.grade c.school_grade_lag_mean2#i.grade c.school_grade_lag_mean3#i.grade ///
class_hh class_m_edu school_hh school_m_edu i.grade i.year class_size, absorb(teacher_fes = i.id_teacher) resid(residual)

gen res = residual + teacher_fes

* use residual to predict autocovariance of teachers

* first get N, K, and C for the corrections
egen students = group(id_student)
sum students
global N = r(max)

egen classes = group(class_id)
sum classes
global C = r(max)



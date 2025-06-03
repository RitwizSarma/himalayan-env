*GMM on Stata: Trial by Riju
clear
 cd /Users/rijugarg/Documents/GitHub/himalayan-env/
//cd "C:\LargeFiles\Research\himalayan_env\himalayan-env"

* EXPLORATORY ANALYSIS
use "DATA/DTA/DATA.dta"
summarize vcfd pm25 lst_median, detail

* INDEX BUILDING
egen std_vcfd = std(vcfd)
egen std_pm25 = std(pm25)
egen std_lst_m = std(lst_median)

* Complete index
pca std_vcfd std_pm25 std_lst_m
ereturn list
matrix loadings = e(L)
matrix list loadings
local w1 = loadings[1,1]
local w2 = loadings[2,1]
local w3 = loadings[3,1]
generate index_env_pca = `w1'*std_vcfd + `w2'*std_pm25 + `w3'*std_lst_m
summarize index_env_pca //PCA generated env index full

* VCF,PM25 indices
pca std_vcfd std_pm25
ereturn list
matrix loadings = e(L)
matrix list loadings
local W1 = loadings[1,1]
local W2 = loadings[2,1]
generate index_env_model2 = `W1'*std_vcfd + `W2'*std_pm25 //PCA generated index for model 2
generate index_envm_uw = (std_vcfd + std_pm25)/2 //uniweighted env index

* NTL,GHSL indices
egen std_NTL = std(ntl_mean)
egen std_GHSL = std(ghsl_mean)
generate index_econ_uw = (std_NTL + std_GHSL)/2 //Uniweight econ index
generate index_econ_uw_sq= index_econ_uw^2 //uniweight econ index squared
pca std_NTL std_GHSL
ereturn list
matrix loadings = e(L)
matrix list loadings
local m1 = loadings[1,1]
local m2 = loadings[2,1]
generate index_econ_pca = `m1'*std_NTL + `m2'*std_GHSL //PCA generated econ index_econ
generate index_econ_pca_sq= index_econ_pca^2 //PCA generated econ index squared
summarize index_econ_pca
gen std_NTL_sq= std_NTL^2

*Generating epochs
generate epoch_1 = year if year >= 2000 & year <= 2010
generate epoch_2 = year if year > 2010 & year <= 2020
generate is_epoch1 = year >= 2000 & year <= 2010
generate is_epoch2 = year > 2010 & year <= 2020

*GMM method for pca index (Model 3 according to earlier do file)
xtset xtid year 
 gmm (index_env_pca - {b0} - {b1}*index_econ_pca - {b2}*index_econ_pca_sq), instruments(index_econ_pca index_econ_pca_sq)
 esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_pca_full.doc" , replace r2 ar2 se label
 //esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_pca_full.tex" , replace r2 ar2 se label
 matrix list e(b)
scalar b0 = e(b)[1,1]
scalar b1 = e(b)[1,2]
scalar b2 = e(b)[1,3]
gen fitted = b0 + b1*index_econ_pca + b2*index_econ_pca_sq
twoway (scatter index_env_pca index_econ_pca, mcolor(gs10)) (line fitted index_econ_pca, lcolor(blue)), title("Kuznets Curve: Actual vs Fitted") legend(order(1 "Actual" 2 "Fitted"))
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata_GMM_trials/PCA_full.png"
scalar turning_point = -b1 / (2*b2)
di turning_point
 
xtset xtid epoch_1
gmm (index_env_pca - {b0} - {b1}*index_econ_pca - {b2}*index_econ_pca_sq) if is_epoch1 == 1, instruments(index_econ_pca index_econ_pca_sq)
esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_pca_epoch1.doc" , replace r2 ar2 se label
//esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_pca_epoch1.tex" , replace r2 ar2 se label
matrix list e(b)
scalar b0_e1 = e(b)[1,1]
scalar b1_e1 = e(b)[1,2]
scalar b2_e1 = e(b)[1,3]
gen fitted1 = b0_e1 + b1_e1*index_econ_pca + b2_e1*index_econ_pca_sq
twoway (line fitted1 index_econ_pca, lcolor(blue)), title("Kuznets Curve: Epoch 1")
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata_GMM_trials/PCA_epoch1.png"
scalar turning_point1 = -b1_e1 / (2*b2_e1)
di turning_point1
 
xtset xtid epoch_2
gmm (index_env_pca - {b0} - {b1}*index_econ_pca - {b2}*index_econ_pca_sq) if is_epoch2 == 1, instruments(index_econ_pca index_econ_pca_sq)
esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_pca_epoch2.doc" , replace r2 ar2 se label
//esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_pca_epoch2.tex" , replace r2 ar2 se label
matrix list e(b)
scalar b0_e2 = e(b)[1,1]
scalar b1_e2 = e(b)[1,2]
scalar b2_e2 = e(b)[1,3]
gen fitted2 = b0_e2 + b1_e2*index_econ_pca + b2_e2*index_econ_pca_sq
twoway (line fitted2 index_econ_pca, lcolor(blue)), title("Kuznets Curve: Epoch 2")
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata_GMM_trials/PCA_epoch2.png"
scalar turning_point2 = -b1_e2 / (2*b2_e2)
di turning_point2

*GMM method for uniweight index
xtset xtid year 
 gmm (index_envm_uw - {b0} - {b1}*index_econ_uw - {b2}*index_econ_uw_sq), instruments(index_econ_uw index_econ_uw_sq)
 esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_uw_full.doc" , replace r2 ar2 se label
 //esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_uw_full.tex" , replace r2 ar2 se label
 matrix list e(b)
scalar b0_uw = e(b)[1,1]
scalar b1_uw = e(b)[1,2]
scalar b2_uw = e(b)[1,3]
gen fitted_uw = b0_uw + b1_uw*index_econ_uw + b2_uw*index_econ_uw_sq
twoway (scatter index_envm_uw index_econ_uw, mcolor(gs10)) (line fitted_uw index_econ_uw, lcolor(blue)), title("Kuznets Curve: Actual vs Fitted") legend(order(1 "Actual" 2 "Fitted"))
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata_GMM_trials/UW_full.png"
scalar turning_point_uw = -b1_uw / (2*b2_uw)
di turning_point_uw
 
xtset xtid epoch_1
gmm (index_envm_uw - {b0} - {b1}*index_econ_uw- {b2}*index_econ_uw_sq) if is_epoch1 == 1, instruments(index_econ_uw index_econ_uw_sq)
esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_uw_epoch1.doc" , replace r2 ar2 se label
//esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_uw_epoch1.tex" , replace r2 ar2 se label
matrix list e(b)
scalar b0_e1_uw = e(b)[1,1]
scalar b1_e1_uw = e(b)[1,2]
scalar b2_e1_uw = e(b)[1,3]
gen fitted1_uw = b0_e1_uw + b1_e1_uw*index_econ_uw + b2_e1_uw*index_econ_uw_sq
twoway (line fitted1_uw index_econ_uw, lcolor(blue)), title("Kuznets Curve: Epoch 1(UW)")
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata_GMM_trials/UW_epoch1.png"
scalar turning_point1_uw = -b1_e1_uw / (2*b2_e1_uw)
di turning_point1_uw
 
xtset xtid epoch_2
gmm (index_envm_uw - {b0} - {b1}*index_econ_uw- {b2}*index_econ_uw_sq) if is_epoch2 == 1, instruments(index_econ_uw index_econ_uw_sq)
esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_uw_epoch2.doc" , replace r2 ar2 se label
//esttab using "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/Results_GMM_trial_stata/gmm_uw_epoch2.tex" , replace r2 ar2 se label
matrix list e(b)
scalar b0_e2_uw = e(b)[1,1]
scalar b1_e2_uw = e(b)[1,2]
scalar b2_e2_uw = e(b)[1,3]
gen fitted2_uw = b0_e2_uw + b1_e2_uw*index_econ_uw + b2_e2_uw*index_econ_uw_sq
twoway (line fitted2_uw index_econ_uw, lcolor(blue)), title("Kuznets Curve: Epoch 2(UW)")
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata_GMM_trials/UW_epoch2.png"
scalar turning_point2_uw = -b1_e2_uw / (2*b2_e2_uw)
di turning_point2_uw
 
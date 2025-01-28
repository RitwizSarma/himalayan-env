clear
use "/Users/rijugarg/Documents/GitHub/himalayan-env/DATA/DTA/DATA.dta"
summarize vcfd pm25 lst_median, detail
egen std_vcfd = std(vcfd)
egen std_pm25 = std(pm25)
egen std_lst_m = std(lst_median) 
pca std_vcfd std_pm25 std_lst_m
matrix loadings = e(r)
matrix list loadings
pca std_vcfd std_pm25 std_lst_m
ereturn list
matrix loadings = e(L)
matrix list loadings
local w1 = loadings[1,1]
local w2 = loadings[2,1]
local w3 = loadings[3,1]
generate index_env = `w1'*std_vcfd + `w2'*std_pm25 + `w3'*std_lst_m
summarize index_env //PCA generated env index full
gen sq_index_env_full= index_env^2
pca std_vcfd std_pm25 
screeplot
pca std_vcfd std_pm25 
matrix loadings = e(r)
matrix list loadings
pca std_vcfd std_pm25
ereturn list
matrix loadings = e(L)
matrix list loadings
local W1 = loadings[1,1]
local W2 = loadings[2,1]
generate index_env_model2 = `W1'*std_vcfd + `w2'*std_pm25 //PCA generated index for model 2
generate index_model2_sq_pca = index_env_model2^2
generate index_envm_uw = (std_vcfd + std_pm25)/2
generate index_envm_uw_sq = index_envm_uw^2
egen std_NTL = std(ntl_mean)
egen std_GHSL = std(ghsl_mean)
generate index_econ_uw = (std_NTL + std_GHSL)/2
pca std_NTL std_GHSL 
screeplot
pca std_NTL std_GHSL 
matrix loadings = e(r)
matrix list loadings
pca std_NTL std_GHSL
ereturn list
matrix loadings = e(L)
matrix list loadings
local m1 = loadings[1,1]
local m2 = loadings[2,1]
generate index_econ_pca = `m1'*std_NTL + `m2'*std_GHSL //PCA generated econ index_econ
summarize index_econ_pca
xtset xtid year 
xtreg index_econ_pca index_env sq_index, fe vce(r) // For 20 years
predict fitted_values, xb
twoway (line fitted_values index_env), name(Graph_20years, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphfull.png", as(png) name("Graph_20years")
xtset xtid year 
xtreg std_NTL index_env sq_index, fe vce(r) // For 20 years
predict fit_values, xb
twoway (line fit_values index_env), name(Graph_2, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graph_full.png", as(png) name("Graph_2")
generate epoch_1 = year if year >= 2000 & year <= 2010
generate epoch_2 = year if year > 2010 & year <= 2020
xtset xtid epoch_1 
xtreg std_NTL index_env sq_index, fe vce(r) // Model 1(1) (from notes)
predict fit_values_epoch1, xb
twoway (line fit_values_epoch1 index_env), name(Graph_M1_1, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM1_1.png", as(png) name("Graph_M1_1")
xtset xtid epoch_1
xtreg std_NTL index_env_model2 index_model2_sq_pca, fe vce(r) // Model 1 (2)
predict fit_values_M1_2, xb
twoway (line fit_values_M1_2 index_env_model2), name(Graph_M1_2, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM1_2.png", as(png) name("Graph_M1_2")
xtset xtid epoch_1
xtreg std_NTL index_envm_uw index_envm_uw_sq, fe vce(r) // Model 1 (3)
predict fit_values_M1_3, xb
twoway (line fit_values_M1_3 index_envm_uw), name(Graph_M1_3, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM1_3.png", as(png) name("Graph_M1_3")
xtset xtid epoch_2
xtreg std_NTL index_env sq_index, fe vce(r) //Model 1(4)
predict fit_values_M1_4, xb
twoway (line fit_values_M1_4 index_envm_uw), name(Graph_M1_4, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM1_4.png", as(png) name("Graph_M1_4")
xtset xtid epoch_2 
xtreg std_NTL index_env_model2 index_model2_sq_pca, fe vce(r) // Model 1(5) (from notes)
predict fit_values_M1_5, xb
twoway (line fit_values_M1_5 index_env_model2), name(Graph_M1_5, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM1_5.png", as(png) name("Graph_M1_5")
xtset xtid epoch_2 
xtreg std_NTL index_envm_uw index_envm_uw_sq, fe vce(r) // Model 1(6) (from notes)
predict fit_values_M1_6, xb
twoway (line fit_values_M1_6 index_envm_uw), name(Graph_M1_6, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM1_6.png", as(png) name("Graph_M1_6")
xtset xtid epoch_1 
xtreg index_econ_pca index_env_model2 index_model2_sq_pca, fe vce(r) // Model 2(1) LHS PCA
predict fit_values_M2_1, xb
twoway (line fit_values_M2_1 index_env_model2), name(Graph_M2_1, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM2_1.png", as(png) name("Graph_M2_1")
xtset xtid epoch_1 
xtreg index_econ_pca index_envm_uw index_envm_uw_sq, fe vce(r) // Model 2(2) LHS PCA
predict fit_values_M2_2, xb
twoway (line fit_values_M2_2 index_envm_uw), name(Graph_M2_2, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM2_2.png", as(png) name("Graph_M2_2")
xtset xtid epoch_1 
xtreg index_econ_uw index_env_model2 index_model2_sq_pca, fe vce(r) // Model 2(3) LHS UW
predict fit_values_M2_3, xb
twoway (line fit_values_M2_3 index_env_model2), name(Graph_M2_3, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM2_3.png", as(png) name("Graph_M2_3")
xtset xtid epoch_1 
xtreg index_econ_uw index_envm_uw index_envm_uw_sq, fe vce(r) // Model 2(4) LHS PCA
predict fit_values_M2_4, xb
twoway (line fit_values_M2_4 index_envm_uw), name(Graph_M2_4, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM2_4.png", as(png) name("Graph_M2_4")
xtset xtid epoch_2 
xtreg index_econ_pca index_env_model2 index_model2_sq_pca, fe vce(r) // Model 2(5) LHS PCA
predict fit_values_M2_5, xb
twoway (line fit_values_M2_5 index_env_model2), name(Graph_M2_5, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM2_5.png", as(png) name("Graph_M2_5")
xtset xtid epoch_2 
xtreg index_econ_pca index_envm_uw index_envm_uw_sq, fe vce(r) // Model 2(6) LHS PCA
predict fit_values_M2_6, xb
twoway (line fit_values_M2_2 index_envm_uw), name(Graph_M2_6, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM2_6.png", as(png) name("Graph_M2_6")
xtset xtid epoch_2
xtreg index_econ_uw index_env_model2 index_model2_sq_pca, fe vce(r) // Model 2(7) LHS UW
predict fit_values_M2_7, xb
twoway (line fit_values_M2_7 index_env_model2), name(Graph_M2_7, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM2_7.png", as(png) name("Graph_M2_7")
xtset xtid epoch_2
xtreg index_econ_uw index_envm_uw index_envm_uw_sq, fe vce(r) // Model 2(8) LHS PCA
predict fit_values_M2_8, xb
twoway (line fit_values_M2_8 index_envm_uw), name(Graph_M2_8, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM2_8.png", as(png) name("Graph_M2_8")
xtset xtid epoch_1
xtreg index_econ_pca index_env sq_index, fe vce(r) // Model 3(1)
predict fit_values_M3_1, xb
twoway (line fit_values_M3_1 index_env), name(Graph_M3_1, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM3_1.png", as(png) name("Graph_M3_1")
xtset xtid epoch_1
xtreg index_econ_uw index_env sq_index, fe vce(r) // Model 3(2)
predict fit_values_M3_2, xb
twoway (line fit_values_M3_2 index_env), name(Graph_M3_2, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM3_2.png", as(png) name("Graph_M3_2")
xtset xtid epoch_2
xtreg index_econ_pca index_env sq_index, fe vce(r) // Model 3(3)
predict fit_values_M3_3, xb
twoway (line fit_values_M3_3 index_env), name(Graph_M3_3, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM3_3.png", as(png) name("Graph_M3_3")
xtset xtid epoch_2
xtreg index_econ_uw index_env sq_index, fe vce(r) // Model 3(4)
predict fit_values_M3_4, xb
twoway (line fit_values_M3_4 index_env), name(Graph_M3_4, replace)
graph export "/Users/rijugarg/Documents/GitHub/himalayan-env/DOC/images/Stata/graphM3_4.png", as(png) name("Graph_M3_4")

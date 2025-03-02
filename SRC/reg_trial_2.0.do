* Analysis. Riju Garg, Madras School of Econ.

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

* REGRESSIONS - 20 YEARS
xtset xtid year 
xtreg index_env_pca index_econ_pca index_econ_pca_sq , fe vce(r) // For 20 years
predict fitted_values, xb
outreg2 using "DOC/Results2.0/results_full.doc", replace word
twoway (line fitted_values index_econ_pca), name(Graph_20years, replace)
graph export "DOC/images/Stata2.0/graphfull.png", as(png) name("Graph_20years")

xtreg index_env_pca  std_NTL std_NTL_sq, fe vce(r) // For 20 years
predict fit_values, xb
outreg2 using "DOC/Results2.0/results_full.doc", append word
twoway (line fit_values std_NTL), name(Graph_2, replace)
graph export "DOC/images/Stata2.0/graph_full.png", as(png) name("Graph_2")

* REGRESSIONS - MODEL 1
generate epoch_1 = year if year >= 2000 & year <= 2010
generate epoch_2 = year if year > 2010 & year <= 2020

//Epoch 1
xtset xtid epoch_1 
xtreg index_env_pca  std_NTL std_NTL_sq , fe vce(r) // Model 1(1) (from notes)
outreg2 using "DOC/Results2.0/results_model1.doc", replace word
predict fit_values_epoch1, xb
twoway (line fit_values_epoch1 std_NTL), name(Graph_M1_1, replace)
graph export "DOC/images/Stata2.0/graphM1_1.png", as(png) name("Graph_M1_1")

xtreg index_env_model2  std_NTL std_NTL_sq, fe vce(r) // Model 1 (2)
outreg2 using "DOC/Results2.0/results_model1.doc", append word
predict fit_values_M1_2, xb
twoway (line fit_values_M1_2 std_NTL), name(Graph_M1_2, replace)
graph export "DOC/images/Stata2.0/graphM1_2.png", as(png) name("Graph_M1_2")

xtreg index_envm_uw  std_NTL std_NTL_sq, fe vce(r) // Model 1 (3)
outreg2 using "DOC/Results2.0/results_model1.doc", append word
predict fit_values_M1_3, xb
twoway (line fit_values_M1_3 std_NTL), name(Graph_M1_3, replace)
graph export "DOC/images/Stata2.0/graphM1_3.png", as(png) name("Graph_M1_3")

//Epoch 2
xtset xtid epoch_2
xtreg index_env_pca  std_NTL std_NTL_sq, fe vce(r) //Model 1(4)
outreg2 using "DOC/Results2.0/results_model1.doc", append word
predict fit_values_M1_4, xb
twoway (line fit_values_M1_4 std_NTL), name(Graph_M1_4, replace)
graph export "DOC/images/Stata2.0/graphM1_4.png", as(png) name("Graph_M1_4")

xtreg index_env_model2  std_NTL std_NTL_sq, fe vce(r) // Model 1(5) (from notes)
outreg2 using "DOC/Results2.0/results_model1.doc", append word
predict fit_values_M1_5, xb
twoway (line fit_values_M1_5 std_NTL), name(Graph_M1_5, replace)
graph export "DOC/images/Stata2.0/graphM1_5.png", as(png) name("Graph_M1_5")

xtreg index_envm_uw  std_NTL std_NTL_sq, fe vce(r) // Model 1(6) (from notes)
outreg2 using "DOC/Results2.0/results_model1.doc", append word
predict fit_values_M1_6, xb
twoway (line fit_values_M1_6 std_NTL), name(Graph_M1_6, replace)
graph export "DOC/images/Stata2.0/graphM1_6.png", as(png) name("Graph_M1_6")

* REGRESSIONS - MODEL 2

//Epoch 1
xtset xtid epoch_1 
xtreg index_env_model2  index_econ_pca index_econ_pca_sq, fe vce(r) // Model 2(1) LHS PCA
outreg2 using "DOC/Results2.0/results_model2.doc", replace word
predict fit_values_M2_1, xb
twoway (line fit_values_M2_1 index_econ_pca), name(Graph_M2_1, replace)
graph export "DOC/images/Stata2.0/graphM2_1.png", as(png) name("Graph_M2_1") 

xtreg index_envm_uw  index_econ_pca index_econ_pca_sq, fe vce(r) // Model 2(2) LHS PCA
outreg2 using "DOC/Results2.0/results_model2.doc", append word
predict fit_values_M2_2, xb
twoway (line fit_values_M2_2 index_econ_pca), name(Graph_M2_2, replace)
graph export "DOC/images/Stata2.0/graphM2_2.png", as(png) name("Graph_M2_2")

xtreg index_env_model2 index_econ_uw index_econ_uw_sq, fe vce(r) // Model 2(3) LHS UW
outreg2 using "DOC/Results2.0/results_model2.doc", append word
predict fit_values_M2_3, xb
twoway (line fit_values_M2_3 index_econ_uw), name(Graph_M2_3, replace)
graph export "DOC/images/Stata2.0/graphM2_3.png", as(png) name("Graph_M2_3")

xtreg index_envm_uw  index_econ_uw  index_econ_uw_sq , fe vce(r) // Model 2(4) LHS PCA
outreg2 using "DOC/Results2.0/results_model2.doc", append word
predict fit_values_M2_4, xb
twoway (line fit_values_M2_4 index_econ_uw), name(Graph_M2_4, replace)
graph export "DOC/images/Stata2.0/graphM2_4.png", as(png) name("Graph_M2_4")

//Epoch 2
xtset xtid epoch_2 
xtreg index_env_model2 index_econ_pca  index_econ_pca_sq, fe vce(r) // Model 2(5) LHS PCA
outreg2 using "DOC/Results2.0/results_model2.doc", append word
predict fit_values_M2_5, xb
twoway (line fit_values_M2_5 index_econ_pca), name(Graph_M2_5, replace)
graph export "DOC/images/Stata2.0/graphM2_5.png", as(png) name("Graph_M2_5") 

xtreg index_envm_uw index_econ_pca index_econ_pca_sq, fe vce(r) // Model 2(6) LHS PCA
outreg2 using "DOC/Results2.0/results_model2.doc", append word
predict fit_values_M2_6, xb
twoway (line fit_values_M2_2 index_econ_pca), name(Graph_M2_6, replace)
graph export "DOC/images/Stata2.0/graphM2_6.png", as(png) name("Graph_M2_6")

xtreg index_env_model2 index_econ_uw index_econ_uw_sq, fe vce(r) // Model 2(7) LHS UW
outreg2 using "DOC/Results2.0/results_model2.doc", append word
predict fit_values_M2_7, xb
twoway (line fit_values_M2_7  index_econ_uw), name(Graph_M2_7, replace)
graph export "DOC/images/Stata2.0/graphM2_7.png", as(png) name("Graph_M2_7")

xtreg index_envm_uw index_econ_uw index_econ_uw_sq, fe vce(r) // Model 2(8) LHS PCA
outreg2 using "DOC/Results2.0/results_model2.doc", append word
predict fit_values_M2_8, xb
twoway (line fit_values_M2_8 index_econ_uw), name(Graph_M2_8, replace)
graph export "DOC/images/Stata2.0/graphM2_8.png", as(png) name("Graph_M2_8")

* REGRESSIONS - MODEL 3

//Epoch 1
xtset xtid epoch_1
xtreg index_env_pca index_econ_pca index_econ_pca_sq, fe vce(r) // Model 3(1)
outreg2 using "DOC/Results2.0/results_model3.doc", replace word
predict fit_values_M3_1, xb
twoway (line fit_values_M3_1 index_econ_pca), name(Graph_M3_1, replace)
graph export "DOC/images/Stata2.0/graphM3_1.png", as(png) name("Graph_M3_1")

xtreg index_env_pca index_econ_uw index_econ_uw_sq, fe vce(r) // Model 3(2)
outreg2 using "DOC/Results2.0/results_model3.doc", append word
predict fit_values_M3_2, xb
twoway (line fit_values_M3_2  index_econ_uw), name(Graph_M3_2, replace)
graph export "DOC/images/Stata2.0/graphM3_2.png", as(png) name("Graph_M3_2")

//Epoch 2
xtset xtid epoch_2
xtreg index_env_pca index_econ_pca index_econ_pca_sq, fe vce(r) // Model 3(3)
outreg2 using "DOC/Results2.0/results_model3.doc", append word
predict fit_values_M3_3, xb
twoway (line fit_values_M3_3 index_econ_pca), name(Graph_M3_3, replace)
graph export "DOC/images/Stata2.0/graphM3_3.png", as(png) name("Graph_M3_3")

xtreg index_env_pca  index_econ_uw index_econ_uw_sq, fe vce(r) // Model 3(4)
outreg2 using "DOC/Results2.0/results_model3.doc", append word
predict fit_values_M3_4, xb
twoway (line fit_values_M3_4 index_econ_uw), name(Graph_M3_4, replace)
graph export "DOC/images/Stata2.0/graphM3_4.png", as(png) name("Graph_M3_4")

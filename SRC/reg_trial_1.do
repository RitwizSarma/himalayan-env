clear
cd "C:\LargeFiles\Research\himalayan_env\himalayan-env"
use "DATA\DTA/DATA.dta"
summarize vcfd pm25 lst_median, detail
egen var1 = std(vcfd)
egen var2 = std(pm25)
egen var3 = std(lst_median)
pca vcfd pm25 lst_median 
screeplot
pca lst_median pm25 vcfd
matrix loadings = e(r)
matrix list loadings
pca lst_median pm25 vcfd
ereturn list
matrix loadings = e(L)
matrix list loadings
local w1 = loadings[1,1]
local w2 = loadings[2,1]
local w3 = loadings[3,1]
generate index_env = `w1'*lst_median + `w2'*pm25 + `w3'*vcfd
summarize index_env
egen std_NTL = std(ntl_mean)
egen std_GHSL = std(ghsl_mean)
generate index_econ = (std_NTL + std_GHSL) / 2
gen sq_index= index_env^2
xtset xtid year 
xtreg index_econ index_env sq_index, fe
predict fitted_values, xb
twoway (line fitted_values index_env)
graph export "DOC/images/graph1.png", as(png) name("Graph1")
xtset xtid year 
xtreg std_NTL index_env sq_index, fe
predict fit_values, xb
twoway (line fit_values index_env)
graph export "DOC/images/graph2.png", as(png) name("Graph2")
generate epoch_1 = year if year >= 2000 & year <= 2010
generate epoch_2 = year if year > 2010 & year <= 2020
xtset xtid epoch_1 
xtreg std_NTL index_env sq_index, fe
predict fit_values_epoch1, xb
twoway (line fit_values_epoch1 index_env)
graph export "DOC/images/graph3.png", as(png) name("Graph3")
xtset xtid epoch_2
xtreg std_NTL index_env sq_index, fe
predict fit_values_epoch2, xb
twoway (line fit_values_epoch2 index_env)
graph export "DOC/images/graph4.png", as(png) name("Graph4")
xtset xtid epoch_1 
xtreg index_econ index_env sq_index, fe
predict fit_values_epoch1_econ, xb
twoway (line fit_values_epoch1_econ index_env)
graph export "DOC/images/graph5.png", as(png) name("Graph5")
xtset xtid epoch_2 
xtreg index_econ index_env sq_index, fe
predict fit_values_epoch2_econ, xb
twoway (line fit_values_epoch2_econ index_env)
graph export "DOC/images/graph6.png", as(png) name("Graph6")
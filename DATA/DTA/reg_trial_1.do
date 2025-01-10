clear
use "/Users/rijugarg/Documents/GitHub/himalayan-env/DATA/DTA/DATA.dta"
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
generate index_econ = (ntl_mean + ghsl_mean) / 2
gen sq_index= index_env^2
xtset xtid year 
xtreg index_econ index_env sq_index, fe
predict fitted_values, xb
twoway (line fitted_values index_env)

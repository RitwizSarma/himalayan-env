clear
use "/Users/rijugarg/Documents/GitHub/himalayan-env/DATA/DTA/lst.dta"
sum lst_mean
replace lst_mean = "." if lst_mean == "NA"
destring lst_mean, generate(lst_mean_float) float
destring lst_mean, replace float
replace lst_median = "." if lst_median == "NA"
destring lst_median, generate(lst_median_float) float
destring lst_median, replace float
replace lst_sum = "." if lst_sum == "NA"
destring lst_sum, generate(lst_sum_float) float
destring lst_sum, replace float
sum lst_mean

drop lst_mean_float
drop lst_median_float
drop lst_sum_float

save "lst_correct.dta", replace

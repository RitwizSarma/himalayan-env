*Final models*

clear
cd /Users/rijugarg/Documents/GitHub/himalayan-env/
//cd "C:\LargeFiles\Research\himalayan_env\himalayan-env"

use "DATA/DTA/DATA.dta"

preserve
collapse (mean) ghsl_mean ntl_mean, by(year)
drop if year<2000
twoway (bar ntl_mean year, lcolor("35 55 59") lwidth(medium)), ylabel(0(2)10, nogrid) ytitle("Mean night light luminosity") xlabel(, angle(45) nogrid) title("Annual Mean Night Light Luminosity") graphregion(color("250 250 250")) plotregion(color("250 250 250"))
graph export "/Users/rijugarg/Desktop/NTL graph.png", as(png) name("Graph")
twoway (line ghsl_mean year, lcolor("35 55 59") lwidth(medium)), ylabel(0(20)160, nogrid) ytitle("Mean built-up surface area") xlabel(, angle(45) nogrid) title("Annual Mean Built-Up Surface Area") graphregion(color("250 250 250")) plotregion(color("250 250 250"))
graph export "/Users/rijugarg/Desktop/GHSL graph.png", as(png) name("Graph")
restore

xtset xtid year 
drop if year<2000
xtreg index_env_pca index_econ_pca index_econ_pca_sq , fe vce(r) 
predict fitting_values
twoway (line fitting_values index_econ_pca, lcolor("35 55 59") lwidth(medium)), ylabel(, nogrid) ytitle("Fitted Values") xlabel(, angle(45) nogrid) title("Index_econ_pca") graphregion(color("250 250 250")) plotregion(color("250 250 250"))
graph export "/Users/rijugarg/Desktop/Model 3 (1).png", as(png) name("Graph")
twoway (line fitting_values index_econ_pca, lcolor("35 55 59") lwidth(medium)), ylabel(0(20)160, nogrid) ytitle("Environmental Degradation") xlabel(, angle(45) nogrid) graphregion(color("250 250 250")) plotregion(color("250 250 250"))
twoway (line fitting_values index_econ_pca, lcolor("35 55 59") lwidth(medium)), ylabel(none, nogrid) ytitle("Environmental Degradation") xlabel(none, nogrid) graphregion(color("250 250 250")) plotregion(color("250 250 250"))
twoway (line fitting_values index_econ_pca, lcolor("35 55 59") lwidth(medium)), ylabel(none, nogrid) ytitle("Environmental Degradation") xlabel(none, nogrid) xtitle("Economic Growth") graphregion(color("250 250 250")) plotregion(color("250 250 250"))
graph export "/Users/rijugarg/Desktop/M3.png", as(png) name("Graph")
  xtreg index_env_model2 index_econ_pca index_econ_pca_sq, fe vce(r)
predict fitt_values
twoway (line fitt_values index_econ_pca, lcolor("35 55 59") lwidth(medium)), ylabel(none, nogrid) ytitle("Environmental Degradation") xlabel(none, nogrid) xtitle("Economic Growth") graphregion(color("250 250 250")) plotregion(color("250 250 250"))
graph export "/Users/rijugarg/Desktop/M2(1).png", as(png) name("Graph")
  xtreg index_env_model2 std_NTL std_NTL_sq, fe vce(r)
predict fitttvaluee
twoway (line fitttvaluee std_NTL, lcolor("35 55 59") lwidth(medium)), ylabel(none, nogrid) ytitle("Environmental Degradation") xlabel(none, nogrid) xtitle("Economic Growth") graphregion(color("250 250 250")) plotregion(color("250 250 250"))
graph export "/Users/rijugarg/Desktop/M1.png", as(png) name("Graph")

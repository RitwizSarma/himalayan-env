clear
cd "C:\LargeFiles\Research\himalayan_env\himalayan-env\DATA\DTA"

append using "pm25_uk.dta" "pm25_hp.dta"
keep year shrid pm25_mean
rename pm25_mean pm25
label var pm25 "Mean level of PM2.5 (air quality)"
label var year "Year"
label var shrid "Shrid ID"
save "pm25.dta", replace

clear
append using "vcf_hp.dta" "vcf_uk.dta"
rename vcf_mean vcf
replace vcf = "1823" if vcf=="NA"
destring vcf, replace
replace vcf = . if vcf==1823
gen vcfd = 100 - vcf
keep vcfd year shrid
label var vcfd "Area not covered by vegetation (forest cover)"

merge 1:1 shrid year using "pm25.dta", gen(merge1) // 97k observations will not merge, since PM2.5 data has larger temporal extent than VCF data, similar messages can be expected for succeeding merges
merge 1:1 shrid year using "lst.dta", gen(merge2)
merge 1:1 shrid year using "ntl.dta", gen(merge3)
merge 1:1 shrid year using "ghsl.dta", gen(merge4)

drop lst_mean lst_sum ghsl_median ghsl_sum ntl_median ntl_sum
drop merge1 merge2 merge3 merge4 // drop this line to check for merge sanity
label var year "Year"
label var shrid "Shrid ID"
label var lst_median "Median of land surface temperature/emissivity"
label var ntl_mean "Mean of night-time light radiance"
label var ghsl_mean "Mean of built-up surface"

by shrid (year), sort: gen id = _n == 1 // xtset requires numeric IDs rather than str - this builds a new numeric id for use with the xtset command
gen xtid = sum(id)
drop id
label var xtid "ID for use with xtset, unique to Shrid ID"

save "DATA.dta", replace
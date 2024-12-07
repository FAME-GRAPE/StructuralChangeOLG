// create merged data sets
cap prog drop Gini_Merge
program define Gini_Merge		
	args var fname theta
	loc data : dir "datasets/." files "?_`var'_theta`theta'_`fname'.dta"
// 	loc data : dir "datasets/." files "?_`var'_`fname'.dta"
	di `data'
	
	clear
	use "datasets/0_`var'.dta"
// 	loc i = 0
	foreach d of local data {
// 		if `i != 0' {
		di "`d'"
			cap merge 1:1 yr using "datasets/`d'"
			cap drop _merge
// 		}
		cap keep gini* yr
// 		loc ++i //= `i' + 1
	}
	cap drop yr
	gen year = _n + 1979 
	tsset year

// 	ren gini_coef_0 baseline
	ren gini_coef_1 unemployment
	ren gini_coef_2 gamma
// 	ren gini_coef_3 Demo
// 	ren gini_coef_4 Structure
// 	ren gini_coef_5 gamma_Demo
// 	ren gini_coef_6 Demo_Structure
	ren gini_coef_7 gamma_Structure
	ren gini_coef_8 ALL
	cap drop gini* 

	sa "datasets/_data_gini_`var'_theta`theta'_`fname'", replace
end
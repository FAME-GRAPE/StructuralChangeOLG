// calculate Gini coefficient
cap prog drop Gini_calc
program define Gini_calc			
	args var directory counter		// counter sta
	clear
// 	if `counter' != 0 | `counter' != 3 | `counter' != 4 | `counter' != 6 | `counter' != 7 { 	// & counter' == 1 { //| `counter' == 8

	if `counter' == 1 | `counter' == 2 | `counter' == 7 | `counter' == 8 {
		pwd
		import delimited "`directory'/outcomes/gini_`var'_trans.csv"
		drop if gini_weight_`var' == 0
		if "`var'" == "inc" drop if age > 42
// 		if "`var'" == "sv" {
// 			drop year
// 			ren omega_share_sv year
// 			ren omega_sv omega_share_sv
// 			ren age omega_sv
// 			ren sv2 age
// 		}
		drop if year >= 80
		compress
		//di "`1'/outcomes/gini_sv_trans.csv"
		//import delimited "F:\janek\code trans\SCENARIOS\_Demo\2. Demo_o4gq\outcomes\gini_sv_trans.csv"
		qui{
			replace gini_weight_`var' = ceil(gini_weight_`var'*10e5)
			gen gini_coef_`counter' = .
			cap noi drop if year == "**"
			if substr("`:type year'" , 1, 3) == "str" {
				encode year, gen(yr)
			}
			else {
				ren year yr
			}
			levelsof yr
			foreach y in `r(levels)' {
				sgini `var' [fweight = gini_weight_`var'] if yr == `y'	// iweights better for inc (1)
				local gc  = r(coeff)
				recode gini_coef_`counter' (. = `gc') if yr == `y'
				drop if yr == `y' & age != 21
			}
			keep yr gini_coef_`counter'
			duplicates drop
		}
		sa "gini/datasets/`counter'_`var'_theta2_alt_11_V.dta", replace
	}
end
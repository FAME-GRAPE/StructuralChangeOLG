/*
sysdir set PLUS "F:\JANEK\Stata16\ado"
ssc install sgini
ssc install gtools
gtools, upgrade
ssc install sumdist
ssc install scheme-burd, replace
set scheme burd
*/

program define GiniW
	clear
	//if `i' == 11 {
	import delimited "`1'/outcomes/gini_sv_trans.csv"
	//di "`1'/outcomes/gini_sv_trans.csv"
	//import delimited "F:\janek\code trans\SCENARIOS\_Demo\2. Demo_o4gq\outcomes\gini_sv_trans.csv"
	qui{
		replace gini_weight_sv = ceil(gini_weight_sv*10e5)
		gen gini_coef_`2' = .
		//drop if year == "**"
		if substr("`:type year'" , 1, 3) == "str" {
			encode year, gen(yr)
		}
		else {
			ren year yr
		}
		glevelsof yr
		foreach y in `r(levels)' {
			sgini sv [fweight = gini_weight_sv] if yr == `y'
			local gc  = r(coeff)
			recode gini_coef_`2' (. = `gc') if yr == `y'
			drop if yr == `y' & age_sv != 21
		}
		keep yr gini_coef_`2'
		duplicates drop
	}
	sa `2'_sv.dta, replace
	//}
end


// calculate Gini on wealth
clear
cd "F:\janek\code trans\SCENARIOS\_stochastic (7 VIII)"
local subdirs : dir "." dirs "*"

di `subdirs'

/*multishell loop*/ forvalues n = 1/16 {
	local d `: word `n' of `subdirs''
	di `"`d'"'
	loc i = `n' - 1
	GiniW `"`d'"' `i'	
}

/*

// create merged data sets
loc data : dir "." files "*_sv.dta"
di `data'
/*
foreach da of local data {
	if regexm(`da', "^[0-9]+_sv.dta") di `da' //loc data2 = `data2' + `da'
}
di `data2'
*/
clear
use 0_sv.dta
loc i = 0
foreach d of local data {
	if `i != 0' {
		merge 1:1 yr using `d'
		drop _merge
	}
	keep gini* yr
	loc i = `i' + 1
}
gen year = yr + 1993 
tsset year

ren gini_coef_0 baseline
ren gini_coef_1 gamma
ren gini_coef_2 Demo
ren gini_coef_3 Shares
ren gini_coef_4 Margins
ren gini_coef_5 Shares_Margins
ren gini_coef_6 ALL
ren gini_coef_7 gamma_Demo
ren gini_coef_8 gamma_Shares
ren gini_coef_9 gamma_Margins
ren gini_coef_10 Demo_Shares
ren gini_coef_11 Demo_Margins
ren gini_coef_12 gamma_Shares_Margins
ren gini_coef_13 Demo_Shares_Margins
ren gini_coef_14 gamma_Demo_Shares
ren gini_coef_15 gamma_Demo_Margins
drop yr 

sa "_data_gini_sv", replace

// Load data set and draw graphs for wealth
clear 
cd "F:\janek\code trans\SCENARIOS\_stochastic (7 VIII)"
use "_data_gini_sv"
//use "_stochastic (6 VIII) omega_hp_error\_data_gini_sv"

global baseline "(connected baseline year, lpattern(solid)  msymbol(0))"
global gamma "(connected gamma year , lpattern(solid) msymbol(oh)) "
global Demo "(connected Demo year , lpattern(dash) msymbol(Th))"
global Shares "(connected Shares year , lpattern(solid) lcolor(navy) mcolor(navy) msymbol(sh))"
global Margins "(connected Margins year , lpattern(solid) msymbol(dh))"
global Shares_Margins "(connected Shares_Margins year, lcolor(navy) mcolor(navy) lpattern(solid) msymbol(dh))"
global ALL "(connected ALL year , lpattern(solid) )"

global gamma_Demo "(connected gamma_Demo year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
global gamma_Shares "(connected gamma_Shares year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
global gamma_Margins "(connected gamma_Margins year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
global Demo_Shares "(connected Demo_Shares year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
global Demo_Margins "(connected Demo_Margins year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
global gamma_Shares_Margins "(connected gamma_Shares_Margins year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
global Demo_Shares_Margins "(connected Demo_Shares_Margins year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
global gamma_Demo_Shares "(connected gamma_Demo_Shares year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
global gamma_Demo_Margins "(connected gamma_Demo_Margins year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
tsset year 


/*
global baseline "(connected baseline year, lpattern(solid)  msymbol(0))"
global gamma "(connected gamma year , lpattern(solid) msymbol(oh)) "
global Demo "(connected Demo_Margins year , lpattern(dash) msymbol(Th))"
global Shares "(connected gamma_Shares_Margins year , lpattern(solid) lcolor(navy) mcolor(navy) msymbol(sh))"
global Margins "(connected Demo_Shares_Margins year , lpattern(solid) msymbol(dh))"
global Shares_Margins "(connected gamma_Demo_Shares year, lcolor(navy) mcolor(navy) lpattern(solid) msymbol(dh))"
global ALL "(connected gamma_Demo_Margins year , lpattern(solid) )"
*/

twoway $gamma_Demo_Margins if year<2080, ///
				 title(Gini coefficient on wealth) ylabel(.49(0.02).60, grid glcolor(gs4)) //ylabel(.57(0.02).67, grid glcolor(gs4))

twoway $baseline $gamma $Demo $Shares $Margins $Shares_Margins $ALL if year<2080, ///
				 title(Gini coefficient on wealth) ylabel(.49(0.02).60, grid glcolor(gs4)) //ylabel(.57(0.02).67, grid glcolor(gs4))
				 
twoway $baseline $Shares_Margins $gamma_Shares_Margins $ALL if year<2080, ///
				 tsc(r(1994(1)2050)) title(Gini coefficient on wealth) ylabel(.51(0.02).60, grid glcolor(gs4)) //ylabel(.57(0.02).67, grid glcolor(gs4))
				 
twoway $baseline $Shares_Margins $Demo $Demo_Margins $Demo_Shares_Margins if year<2080, ///
				 tsc(r(1994(1)2050)) title(Gini coefficient on wealth) ylabel(.51(0.02).60, grid glcolor(gs4)) //ylabel(.57(0.02).67, grid glcolor(gs4))
			
graph export gini_wealth.png, replace
graph close

///////////////////////////////////////////////////////////////////

clear
use 0_sv.dta
loc i = 0
forvalues d = 1(1)5 {
	if `i' != 0 {
		merge 1:1 yr using `d'_sv
		drop _merge
	}
	loc i = `i' + 1
}
gen year = yr + 1993

tsset year
tsline gini_coef_0 gini_coef_1 gini_coef_3 gini_coef_4 gini_coef_5 if year < 2050, lw(medthick ..) tsc(r(1994(1)2050)) title(Gini savings)
line gini* year if year < 2050, lw(medthick ..) title(Gini savings)

///////////////////////////////////////////////////////////////////
	clear
	import delimited "6. ALL/outcomes/gini_sv_trans.csv"
	qui{
		replace gini_weight_sv = ceil(gini_weight_sv*10e5)
		gen gini_coef_6 = .
		//drop if year == "**"
		if substr("`:type year'" , 1, 3) == "str" {
			encode year, gen(yr)
		}
		else {
			ren year yr
		}
		levelsof yr
		foreach y in `r(levels)' {
			sgini sv [fweight = gini_weight_sv] if yr == `y'
			local gc  = r(coeff)
			recode gini_coef_6 (. = `gc') if yr == `y'
			drop if yr == `y' & age_sv != 21
		}
		keep yr gini_coef_6
		duplicates drop
	}
	sa 6_sv.dta, replace

	
		sgini sv [fweight = gini_weight_sv] if yr == 10
		local gc  = r(coeff)
		recode gini_coef_1 (. = `gc') if yr == 10
		drop if yr == 10 & age_sv != 21
*/
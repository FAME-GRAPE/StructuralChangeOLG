cap sysdir set PLUS "F:\JANEK\Stata16\ado"
cap ssc install scheme-burd, replace
cap set scheme burd
	
clear 
cd "F:\janek\code trans\SCENARIOS\8. ALL\outcomes"

import delim "F:\janek\code trans\data\subsidy_ratio_data", clear
ren v1 data
tempfile dtemp
save `dtemp'

clear
set obs 10
gen data = 0
append using `dtemp'

import delimited "base__G_taxCsubsidy_share_trans.txt", clear
merge 1:1 _n using `dtemp', nogen //assert(match)
ren v1 model

tempfile mtemp
save `mtemp'

gen t = _n + 1979
tsset t
tsline model data if t <= 2020


// =========================================================================
// cd "F:\janek\code trans\SCENARIOS\_alternative\8. ALL\outcomes"
// cd "F:\janek\code trans\SCENARIOS\_baseline\8. ALL\outcomes"
// cd "F:\janek\code trans\SCENARIOS\_common pensions\8. ALL\outcomes"
//
// cd "F:\janek\code trans\SCENARIOS\8. ALL\outcomes"
// =========================================================================


import delimited "base__G_taxCbenefits_trans.txt", clear
ren v1 pensions
gen t = _n + 1979
tsset t
tsline pensions if t <= 2140


// =========================================================================

import delimited "base__G_taxCbequest.txt", clear
ren v1 bequest
gen t = _n + 1979
tsset t
tsline bequest //if t >= 2240

// =========================================================================

import delimited "base__G_taxCsubsidy_share_trans.txt", clear
ren v1 subsidy
gen t = _n + 1979
tsset t
tsline subsidy if t <= 2140

// =========================================================================

import delimited "base__G_taxCcontrib_to_gdp_trans.txt", clear
ren v1 contribution
gen t = _n + 1979
tsset t
tsline contribution if t <= 2140

// =========================================================================

cd "F:\janek\code trans\SCENARIOS\8. ALL\outcomes"
import delimited "base__G_taxCinvestment_rate.txt", clear
ren v1 ALL
tempfile temp
save "`temp'", replace
cd "F:\janek\code trans\SCENARIOS\5. gamma + Demo\outcomes"

import delimited "base__G_taxCinvestment_rate.txt", clear
merge 1:1 _n using `temp', nogen
ren v1 gamma_Demo
tempfile temp2
save "`temp2'", replace
cd "F:\janek\code trans\SCENARIOS\2. gamma\outcomes"

import delimited "base__G_taxCinvestment_rate.txt", clear
merge 1:1 _n using `temp2', nogen
ren v1 gamma
tempfile temp3
save "`temp3'", replace
cd "F:\janek\code trans\SCENARIOS\1. unemployment\outcomes"

import delimited "base__G_taxCinvestment_rate.txt", clear
merge 1:1 _n using `temp3', nogen
ren v1 unemployment


gen t = _n + 1979
tsset t
tsline ALL g* u* if t <= 2140


// =========================================================================

import delimited "base__G_taxCtC_trans.txt", clear
ren v1 taxC
gen t = _n + 1979
tsset t
tsline taxC if t <= 2140

// =========================================================================

import delimited "base__G_taxCtK_tax_revenue.txt", clear
ren v1 taxK_revenue
gen t = _n + 1979
tsset t
// tsline taxK_revenue if t <= 2140, name("taxK_revenue", replace)
tempfile temp
save "`temp'", replace

import delimited "base__G_taxCtC_tax_revenue.txt", clear
merge 1:1 _n using `temp', nogen
ren v1 taxC_revenue
// gen t = _n + 1979
// tsset t
// tsline taxC_revenue if t <= 2140, name("taxC_revenue", replace)
tempfile temp2
save "`temp2'", replace

import delimited "base__G_taxCtL_tax_revenue.txt", clear
merge 1:1 _n using `temp2', nogen
ren v1 taxL_revenue
// gen t = _n + 1979
tsset t
tsline tax* if t <= 2140, name("tax_revenue", replace)

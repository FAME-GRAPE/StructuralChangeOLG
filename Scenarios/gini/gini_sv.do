/*
cap sysdir set PLUS "F:\JANEK\Stata16\ado"
cap ssc install sgini
cap ssc install gtools
cap gtools, upgrade
cap ssc install sumdist
cap ssc install scheme-burd, replace
cap set scheme burd
*/

// calculate Gini on wealth
clear
run gini_calc.ado
cd ..
//cd "F:\janek\code trans\SCENARIOS\_stochastic (7 VIII)"
local subdirs : dir "." dirs "*"

di `subdirs'

/*multishell loop*/ forvalues n = 0/8 {
	loc k = `n' + 1
	local d `: word `k' of `subdirs''
	di `"`d'"'
	//pwd
	Gini_calc "sv" `"`d'"' `n'	
}

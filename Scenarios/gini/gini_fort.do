cap sysdir set PLUS "F:\JANEK\Stata16\ado
cap ssc install multishell
cap ssc install sgini
cap ssc install scheme-burd, replace
cap set scheme burd

clear all
adopath ++ "F:\JANEK\Stata16\ado\"
multishell path "C:\Users\jlutynski\Documents\multishell_temp\", clear
multishell adopath "F:\JANEK\Stata16\ado"
multishell add "gini_cons.do", skiploop
multishell add "gini_sv.do", skiploop
multishell add "gini_inc2.do", skiploop
multishell add "gini_inc.do", skiploop

multishell run, threads(4) sleep(15000)

!move /Y "*.log" "./logs"
//!del *.log

global fname "alternative"
run gini_merge.ado
Gini_Merge "sv" $fname
Gini_Merge "cons" $fname
Gini_Merge "inc" $fname
Gini_Merge "inc2" $fname

foreach var in sv cons inc inc2 {	// graphs to file
	loc fname2 $fname
	Gini_graph "`var'" $fname
	twoway $unemployment $gamma $gamma_Demo $ALL if year >= 1989 & year <= 2050, $gopt //nodraw
	graph export graphs\gini_`var'_`fname2'.png, replace 
	graph close
}
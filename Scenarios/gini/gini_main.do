///*
cap sysdir set PLUS "F:\JANEK\Stata16\ado
cap ssc install multishell
cap ssc install sgini
cap ssc install scheme-burd, replace
cap set scheme burd
//*/

clear all
adopath ++ "F:\JANEK\Stata16\ado\"
multishell path "C:\Users\jlutynski\Documents\multishell_temp\", clear
multishell adopath "F:\JANEK\Stata16\ado"
multishell add "gini_cons.do", skiploop
multishell add "gini_sv.do", skiploop
multishell add "gini_sv2.do", skiploop
multishell add "gini_inc2.do", skiploops
multishell add "gini_inc.do", skiploop

multishell run, threads(8) sleep(15000)

!move /Y "*.log" "./logs"
//!del *.log

global fname "alt_11_V" //"alt_8_V"
global theta "2"

run gini_merge.ado
Gini_Merge "sv" $fname $theta
Gini_Merge "sv2" $fname $theta
Gini_Merge "cons" $fname $theta
Gini_Merge "inc" $fname $theta
Gini_Merge "inc2" $fname $theta

// !copy /Y "*$theta_$fname.dta" "C:\Users\jlutynski\Dropbox (FAME GRAPE)\NCN EMERYT\_Paper_17_reallocation\_code\code trans\SCENARIOS\gini\datasets"

/*
run gini_graph.ado
//cd "F:\janek\code trans\SCENARIOS\_stochastic (7 VIII)"
Gini_graph "sv" $fname
Gini_graph "sv2" $fname

// Gini_graph "cons" $fname
//
// Gini_graph "inc" $fname	 
// Gini_graph "inc2" $fname 

// twoway $unemployment $gamma $gamma_Demo $ALL if year >= 1989 & year <= 2050, $gopt
// // twoway $gamma $gamma_Demo $ALL if year > 1994 & year<2050, $gopt
			
foreach var in sv sv2 cons inc inc2 {	// graphs to file
	loc fname2 $fname
	Gini_graph "`var'" $fname
	twoway $unemployment $gamma $gamma_Demo $ALL $area if year >= 1989 & year <= 2050, $gopt //nodraw
	graph export graphs\gini_`var'_`fname2'.png, replace 
	graph export "C:\Users\jlutynski\Dropbox (FAME GRAPE)\NCN EMERYT\_Paper_17_reallocation\graphs\theta $theta\gini_`var'_`fname2'.png", replace 
	graph close
}
*/


// twoway $unemployment $gamma $Demo $Structure if year<2050, $gopt	// $ALL 				 
// twoway $baseline $gamma $Structure $gamma_Structure $ALL if year<2030, $gopt				 
// twoway $baseline $Demo $Structure $Demo_Structure if year<2030, $gopt

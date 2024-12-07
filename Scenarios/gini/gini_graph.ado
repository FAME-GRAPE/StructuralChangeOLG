cap prog drop Gini_graph
program define Gini_graph	// Load data set and draw graphs for wealth
	args var fname
	clear 
	pwd
	di "datasets/_data_gini_`var'_theta`theta'_`fname'"
	use "datasets/_data_gini_`var'_theta`theta'_`fname'"
	//use "_stochastic (6 VIII) omega_hp_error\_data_gini_sv"
	cap foreach v of varlist gamma gamma_Demo ALL unemployment {
		cap replace `v' = `v'/unemployment 
	}

	global baseline "(connected baseline year, lpattern(solid)  msymbol(0))"
	global unemployment "(connected unemployment year , lpattern(shortdash_dot) msymbol(smplus))"
	global gamma "(connected gamma year , lpattern(solid) msymbol(oh))"
	global Demo "(connected Demo year , lpattern(dash) msymbol(Th))"
	global Structure "(connected Structure year, lcolor(navy) mcolor(navy) lpattern(solid) msymbol(dh))"
	global gamma_Demo "(connected gamma_Demo year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
	global gamma_Structure "(connected gamma_Structure year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
	global Demo_Structure "(connected Demo_Structure year, lcolor(navy) mcolor(navy) lpattern(dash_dot) msymbol(x))"
	global ALL "(connected ALL year , lpattern(dot) msymbol(+))"
	global area "(rarea gamma_Demo ALL year, sort fcolor(red) lcolor(red) lw(0) fintensity(3))"
	tsset year 
	//drop if year == 1989
	if "`var'" == "sv" gl gopt title(Gini coefficient on wealth) ylabel(.58(0.01).661, grid glcolor(gs14)) xlabel(1990(10)2050, grid glcolor(gs14)) 
	if "`var'" == "sv2" gl gopt title(Gini coefficient on wealth) ylabel(.58(0.01).661, grid glcolor(gs14)) xlabel(1990(10)2050, grid glcolor(gs14)) 
	if "`var'" == "cons" global gopt title(Gini coefficient on consumption) ylabel(.22(0.01).25, grid glcolor(gs14)) xlabel(1990(10)2050, grid glcolor(gs14)) // ylabel(.15(0.005).19, grid glcolor(gs4)) 
	if "`var'" == "inc" global gopt title(Gini coefficient on income) ylabel(.23(0.02).34, grid glcolor(gs14)) xlabel(1990(10)2050, grid glcolor(gs14)) //ylabel(.22(0.005).24, grid glcolor(gs4))
	if "`var'" == "inc2" global gopt title(Gini coefficient on income 2) ylabel(.25(0.025).45, grid glcolor(gs14)) xlabel(1990(10)2050, grid glcolor(gs14)) //ylabel(.22(0.005).24, grid glcolor(gs4))
	
	
	if "`var'" == "sv" gl gopt title(Gini coefficient on wealth (sv+bequests)) ylabel(.99(0.01)1.07, grid glcolor(gs14)) xlabel(1990(10)2050, grid glcolor(gs14)) 
	if "`var'" == "sv2" gl gopt title(Gini coefficient on wealth (svplus)) ylabel(.99(0.01)1.05, grid glcolor(gs14)) xlabel(1990(10)2050, grid glcolor(gs14)) 
	if "`var'" == "cons" global gopt title(Gini coefficient on consumption) ylabel(.9(0.02)1.1, grid glcolor(gs14)) xlabel(1990(10)2050, grid glcolor(gs14)) // ylabel(.15(0.005).19, grid glcolor(gs4)) 
	if "`var'" == "inc" global gopt title(Gini coefficient on income) ylabel(.99(0.01)1.05, grid glcolor(gs14)) xlabel(1990(10)2050, grid glcolor(gs14)) //ylabel(.22(0.005).24, grid glcolor(gs4))
	if "`var'" == "inc2" global gopt title(Gini coefficient on income (with retirees)) ylabel(.99(0.05)1.25, grid glcolor(gs14)) xlabel(1990(10)2050, grid glcolor(gs14)) 

end



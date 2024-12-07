cap sysdir set PLUS "F:\JANEK\Stata16\ado"
cap ssc install scheme-burd, replace
cap set scheme burd

clear
cd "F:\janek\code trans\data"

forv i = 1/4 {
	import delim "_data_omega_share_o`i'_v6.csv", clear
	forv k = 1/80 {
		ren j`k' o`i'j`k'
	}
		loc ii = `i' - 1
// 	loc tempi temp`ii'
	if `i' != 1 merge 1:1 _n using `temp', nogen 
	tempfile temp //`i'
	save "`temp'", replace
}
gen i = _n
tempfile shares
save "`shares'"
import delim "F:\janek\code trans\SCENARIOS\8. ALL\outcomes\N_post.txt", clear
ren v1 N
ren v2 j
ren v3 i
reshape wide N, i(i) j(j)

egen N = rowtotal(N*)
merge 1:1 i using `shares', nogen 

forv i = 1/4 {
	forv k = 1/80 {
		gen s`i'`k' = o`i'j`k'*N`k'
	}
	egen sum_s = rowtotal(s`i'*)
	gen share`i' = sum_s/N
	drop s`i'* sum*
}

gen year = i + 1988
tsset year
drop N* o* i

tempfile shares
save "`shares'", replace
import delim "_data_omega_share_1y.csv", clear
merge 1:1 _n using `shares', nogen 

tsline share1 share_he_services if year <= 2020, legend(order(1 "model" 2 "data") position(0) bplacement(neast)) ysize(10) xsize(15) ylabel(0(0.1).6, grid glcolor(gs14)) xlabel(1990(5)2020, grid glcolor(gs14)) xtitle("") name("g1", replace) title("{&chi}{sub:S,HE}", span)
tsline share2 share_le_services if year <= 2020, legend(order(1 "model" 2 "data") position(0) bplacement(neast)) ysize(10) xsize(15) ylabel(0(0.1).6, grid glcolor(gs14)) xlabel(1990(5)2020, grid glcolor(gs14)) xtitle("") name("g2", replace) title("{&chi}{sub:S,LE}", span)
tsline share3 share_he_manufacturing if year <= 2020, legend(order(1 "model" 2 "data") position(0) bplacement(neast)) ysize(10) xsize(15) ylabel(0(0.01).1, grid glcolor(gs14)) xlabel(1990(5)2020, grid glcolor(gs14)) xtitle("") name("g3", replace) title("{&chi}{sub:M,HE}", span)
tsline share4 share_le_manufacturing if year <= 2020, legend(order(1 "model" 2 "data") position(0) bplacement(neast)) ysize(10) xsize(15) ylabel(0(0.1).6, grid glcolor(gs14)) xlabel(1990(5)2020, grid glcolor(gs14)) xtitle("") name("g4", replace) title("{&chi}{sub:M,LE}", span)

graph combine g1 g2 g3 g4 , r(2) c(2)

graph export "F:\janek\code trans\graphs/shares_model.png", replace


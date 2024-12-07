/*
cap sysdir set PLUS "F:\JANEK\Stata16\ado"
cap ssc install scheme-burd, replace
cap set scheme burd
*/

clear
import delimited "F:\janek\code trans\SCENARIOS\2. Demo\outcomes\assets_1.csv"
//import delimited "F:\janek\code trans\SCENARIOS\2. Demo\outcomes\assets_iter.csv"
ren s_ss assets_ss
reshape wide assets_ss omega, i(j) j(o) 

line as* j, ylabel(0(1)20, grid glcolor(gs14)) xlabel(0(5)80, grid glcolor(gs14))

reshape wide assets_ss omega, i(j iter) j(o) 
reshape wide assets*, i(j) j(iter)
line assets_ss2* j, ylabel(0(1)20, grid glcolor(gs14)) xlabel(0(5)80, grid glcolor(gs14)) legend(off)
line assets_ss13 j

//tempfile tfile
//sa `tfile'

//////////////////////////////////////////////////////////////
/// trans

clear
cd "F:/janek/code trans/SCENARIOS/"
//cd "F:/janek/code trans/SCENARIOS/_stochastic (23 VII)/"
local subdirs : dir "." dirs "*"
loc i = 0
foreach d of local subdirs {
 di "`d'"
 	di `i'
	global scenario`i' = "`d'/outcomes/base__G_taxCsv_j_trans" //base__taxC__sv_j_trans" //base__taxC__sv_j_trans  |   euler_mat
	//global scenario`i' = "`d'/_deterministic_no_gov (31 VII)/outcomes/base__G_taxCsv_j_trans" //base__taxC__sv_j_trans
	if `i' != 1 & `i' != 6 & `i' != 18 {
		loc `i++'
	} 
	else if `i' == 1 {
		loc i = `i' + 9
	} 
	else if `i' == 18 {
		loc i = `i' - 16
	}
	else if `i' == 6 {
		loc i = `i' + 100
	}
}

clear
import delim "$scenario6" , delim(";")

ren v582 j
ren v583 o
gen jo = j + (o - 1)*581
reshape long v, i(jo)
ren v assets
ren _j i
reshape wide assets, i(i jo) j(o) 
drop jo
/* 
//ren v2 j
//ren v3 i
reshape wide euler_mat, i(i) j(j) 
line euler_mat j if j == i, $sett 
*/

global sett ylabel(0(1)20, grid glcolor(gs14))

line assets* j if i == 1, $sett name(g0, replace)
line assets* j if j == i, $sett name(g1, replace)
line assets* j if j + 30 == i  , $sett name(g2, replace)
line assets* j if j + 60 == i , $sett name(g3, replace)
line assets* j if j + 90 == i , $sett name(g4, replace)
//line assets* j if j + 120 == i , $sett name(g5, replace)
graph combine g0 g1 g2 g3 g4 , rows(1) imargin(0 0 0 0) ycommon // name(comb_5, replace) //scheme(sj) commonscheme


//merge 1:1 j using `tfile'
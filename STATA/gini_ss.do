/*
sysdir set PLUS "F:\JANEK\Stata16\ado"
cap ssc install sgini
cap ssc install scheme-burd, replace
cap set scheme burd
*/
clear
cd "F:/JANEK\code trans\SCENARIOS"
import delimited "8. ALL/outcomes/gini.csv"

replace gini_weight_sv = ceil(gini_weight_sv*10e5)
sgini sv [fweight = gini_weight_sv]
 
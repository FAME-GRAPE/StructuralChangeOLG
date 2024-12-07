clear
sysdir set PERSONAL "F:\JANEK\Stata16\ado\personal"
cap sysdir set PLUS "F:\JANEK\Stata16\ado
cap ssc install scheme-burd, replace
cap set scheme burd

loc path1 "F:\janek\code trans\SCENARIOS\3. Structure\outcomes"

to_GDP_ratio "`path1'" sum_b "base___G____y_trans.txt" "base___G____benefits_trans.txt"

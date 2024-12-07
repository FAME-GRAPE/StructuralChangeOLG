clear 
cd "F:\Janek\code trans\SCENARIOS"

import delim "8. ALL\outcomes\base__G_taxCrbar_trans.txt"
ren v1 ALL
sa r8, replace

import delim "1. unemployment\outcomes\base__G_taxCrbar_trans.txt", clear
ren v1 unemployment
sa r1, replace

import delim "2. gamma\outcomes\base__G_taxCrbar_trans.txt", clear
ren v1 gamma
sa r2, replace

import delim "5. gamma + Demo\outcomes\base__G_taxCrbar_trans.txt", clear
ren v1 gamma_Demo

merge 1:1 _n using r1, nogen
merge 1:1 _n using r2, nogen
merge 1:1 _n using r8, nogen

gen t = _n
tsset t
tsline ALL gam* un* if t< 100


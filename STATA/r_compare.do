clear 
import delim ".\_delta_final\8. ALL\outcomes\base__G_taxCr_trans.txt"

ren v1 r_w_mult
tempfile tfile
sa `tfile'

clear 
import delim ".\8. ALL\outcomes\base__G_taxCr_trans.txt"
ren v1 r

merge 1:1 _n using `tfile', nogen

gen t = _n
tsset t

tsline r*
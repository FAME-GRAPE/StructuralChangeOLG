clear
cd "F:\janek\code trans\SCENARIOS\8. ALL\outcomes"

import delimited "N_post.txt"

ren v1 N
ren v2 j
ren v3 i

xtset j i 

// twoway (line N j if i == 1) (line N j if i == 5) (line N j if i == 25) (line N j if i == 51) (line N j if i == 100) (line N j if i == 200)

reshape wide N, i(i) j(j)
// egen bigN = rowtotal(N(2/42))
gen bigN = N2
replace i = i + 1979
forv n = 3/42{
	replace bigN = bigN + N`n'
}
keep bigN i
export delimited "bigN_check.csv", replace

//cd "F:\janek\code trans\SCENARIOS"
export delimited "N_check.csv", replace
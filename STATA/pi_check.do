clear
cd "F:\janek\code trans\SCENARIOS\6. ALL\outcomes"

import delimited "pi_post.txt"

ren v1 pi
ren v2 j
ren v3 i

xtset j i 

twoway (line pi j if i == 1) (line pi j if i == 5) (line pi j if i == 25) (line pi j if i == 51) (line pi j if i == 100) (line pi j if i == 200)

line pi i if j == 80 & i < 150

reshape wide pi, i(i) j(j)

cd "F:\janek\code trans\SCENARIOS"
export delimited "pi_check.csv"
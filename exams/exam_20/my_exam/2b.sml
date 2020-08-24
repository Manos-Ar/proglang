fun check k n = k * k > n orelse n mod k <> 0 andalso check (k+2) n

fun prime 2 = true
	| prime n = n mod 2 <> 0 andalso check 3 n

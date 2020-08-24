fun c ls=
	let fun d [] n _ = n
	| d 42::xs n cur = 
		if cur < n then d xs n (cur+1)
		else d xs (cur+1) (cur+1)
	| d h::xs n cur = d xs n 0
	in d ls 0 0
end
		 

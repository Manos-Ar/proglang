fun reverse ls = 
	let 
		fun help (nil,xs) = xs
		| help (y::ys,xs) = help (ys,y::xs)
	in help (ls,nil)
end


fun reconstruct [] = []
	| reconstruct [x] = [[x]]
	| reconstruct (z::zs) =  
		let fun walk ([], _, _, acc) = acc
	 	(*| walk ((x::xs), sofar, i, ([]::ys)) = walk (xs, sofar, (i+1), [x]::ys)*)
	 	| walk ((x::xs), sofar, i, ((y::ys)::acc)) = 
	 		if sofar > i then walk (xs, sofar, (i+1), (x::y::ys)::acc)
	 		else walk (xs, x, 0, ([x]::(y::ys)::acc))
	 	in reverse (map reverse (walk (zs, z, 0, [[z]])))
end

(* reconstruct [3,1,2,3,1,4,2,5,6,0,4,7,8,9,10]*)

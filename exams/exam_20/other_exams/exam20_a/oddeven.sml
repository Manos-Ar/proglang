fun oddeven [] = 0
	| oddeven (y::ys) =
		let fun walk [] sofar _ _  = sofar
		| walk (x::xs) sofar curr pre = 
			let 
				val a = x mod 2
				val b = pre mod 2
			in
			if a <> b andalso curr < sofar then walk xs sofar (curr+1) x
			else if a<> b andalso curr >= sofar then walk xs (curr+1) (curr+1) x
			else walk xs sofar 1 x
		end
		in walk ys 1 1 y
end

(*
sml oddeven [1,2,3];
sml oddeven [6,8,17,42,37,91];
sml oddeven [23,11,38,39,33,24,25,13,36,7,10,9,6,36];
*)

fun reverse ls = 
	let 
		fun help (nil,xs) = xs
		| help (y::ys,xs) = help (ys,y::xs)
	in help (ls,nil)
end

fun pair [] _= (0,0)
|pair (x::xs) y =
	if x = 2*y then (y,2*y)
	else pair xs y

fun double_pair ls = 
	let 
		val a= map (pair ls) ls
		fun del ([],acc) = acc
		| del ((0,0)::xs,acc) = del (xs,acc)
		| del ((a,b)::xs,acc) = del (xs, (a,b)::acc)
		in reverse (del (a, []))
end

(* 
double_pair [1,2,3,4,5,7,8,14,21,39,42]
*)

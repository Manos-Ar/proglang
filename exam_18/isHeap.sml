datatype heap = Empty | Node of int*heap*heap

fun isHeap Empty = true
| isHeap Node((t,r,l)) = 
	let fun get Empty _= true
	| get Node((a,b,c)) d = (d<a) andalso (get b a) andalso (get c a)
	in (get r t) andalso (get l t)
end

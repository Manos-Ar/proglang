local
structure S = BinaryMapFn(struct
	type ord_key = int
	val compare = Int.compare
end)


(*
	the following function was taken from
	http://courses.softlab.ntua.gr/pl1/2013a/Exercises/countries.sml
	and modified to keep the first two, not one, integers out of the list
*)

fun parse file =
    let
        fun readInt input = 
	    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

    	val inStream = TextIO.openIn file

	val n = readInt inStream

	val k = readInt inStream
	val _ = TextIO.inputLine inStream

	fun readInts 0 acc = acc
	  | readInts i acc = readInts (i - 1) (readInt inStream :: acc)
    in
   	(n, k, readInts n [])
    end

fun array_empty (num, s) =
    if num=0 then s
    else array_empty ((num - 1), S.insert(s, num, 0))

fun f1 (k, num, colors, s) = 
	if num = k then (colors , s)
	else if colors = [] then ([],S.empty)
	else 
		let
			val a = hd colors
			val b = S.find(s,a)
			val c = valOf(b)
			val d = S.insert(s, a, (c+1))
			val e = if c=0 then num+1 else num
		in 
			f1(k, e, tl colors, d)
		end

fun f2 (colors, s) =
	if colors = [] then ([],S.empty)
	else 
		let 
			val a = hd colors
			val b = S.find(s,a)
			val c = valOf(b)
			val d = S.insert(s, a, (c-1))
		in 
			if c-1 = 0 then (tl colors, d)
			else f2(tl colors, d)
		end

fun f3 (k, list1 , list2, s, counter) = 
	if S.numItems(s)=0 then counter
	else if list2 = [] then 
		let 
			val a = f2(list1,s)
			val b = #1 a
			val c = (length b)+1
		in 
			if c<counter andalso c>1 then c
			else counter
		end
	else
		let
			val a = f2(list1,s)
			val b = #1 a
			val c = #2 a
			val d = (length b)-(length list2)+1
			val e = if d < counter andalso d>1 then d
				else counter
			val f = f1(k, (k-1), list2, c)
			val g = #1 f
			val h = #2 f
		in
			f3(k, b, g, h, e)
	end
in
fun colors f =
	let
		val a = parse f
		val n = #1 a
		val k = #2 a
		val l = #3 a
		val min = n+1
		val b = array_empty(k, S.empty)
		val c = f1(k, 0, l, b)
		val d = f3(k, l, #1 c, #2 c, min)
		val e = Int.toString(d);
	in
		if d=min then print ("0"^"\n") else print(e^"\n")
	end
end
	

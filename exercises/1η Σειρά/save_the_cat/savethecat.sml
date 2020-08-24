local
fun comp (a:int*int, b:int*int) =
	if #1 a = #1 b andalso #2 a = #2 b then EQUAL
	else if #1 a < #1 b orelse (#1 a = #1 b andalso #2 a < #2 b) then LESS
	else GREATER

structure S = BinaryMapFn(struct
	type ord_key = int*int
	val compare = comp
end)

(*
	the stack and queue implementations were taken from
	http://www.cs.cornell.edu/courses/cs312/2004sp/lectures/rec07.html?fbclid=IwAR2IY4mXnZbBhEm4SjAw9sbQMOjeLMka24uvuNJX12ov32w25gcNN8H99xY
*)

local
signature STACK = 
    sig
      type 'a stack
      exception EmptyStack

      val empty : 'a stack
      val isEmpty : 'a stack -> bool

      val push : ('a * 'a stack) -> 'a stack
      val pop : 'a stack -> 'a stack
      val top : 'a stack -> 'a
      val map : ('a -> 'b) -> 'a stack -> 'b stack
      val app :  ('a -> unit) -> 'a stack -> unit
    end

structure Stack :> STACK = 
    struct
      type 'a stack = 'a list
      exception EmptyStack

      val empty : 'a stack = []
      fun isEmpty (l:'a list): bool = 
        (case l of
           [] => true
         | _ => false)

      fun push (x:'a, l:'a stack):'a stack = x::l
      fun pop (l:'a stack):'a stack = 
        (case l of 
           [] => raise EmptyStack
         | (x::xs) => xs)

      fun top (l:'a stack):'a = 
        (case l of
           [] => raise EmptyStack
         | (x::xs) => x)

      fun map (f:'a -> 'b) (l:'a stack):'b stack = List.map f l
      fun app (f:'a -> unit) (l:'a stack):unit = List.app f l
    end
in
signature QUEUE =
    sig
      type 'a queue
      exception EmptyQueue

      val empty : 'a queue
      val isEmpty : 'a queue -> bool

      val enqueue : ('a * 'a queue) -> 'a queue
      val dequeue : 'a queue -> 'a queue
      val front : 'a queue -> 'a

      val map : ('a -> 'b) -> 'a queue -> 'b queue
      val app : ('a -> unit) -> 'a queue -> unit      
    end

structure Queue :> QUEUE = 
    struct

      structure S = Stack

      type 'a queue = ('a S.stack * 'a S.stack)
      exception EmptyQueue

      val empty : 'a queue = (S.empty, S.empty)
      fun isEmpty ((s1,s2):'a queue) = 
        S.isEmpty (s1) andalso S.isEmpty (s2) 

      fun enqueue (x:'a, (s1,s2):'a queue) : 'a queue = 
        (S.push (x,s1), s2)

      fun rev (s:'a S.stack):'a S.stack = let
        fun loop (old:'a S.stack, new:'a S.stack):'a S.stack = 
          if (S.isEmpty (old))
            then new
          else loop (S.pop (old), S.push (S.top (old),new))
      in
        loop (s,S.empty)
      end

      fun dequeue ((s1,s2):'a queue) : 'a queue = 
        if (S.isEmpty (s2))
          then (S.empty, S.pop (rev (s1))) 
                    handle S.EmptyStack => raise EmptyQueue
        else (s1,S.pop (s2))

      fun front ((s1,s2):'a queue):'a = 
        if (S.isEmpty (s2))
          then S.top (rev (s1))
                   handle S.EmptyStack => raise EmptyQueue
        else S.top (s2)

      fun map (f:'a -> 'b) ((s1,s2):'a queue):'b queue = 
        (S.map f s1, S.map f s2)

      fun app (f:'a -> unit) ((s1,s2):'a queue):unit = 
        (S.app f s2;
         S.app f (rev (s1)))

    end
end

fun read  file = 
	let
		val ins = TextIO.openIn file
		fun f1 (ch, s, x, y, q, cat) =
			case ch of
			   NONE => (TextIO.closeIn ins; (s, q, cat))
			  |SOME(#"W") => f1(TextIO.input1 ins, S.insert(s,(x,y),(x,y,0,~2,~1,false,"")),x,y+1, Queue.enqueue((x,y), q),cat)
			  |SOME(#"A") => f1(TextIO.input1 ins, S.insert(s,(x,y),(x,y,~2,0,~1,false,"")), x,y+1,q,(x,y))
			  |SOME(#"X") => f1(TextIO.input1 ins, S.insert(s,(x,y),(x,y,~2,~2,~9,false,"")), x,y+1,q,cat)
			  |SOME(#"\n") => f1(TextIO.input1 ins, s, x+1,0, q,cat)
			  |SOME(c) => f1(TextIO.input1 ins, S.insert(s,(x,y),(x,y,~2,~2,~1,false,"")), x,y+1,q,cat)
	in
		f1(TextIO.input1 ins, S.empty, 0,0, Queue.empty, (0,0))
	end

fun check (s:(int * int * int * int * int * bool * string) S.map,(a,b):(int * int)) =
	if S.find(s,(a,b)) = NONE then false
	else
		let
			val a = S.find(s,(a,b))
			val c = valOf(a)
			val d = ((#5) c)
			val e = (d = ~9)
		in
			if e then false else true
		end

fun insert_q (s:(int * int * int * int * int * bool * string) S.map,q,key:int*int ,time,a) = 
	if a=false then (s,q)
	else
		let
			val b = valOf(S.find(s,key))
			val c = (#1 b, #2 b, time,#4 b, #5 b, #6 b, #7 b)
			val d = S.insert(s,key,c)
			val e = Queue.enqueue(key, q)
		in
			(d,e)
		end


fun flood (s:(int * int * int * int * int * bool * string) S.map, q) = 
	if Queue.isEmpty(q) then s
	else
		let
			val a = Queue.front(q)
			val b = Queue.dequeue(q)
			val c = valOf(S.find(s,a))
			val d = #5 c
			val e = if d = ~1 then (#1 c, #2 c,#3 c,#4 c, #3 c, #6 c, #7 c)
				else c
			val f = S.insert(s, a, e)
			val time = (#3 c)+1
			val g = if #5 c = ~1 then check(f,((#1 a)-1,#2 a)) else false
			val h = if #5 c = ~1 then check(f,((#1 a)+1,#2 a)) else false
			val i = if #5 c = ~1 then check(f,(#1 a,(#2 a)+1)) else false
			val j = if #5 c = ~1 then check(f,(#1 a,(#2 a)-1)) else false
			val k = insert_q(f, b, ((#1 a)-1,#2 a), time, g)
			val l = insert_q(#1 k, #2 k, ((#1 a)+1,#2 a), time, h)
			val m = insert_q(#1 l, #2 l, (#1 a,(#2 a)+1), time, i)
			val n = insert_q(#1 m, #2 m, (#1 a,(#2 a)-1), time, j)
		in
			flood(#1 n, #2 n)
		end

fun insert_c(s:(int * int * int * int * int * bool * string) S.map,q,key:(int*int),t, a, move) =
	if a=false then (s,q)
	else
		let 
			val b = valOf(S.find(s,key))
			val visited = #6 b
			val c = if (t<(#5 b) orelse (#5 b) = ~1) andalso visited = false then true else false
			val d = (#1 b, #2 b, #3 b, t, #5 b, true, move)
		in
			if c then (S.insert(s,key,d), Queue.enqueue(key,q))
			else (s,q)
		end

fun bfs (s:(int * int * int * int * int * bool * string) S.map, q,max, cat_new:(int*int)) =
	if Queue.isEmpty(q) then (s,cat_new)
	else
		let
			val a = Queue.front(q)
			val b = Queue.dequeue(q)
			val c = valOf(S.find(s,a))
			val safe = (#5 c)-1
			val d = if safe>max then (safe,a)
				else if safe = max  then if comp(a, cat_new)=LESS then (max,a)
							else (max,cat_new)
				else (max,cat_new)
			val t = (#4 c)+1
			val g = check(s,((#1 a)-1,#2 a))
			val h = check(s,((#1 a)+1,#2 a))
			val i = check(s,(#1 a,(#2 a)+1))
			val j = check(s,(#1 a,(#2 a)-1))
			val k = insert_c(s, b, ((#1 a)+1,#2 a), t, h, (#7 c)^"D")
			val l = insert_c(#1 k, #2 k, (#1 a,(#2 a)-1), t, j, (#7 c)^"L")
			val m = insert_c(#1 l, #2 l, (#1 a,(#2 a)+1), t, i, (#7 c)^"R")
			val n = insert_c(#1 m, #2 m, ((#1 a)-1,#2 a), t, g, (#7 c)^"U")
		in
			bfs(#1 n, #2 n, #1 d, #2 d)
		end


in
fun savethecat file =
	let
		val a = read file
		val s = #1 a
		val q = #2 a
		val cat = #3 a
		val s1 = flood(s, q)
		val c = valOf(S.find(s,cat))
		val d = (#1 c, #2 c, #3 c, 0, #5 c, true, #7 c)
		val s2 = S.insert(s1, cat, d)
		val q2 = Queue.enqueue(cat, Queue.empty)
		val e = bfs(s2, q2, ~2, cat)
		val cat_new = #2 e
		val f = S.find(#1 e, cat_new)
		val g = valOf(f)
		val safe = (#5 g) - 1
		val route = #7 g
		val route1 = if route = "" then "stay" else route
		val time = if safe<0 then "infinity" else Int.toString(safe);
	in
		print(time^"\n"^route1^"\n")
	end
end

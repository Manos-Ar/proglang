(*
	The Trie implementation was taken from
	https://github.com/jlao/sml-trie/blob/master/trie.sml?fbclid=IwAR1GVmhMqks4sS6lc2tCVtWdpZKH-uTSNJjo5hL585_Cr2fLQO-oANCI0d4
	and was modfied to meet the problem requirements
*)


(* Signature for dictionaries *)
(*
   For simplicity, we assume keys are strings, while stored entries
   are of arbitrary type.  This is prescribed in the signature.
   Existing entries may be "updated" by inserting a new entry
   with the same key.
*)
local

signature DICT =
sig
  type key = string                 (* concrete type *)
  type entry = key * int         (* concrete type *)

  type 'a dict                      (* abstract type *)

  val empty : 'a dict
  val lookup : 'a dict -> key -> 'a option
  val insert : int dict *  entry -> int dict
  val toString : ('a -> string) -> 'a dict -> string
end;  (* signature DICT *)

exception InvariantViolationException

structure Trie :> DICT = 
struct
  type key = string
  type entry = key * int

  datatype 'a trie = 
    Root of 'a option * 'a trie list
  | Node of 'a option * char * 'a trie list

  type 'a dict = 'a trie

  val empty = Root(NONE, nil)

  (* val lookup: 'a dict -> key -> 'a option *)
  fun lookup trie key =
    let
      (* val lookupList: 'a trie list * char list -> 'a option *)
      fun lookupList (nil, _) = NONE
        | lookupList (_, nil) = raise InvariantViolationException
        | lookupList ((trie as Node(_, letter', _))::lst, key as letter::rest) =
            if letter = letter' then lookup' (trie, rest)
            else lookupList (lst, key)
        | lookupList (_, _) =
            raise InvariantViolationException

      (*
        val lookup': 'a trie -> char list
      *)
      and lookup' (Root(elem, _), nil) = elem
        | lookup' (Root(_, lst), key) = lookupList (lst, key)
        | lookup' (Node(elem, _, _), nil) = elem
        | lookup' (Node(elem, letter, lst), key) = lookupList (lst, key)
    in
      lookup' (trie, explode key)
    end

  (*
    val insert: 'a dict * 'a entry -> 'a dict
  *)
  fun insert (trie:int trie, (key, value:int)) = 
    let
      (*
        val insertChild: 'a trie list * key * value -> 'a trie list
        Searches a list of tries to insert the value. If a matching letter 
        prefix is found, it peels of a letter from the key and calls insert'. 
        If no matching letter prefix is found, a new trie is added to the list.
        Invariants:
          * key is never nil.
          * The trie list does not contain a Root.
        Effects: none
      *)
      fun insertChild (nil, letter::nil, value:int) = 
            [ Node(SOME(value), letter, nil) ]
        | insertChild (nil, letter::rest, value) = 
            [ Node(SOME(value), letter, insertChild (nil, rest, value)) ]
        | insertChild ((trie as Node(_, letter', _))::lst, key as letter::rest, value) = 
            if letter = letter' then
              insert' (trie, rest, value) :: lst
            else
              trie :: insertChild (lst, key, value)
        | insertChild (Root(_,_)::lst, letter::rest, value) =
            raise InvariantViolationException
        | insertChild (_, nil, _) = (* invariant: key is never nil *)
            raise InvariantViolationException

      (*
        val insert': 'a trie * char list * 'a -> 'a trie
        Invariants:
          * The value is on the current branch, including potentially the current node we're on.
          * If the key is nil, assumes the current node is the destination.
        Effects: none
      *)
      and insert' (Root(_, lst), nil, value) = Root(SOME(value), lst)
        | insert' (Root(elem, lst), key, value) = Root(elem, insertChild (lst, key, value))
        | insert' (Node(_, letter, lst), nil, value) = Node(SOME(value), letter, lst)
        | insert' (Node(elem, letter, lst), key, value) = Node(SOME(valOf(elem)+1), letter, insertChild (lst, key, value))
    in
      insert'(trie, explode key, value)
    end

    (*
      val toString: ('a -> string) -> 'a dict -> string
    *)
    fun toString f trie =
      let
        val prefix = "digraph trie {\nnode [shape = circle];\n"
        val suffix = "}\n"

        (* val childNodeLetters: 'a trie list * char list -> char list *)
        fun childNodeLetters (lst, id) =
          (foldr 
            (fn (Node(_, letter, _), acc) => letter::acc
              | _ => raise InvariantViolationException) nil lst)

        (* val edgeStmt: string * string * char -> string *)
        fun edgeStmt (start, dest, lbl) =
          start ^ " -> " ^ dest ^ " [ label = " ^ Char.toString(lbl) ^ " ];\n"

        (* val allEdgesFrom: char list * char list *)
        fun allEdgesFrom (start, lst) = 
          (foldr 
            (fn (letter, acc) => 
              acc ^ edgeStmt(implode(start), implode(start @ [letter]), letter))
            "" lst)

        (* val labelNode: stirng * string -> string *)
        fun labelNode (id: string, lbl: string) =
          id ^ " [ label = \"" ^ lbl ^ "\" ];\n"

        fun toString' (Root(elem, lst), id) =
              let
                val idStr = implode(id)
                val childLetters = childNodeLetters(lst, id)
                val childStr = foldr (fn (trie, acc) => acc ^ toString'(trie, id)) "" lst
              in
                (case elem
                  of SOME(value) => 
                      labelNode (idStr, f(value)) ^ 
                      allEdgesFrom (id, childLetters)
                   | NONE => 
                      labelNode (idStr, "") ^ 
                      allEdgesFrom (id, childLetters)) ^ childStr
              end
          | toString' (Node(elem, letter, lst), id) =
              let
                val thisId = id @ [letter]
                val idStr = implode(thisId)
                val childLetters = childNodeLetters(lst, thisId)
                val childStr = foldr (fn (trie, acc) => acc ^ toString'(trie, thisId)) "" lst
              in
                (case elem
                  of SOME(value) => 
                      labelNode (idStr, f(value)) ^ 
                      allEdgesFrom (thisId, childLetters)
                   | NONE => 
                      labelNode (idStr, "") ^ 
                      allEdgesFrom (thisId, childLetters)) ^ childStr
              end
      in
        prefix ^ (toString' (trie, [#"_", #"R"])) ^ suffix
      end
end


fun read file =
	let
		fun readInt input =
			Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
		
		    	val inStream = TextIO.openIn file

			val k = readInt inStream

			val n = readInt inStream
			
			val q = readInt inStream
			val _ = TextIO.inputLine inStream

	in
		(k, n, q, inStream)
	end

fun readline (k, inStream) =
	let
		fun readInts (0,acc, inStream) = (acc, inStream)
	  	| readInts (i, acc, inStream) = readInts ((i - 1), (str(valOf(TextIO.input1 inStream))^acc), inStream)
	in
		readInts (k, "", inStream)
	end

fun read' (l, n, k, inStream) =
	if n = 0 then (l, inStream)
	else
		let
			val a = readline(k,inStream)
			val b = #2 a
			val c = #1 a
			val _ = TextIO.inputLine b
			val d = Trie.insert(l, (c, 1))
		in
			read' (d, n-1, k, b)
		end

fun read'' (l, q, k, inStream) =
	if q = 0 then (l, TextIO.closeIn inStream)
	else 
		let
			val a = readline(k,inStream)
			val b = #2 a
			val c = #1 a
			val _ = TextIO.inputLine b
			val d = c::l
		in
			read''(d, q-1, k, b)
		end

fun test file = 
	let
		val a = read file
		val b = read' (Trie.empty, #2 a, #1 a, #4 a)
		val c = read'' ([], #3 a, #1 a, #2 b)
	in 
		(#1 b, #1 c)
	end

fun power (acc,x,0)=acc
	|power(acc,x,n)=if n mod 2=0 then power (acc,x*x, n div 2)
			else power(x*acc,x*x, n div 2)

fun solve (t, l, st, count, sum, i)=
	if l=nil then (count, sum)
	else if i=1 andalso Trie.lookup t (str(hd l)) = NONE then (0, 0)
	else if i>1 andalso Trie.lookup t (st^str(hd l)) = NONE then (count, sum)
	else
		let
			val cnt = if i>1 then count else valOf(Trie.lookup t (str(hd l)))
			val a = st^str(hd l)
			val b = valOf(Trie.lookup t a)
			val c = if i>1 then(sum + (b*(power(1,2,i) -1) mod 1000000007) - (b*(power(1,2,i-1)-1) mod 1000000007)) mod 1000000007
					else (b*(power(1,2,i) -1)) mod 1000000007
		in
			solve(t, tl l, a, cnt, c, i+1)
		end

fun solve' (t, l, res) =
	if l=nil then res
	else
		let
			val a = solve(t, explode(hd l), "", 0, 0, 1)
		in
			solve' (t, tl l, a::res)
	end

fun prind (l:(int*int) list, s)=
	if l=nil then s
	else
		let
			val a = Int.toString(#1 (hd l))^" "^Int.toString(#2 (hd l))^"\n"
		in
			prind (tl l, s^a)
		end

in

fun lottery file =
	let
		val a = test file
		val b = solve'(#1 a, #2 a, [])
	in
		print(prind (b, ""))
	end
end
















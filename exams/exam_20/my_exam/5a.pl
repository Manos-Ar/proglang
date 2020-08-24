max_data(n(A, L), M) :-
	max_data(L, M1),
	(M1 > A -> M is M1; M is A).

max_data([], 0).
max_data([T|Ts],M) :-
	max_data(T,MT),
	max_data(Ts,MTs),
	(MT > MTs -> M is MT; M is MTs).
	
/*max_data(n(8, [n(4, [n(6, [n(1, [])]), n(3, [n(2, [])])]),n(5, [n(4, []), n(1, [])]),n(9, [n(5, [n(0, []), n(4, [])]), n(7, [n(2, [])])])]),M).*/


%the queue implementation was taken from
%https://stackoverflow.com/questions/31918227/difference-lists-in-prolog-and-mutable-variables

%% empty_queue(-Queue)
% make an empty queue
empty_queue(queue(0, Q, Q)).

%% queue_head(?Queue, ?Head, ?Queue0)
% Queue, with Head removed, is Queue0
queue_head(queue(s(X), [H|Q], Q0), H, queue(X, Q, Q0)).

%% queue_last(+Queue0, +Last, -Queue)
% Queue0, with Last at its back, is Queue
queue_last(queue(X, Q, [L|Q0]), L, queue(s(X), Q, Q0)).


%the following predicate was taken from
%http://courses.softlab.ntua.gr/pl1/2019a/Exercises/read_colors_SWI.pl
read_line(Stream, L) :-
	read_line_to_codes(Stream, Line),
	atom_codes(Atom, Line),
	atomic_list_concat(Atoms, ' ', Atom),
	maplist(atom_number, Atoms, L).

read_input(Stream, 0, L, L) :-
	close(Stream), !.
read_input(Stream, N, L, NL) :-
	NN is N-1,
	read_line(Stream, A),
	append([A], L, LL),
	read_input(Stream, NN, LL, NL).

read_file(F, A) :-
	open(F, read, Stream),
	read_line(Stream, [N]),
	read_input(Stream, N, [], A).

is_final([L, R, _], TL, TR) :-
	L >= TL,
	R =< TR.

is_bad([_, R, _]) :-
	R > 999999.

next([L, R, P], N) :-
	NL1 is L//2,
	NL2 is L*3+1,
	NR1 is R//2,
	NR2 is R*3+1,
	atom_concat(P, h, NP1),
	atom_concat(P, t, NP2),
	N = [[NL1, NR1, NP1], [NL2, NR2, NP2]].

hash([L, R, _], H) :-
	A is L+R,
	H is A*(A+1)//2+L.

check1(A, T) :-
	\+ is_bad(A),
	hash(A, H),
	\+ get_assoc(H, T, _).

check(T, Q, [], T, Q).
check(T, Q, [H|L], NT, NQ) :-
	(\+ check1(H, T) -> check(T, Q, L, NT, NQ); hash(H, A), put_assoc(A, T, 1, TT), queue_last(Q, H, QQ), check(TT, QQ, L, NT, NQ)), !.

solver3(T, Q, A, TL, TR, R) :-
	next(A, B),
	check(T, Q, B, NT, NQ),
	solver2(NT, NQ, TL, TR, R), !.

solver2(_, Q, _, _, Rs) :-
	empty_queue(Q),
	Rs='IMPOSSIBLE', !.
solver2(T, Q, TL, TR, Rs) :-
	queue_head(Q, [L, R, P], QQ),
	(is_final([L, R, P], TL, TR) -> Rs=P; solver3(T, QQ, [L, R, P], TL, TR, Rs)), !.

solver1([L, R, P], TL, TR, Rs) :-
	empty_queue(Q),
	empty_assoc(T),
	queue_last(Q, [L, R, P], QQ),
	hash([L, R, P], H),
	put_assoc(H, T, 1, NT),
	(is_final([L, R, P], TL, TR) -> Rs='EMPTY'; solver2(NT, QQ, TL, TR, Rs)), !.

solve([], L, L).
solve([[L, R, TL, TR]|T], M, Rs) :-
	solver1([L, R, ''], TL, TR, A),
	append([A], M, B),
	solve(T, B, Rs).

ztalloc(F, R) :-
	read_file(F, A),
	solve(A, [], R).
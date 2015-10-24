% -*- mode: prolog -*-

% min/2
min([N], N).
min([H|T], H) :- min(T, Z), H < Z.
min([H|T], Z) :- min(T, Z), H >= Z.

% max/2
max([N], N).
max([H|T], H) :- max(T, Z), H > Z.
max([H|T], Z) :- max(T, Z), H =< Z.

% uniq/1
uniq([]).
uniq([H|T]) :- \+ member(H,T), !, uniq(T).

% range/1
range([]).
range(L) :- uniq(L), min(L,1), max(L,N), !, length(L, N).

% square/2
square([H|T], L) :- length(H, L), square-aux([H|T], L, L).
square-aux([], _, 0).
square-aux([L|T], W, H) :- length(L, W), square-aux(T, W, Z), H is Z + 1.

% isLatinSquare/1

% latinSquare/1

move (state ([A | T], Z, X), state (T, [A | Z], X), a, b) :- ok (A, Z).
move (X, Y, b, a) :- move (Y, X, a, b).
  
move (state ([A | T], X, Z), state (T, X, [A | Z]), a, c) :- ok (A, Z).
move (X, Y, c, a) :- move (Y, X, a, c).

move (state (X, [A | T], Z), state (X, T, [A | Z]), b, c) :- ok (A, Z).
move (X, Y, c, b) :- move (Y, X, b, c).

transfer (X, X, []).
transfer (X, Y, [[From, To] | T]) :- move (X, Z, From, To), transfer (Z, Y, T).
  
ok (A, []).
ok (A, [H | T]) :- lt (A, H).

set (o, []).
set (s(X), Z) :- set (X, Y), append (Y, [s(X)], Z).

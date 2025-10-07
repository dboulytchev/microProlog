-- Length
length ([], o).
length ([H | T], s(L)) :- length (T, L).

-- Concatenation
append ([], L, L).
append ([H | T], L, [H | TL]) :- append (T, L, TL).

-- Inversion
reverse ([], []).
reverse ([H | T], X) :- reverse (T, RT), append (RT, [H], X).

-- Member 
member (X, [X | Y]).
member (X, [Y | Z]) :- member (X, Z).

-- Lookup
lookup (X, [pair (X, Z) | Y], Z).
lookup (X, [H | Y], Z) :- lookup (X, Y, Z).
  
-- Sorting
sort ([], []).
sort ([X | Y], T) :- sort (Y, Z), insert (X, Z, T).

-- Insertion into sorted
insert (X, [], [X]).
insert (X, [Y | Z], [X, Y | Z]) :- le (X, Y).
insert (X, [Y | Z], [Y | T]) :- le (Y, X), insert (X, Z, T).


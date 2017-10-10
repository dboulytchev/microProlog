-- Length
length ([H | T], s(L)) :- length (T, L).
length ([], o).

-- Concatenation
append ([H | T], L, [H | TL]) :- append (T, L, TL).
append ([], L, L).

-- Inversion
reverse ([H | T], X) :- reverse (T, RT), append (RT, [H], X).
reverse ([], []).

-- Member 
member (X, [X | Y]).
member (X, [Y | Z]) :- member (X, Z).

-- Sorting
sort ([X | Y], T) :- sort (Y, Z), insert (X, Z, T).
sort ([], []).

-- Insertion into sorted
insert (X, [Y | Z], [X, Y | Z]) :- le (X, Y).
insert (X, [Y | Z], [Y | T]) :- le (Y, X), insert (X, Z, T).
insert (X, [], [X]).

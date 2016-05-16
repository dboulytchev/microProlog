-- Length
length (cons (H, T), s(L)) :- length (T, L).
length (nil, o).

-- Concatenation
append (cons (H, T), L, cons (H, TL)) :- append (T, L, TL).
append (nil, L, L).

-- Inversion
revert (cons (H, T), X) :- revert (T, RT), append (RT, cons (H, nil), X).
revert (nil, nil).

-- Member 
member (X, cons(Y, Z)) :- member (X, Z).
member (X, cons(X, Y)).

-- Sorting
sort (cons (X, Y), T) :- sort (Y, Z), insert (X, Z, T).
sort (nil, nil).

-- Insertion into sorted
insert (X, cons (Y, Z), cons (X, cons (Y, Z))) :- le (X, Y).
insert (X, cons (Y, Z), cons (Y, T)) :- le (Y, X), insert (X, Z, T).
insert (X, nil, cons (X, nil)).

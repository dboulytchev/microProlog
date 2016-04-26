-- Length
length (nil, o).
length (cons (H, T), s(L)) :- length (T, L).

-- Concatenation
append (nil, L, L).
append (cons (H, T), L, cons (H, TL)) :- append (T, L, TL).

-- Inversion
revert (nil, nil).
revert (cons (H, T), X) :- append (RT, cons (H, nil), X), revert (T, RT).

-- Member 
member (X, cons(X, Y)).
member (X, cons(Y, Z)) :- member (X, Z).

-- Sorting
sort (nil, nil).
sort (cons (X, Y), T) :- sort (Y, Z), insert (X, Z, T).

-- Insertion into sorted
insert (X, nil, cons (X, nil)).
insert (X, cons (Y, Z), cons (Y, T)) :- le (Y, X), insert (X, Z, T).
insert (X, cons (Y, Z), cons (X, cons (Y, Z))) :- le (X, Y).

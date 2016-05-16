-- Addition 
add (s(X), Y, s(Z)) :- add (X, Y, Z).
add (o, X, X).

-- Multiplication 
mul (s(X), Y,  T) :- mul (X, Y, Z), add (Y, Z, T).
mul (o, X, o).

-- Equality
eq (s(X), s(Y)) :- eq (X, Y).
eq (o, o).

-- Less-or-equal 
le (X, s(Y)) :- le (X, Y).
le (X, X).

-- Greater-or-equal
ge (X, Y) :- le (Y, X).

-- Subtraction 
sub (X, Y, Z) :- add (Z, Y, X).
sub (X, Y, o) :- ge (Y, X).

-- Less/greater
gt (X, Y) :- lt (Y, X).
lt (X, Y) :- sub(Y, X, Z), ge (Z, s(o)).

-- Factorial 
fact (s(X), Fsx) :- fact (X, Fx), mul (s(X), Fx, Fsx).
fact (o, s(o)).

-- Addition 
add (o, X, X).
add (s(X), Y, s(Z)) :- add (X, Y, Z).

-- Multiplication 
mul (o, X, o).
mul (s(X), Y,  T) :- add (Y, Z, T), mul (X, Y, Z).

-- Equality
eq (o, o).
eq (s(X), s(Y)) :- eq (X, Y).

-- Less-or-equal 
le (X, X).
le (X, s(Y)) :- le (X, Y).

-- Greater-or-equal
ge (X, Y) :- le (Y, X).

-- Subtraction 
sub (X, Y, o) :- ge (Y, X).
sub (X, Y, Z) :- add (Z, Y, X).

-- Less/greater
lt (X, Y) :- sub(Y, X, Z), ge (Z, s(o)).
gt (X, Y) :- lt (Y, X).

-- Factorial 
fact (o, s(o)).
fact (s(X), Fsx) :- fact (X, Fx), mul (s(X), Fx, Fsx).

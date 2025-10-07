-- Addition 
add (o, X, X).
add (s(X), Y, s(Z)) :- add (X, Y, Z).

-- Multiplication
mul (o, X, o).  
mul (s(X), Y,  T) :- mul (X, Y, Z), add (Y, Z, T).

-- Common divisor
cd   (X, Y, Z) :- le (A, X), le (B, Y), le (Z, X), le (Z, Y), mul (Z, A, X), mul (Z, B, Y).
nld  (Z, X, Y) :- cd (X, Y, Z), cd (X, Y, Z1), lt (Z1, Z).
  
nlcd (X, Y, Z) :- cd (X, Y, Z), nld (Z, X, Y).

--
nmp (X, Y) :- nlcd (X, Y, _).
  
-- Equality
eq (o, o).
eq (s(X), s(Y)) :- eq (X, Y).

-- Less-or-equal 
le (X, X).
le (X, s(Y)) :- le (X, Y).

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

-- Dividers
divider (X, Y) :- le (X, Y), mul (X, Z, Y).

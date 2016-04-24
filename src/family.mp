sibling (X, Y) :- parent (Z, X), parent (Z, Y).
brother (X, Y) :- sibling (X, Y), male (X).
sister  (X, Y) :- sibling (X, Y), female (X).

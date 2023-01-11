father  (X, Y) :- parent (X, Y), male (X).
mother  (X, Y) :- parent (X, Y), female (X).
sibling (X, Y) :- parent (Z, X), parent (Z, Y).
brother (X, Y) :- sibling (X, Y), male (X).
sister  (X, Y) :- sibling (X, Y), female (X).
  
grandfather (X, Y) :- father (X, Z), parent (Z, Y).
grandmother (X, Y) :- mother (X, Z), parent (Z, Y).  
  
male (joe).
male (edward).  
female (kate).
female (emily).

parent (joe, emily).
parent (joe, edward).
parent (kate, emily).
parent (kate, edward).  

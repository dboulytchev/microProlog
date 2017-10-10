q(X) :- !.
q(s(o)).

p(X) :- q(X), !.
p(s(s(o))).

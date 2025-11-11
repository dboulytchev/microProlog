move (o, X, _, Z, [[X, Z]]).
move(s(M), X, Y, Z, P) :- 
    move(M, X, Z, Y, P1),
    move(o, X, Y, Z, P2),
    move(M, Y, X, Z, P3),
    append(P1, P2, Q),
    append(Q, P3, P).

color(red).
color(green).
color(white).
color(blue).

different(red, green).
different(red, white).
different(red, blue).
different(green, white).
different(green, blue).
different(white, blue).

vertex(a).
vertex(b).
vertex(c).
vertex(d).

different(a, b).
different(a, c).
different(a, d).
different(b, c).
different(b, d).
different(c, d).

different(X, Y) :- different (Y, X).
  
adj(a, b).
adj(b, c).  
adj(c, d).
adj(d, a).

adj(X, Y) :- adj(Y, X).
  
colored(V, C) :- vertex(V), color(C).

correct(Coloring) :-    
  member(pair(X, C), Coloring),  
  member(pair(Y, D), Coloring),
  adj(X, Y),
  different(C, D).

complete(Coloring) :-
  member(pair(a, Xa), Coloring),
  member(pair(b, Xb), Coloring),
  member(pair(c, Xc), Coloring),
  member(pair(d, Xd), Coloring),
  color(Xa),
  color(Xb),
  color(Xc),
  color(Xd).
  
 
  


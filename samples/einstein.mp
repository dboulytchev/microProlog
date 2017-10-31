right_to(b, a).
right_to(c, b).
right_to(d, c).
right_to(e, d).

left_to(X, Y) :- right_to(Y, X).

neighbor(X, Y) :- right_to(X, Y).
neighbor(X, Y) :- right_to(Y, X).

street([house(a, _, _, _, _, _),
        house(b, _, _, _, _, _),
        house(c, _, _, _, _, _),
        house(d, _, _, _, _, _),
        house(e, _, _, _, _, _) 
       ]).

solve(Who) :-
   street(S),

   -- The Englishman lives in the red house.
   member(house(A, english, red, _  , _  , _), S),

   --The Swede keeps dogs.
   member(house(B, swede  , _  , dog, _  , _), S),

   --The Dane drinks tea.
   member(house(C, dane   , _  , _  , tea, _), S),

   --The owner of the green house drinks coffee.
   member(house(D, _, green, _, coffee, _), S),

   --The Pall Mall smoker keeps birds.
   member(house(E, _, _, bird, _, pallMall), S),

   --The owner of the yellow house smokes Dunhills.
   member(house(F, _, yellow, _, _, dunhills), S),

   --The man in the center house drinks milk.
   member(house(c, _, _, _, milk, _), S),

   --The Norwegian lives in the first house.
   member(house(a, norwegian, _, _, _, _), S),

   --The man who smokes Blue Masters drinks bier.
   member(house(G, _, _, _, bier, blueMasters), S),

   --The German smokes Prince.
   member(house(H, german, _, _, _, prince), S),

   
   --The man who keeps horses lives next to the Dunhill smoker.
   member(house(I, _, _, horse, _, _), S),
   neighbor(I, F),

   --The Blend smoker has a neighbor who keeps cats.
   member(house(J, _, _, cat, _, _), S),
   member(house(K, _, _, _  , _, blend), S),
   neighbor(J, K),
  
   --The Norwegian lives next to the blue house.
   member(house(b, _, blue, _, _, _), S),

   --The Blend smoker has a neighbor who drinks water
   member(house(L, _, _, _, water, _), S),
   neighbor(L, K), 

   --The green house is just to the left of the white one.
   member(house(M, _, white, _, _, _), S),
   left_to(D, M),

   member(house(_, Who, _, fish, _, _), S).

   

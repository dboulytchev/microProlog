lookup (X, [[X, Y]|_], Y).
lookup (X, [_|T], Y) :- lookup (X, T, Y).

lookup_cut (X, [[X, Y]|_], Y) :- !.
lookup_cut (X, [_|T], Y) :- lookup_cut (X, T, Y).

-- requires peano!
  
split (X, o)       :- le (X, s(s(s(o)))).
split (X, s(o))    :- gt (X, s(s(s(o)))), le (X, s(s(s(s(s(s(o))))))).
split (X, s(s(o))) :- gt (s(s(s(s(s(s(o))))))).

split_cut (X, o)       :- le (X, s(s(s(o)))), !.
split_cut (X, s(o))    :- gt (X, s(s(s(o)))), le (X, s(s(s(s(s(s(o))))))), !.
split_cut (X, s(s(o))) :- gt (s(s(s(s(s(s(o))))))), !.
  
  

transfer (cons (R, Others), Src, Dst, Aux, Answer) :- 
  transfer (Others, Src, Aux, Dst, A1), 
  transfer (Others, Aux, Dst, Src, A2),
  append (A2, cons (move (R, Src, Dst), A1), Answer).
transfer (nil, Src, Dst, Aux, nil).


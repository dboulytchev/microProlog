../src/MicroProlog.opt <<EOF
load "../samples/peano.mp"
? add (o, o, X)
n
? add (o, s(o), X)
n
? add (s(o), o, X)
n
? add (s(o), s(o), X)
n
? add (s(s(o)), s(s(o)), X)
n
? mul (o, o, X)
n
? mul (s(o), s(o), X)
n
? mul (s(s(o)), s(s(o)), X)
n
? eq (o, s(o))
? eq (s(o), s(o))
n
? eq (s(s(s(o))), s(s(s(o))))
n
? lt(s(o), o)
? lt(s(o), s(s(o)))
n
? lt(s(o), s(o))
? le(s(o), s(o))
n
? le(s(o), s(s(o)))
n
? le(s(o), s)
? sub (s(o), s(o), X)
n
? sub (s(s(o)), s(o), X)
n
? sub (s(o), s(s(o)), X)
n
? fact (s(s(s(o))), X)
n
? fact (o, X)
n
? fact (s(o), X)
n
quit
EOF

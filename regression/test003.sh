../src/MicroProlog.opt <<EOF
load "../samples/peano.mp"
? add (o, X, o)
n
? add (o, X, s(o))
n
? add (s(o), X, s(s(o)))
n
? add (s(o), X, o)
? add (X, o, o)
n
? add (X, o, s(o))
n
? add (X, s(o), s(s(o)))
n
? add (X, s(o), o)
? add (X, Y, o)
n
? add (X, Y, s(o))
y
y
? add (X, s(o), Y)
n
? add (X, Y, s(X))
y
y
y
y
n
? add (X, s(X), Y)
y
y
y
y
n
? add (X, X, Y)
y
y
y
n
? add (X, Y, Z)
y
y
y
y
n
? mul (X, X, Y)
y
y
y
n
? mul (X, Y, Z)
y
y
y
n
? mul (X, Y, X)
n
? le (X, s(s(s(o))))
y
y
y
y
? lt (X, s(s(s(o))))
y
y
n
? lt (X, Y)
y
y
y
n
? fact (X, s(o))
n
? fact (X, s(s(o)))
n
? fact (X, s(s(s(s(s(s(o)))))))
n
? fact (X, s(X))
n
? fact (X, X)
n
quit
EOF

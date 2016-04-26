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
quit
EOF

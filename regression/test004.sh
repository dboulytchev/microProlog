../src/MicroProlog.opt <<EOF
load "../samples/peano.mp"
load "../samples/list.mp"
? length (X, s(s(s(o))))
n
? member (X, cons (a, cons (b, cons (c, nil))))
y
y
n
? append (X, Y, Y)
n
? append (X, Y, nil)
n
? revert (X, X)
y
y
y
y
y
y
n
? sort (X, X)
y
y
y
y
y
y
n
? sort (X, cons (o, cons (s(o), nil)))
y
n
quit
EOF

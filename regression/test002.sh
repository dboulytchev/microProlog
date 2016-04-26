../src/MicroProlog.opt <<EOF
load "../samples/list.mp"
load "../samples/peano.mp"
? length (nil, X)
n
? length (cons (s, nil), X)
n
? append (nil, cons (a, nil), X)
n
? append (cons (a, nil), nil, X)
n
? append (cons (a, cons (b, nil)), cons (c, cons (d, nil)), X)
n
? revert (nil, X)
n
? revert (cons (a, cons (b, cons (c, nil))), X)
n
? member (a, nil)
? member (a, cons (a, nil))
n
? member (a, cons (b, cons (a, nil)))
n
? sort (nil, X)
n
? sort (cons (s(s(s(o))), cons (s(s(o)), cons (s(o), cons (o, nil)))), X)
n
quit
EOF

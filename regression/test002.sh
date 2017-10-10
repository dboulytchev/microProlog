../src/MicroProlog.opt <<EOF
load "../samples/list.mp"
load "../samples/peano.mp"
? length ([], X)
n
? length ([s], X)
n
? append ([], [a], X)
n
? append ([a], [], X)
n
? append ([a, b], [c, d], X)
n
? reverse ([], X)
n
? reverse ([a, b, c], X)
n
? member (a, [])
? member (a, [a])
n
? member (a, [b, a])
n
? sort ([], X)
n
? sort ([s(s(s(o))), s(s(o)), s(o), o], X)
n
quit
EOF

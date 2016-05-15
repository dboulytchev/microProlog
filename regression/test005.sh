../src/MicroProlog.opt <<EOF
load "../samples/hanoi.mp"
load "../samples/list.mp"
? transfer (cons (x, cons (y, cons (z, cons (t, nil)))), a, b, c, X)
n
quit
EOF

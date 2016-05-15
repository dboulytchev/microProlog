../src/MicroProlog.opt <<EOF
a(X, Y) :- b(X), !, c(Y).
b(one).
b(two).
b(three).
c(one).
c(two).
c(three).
? a(X, Y)
y
y
y
quit
EOF

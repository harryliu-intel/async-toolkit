(load "yield.scm" "display")
(define x (Mpfr.New 200))
(Mpfr.SetLR x 0 'N)
(Mpfr.Div x x x 'N)

(dis (Mpfr.Format x 10 'N) dnl)

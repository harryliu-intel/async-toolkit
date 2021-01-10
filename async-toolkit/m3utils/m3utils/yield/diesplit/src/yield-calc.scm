(load "term-defs.scm")
;;(load "term.scm")
(load "poly-term.scm")


(define (area-yield A)
  (make-power-term A))

(define (modules-yield . x)
  (accumulate *-term (make-number-term 1) x))

(define (redundant-yield x N M)
  (sum-term M N (lambda(k)(*-term (make-number-term (choose N k))
                                  (*-term (^-term x k)
                                          (^-term 
                                           (+-term 1-term (*-term -1-term x))
                                           (- N k))))
                       )))
                                                   
                                   

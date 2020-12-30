(load "term-defs.scm")
(load "term.scm")

(define (choose n k)
  (let loop ((d 1)             ;; Denominator
             (u (+ n (- k) 1)) ;; nUmerator
             (p 1)             ;; Product
             )
    (if (> d k) (begin (dis "choose: " (round p) dnl) (round p))
        (loop (+ d 1) (+ u 1) (* p (/ u d))))))

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
                                                   
                                   

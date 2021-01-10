(require-modules "m3")

(load "mpfr.scm")

(Polynomial.SetPrec the-mpfr-default-precision)

(define make-power-term Polynomial.MakePower)

(define (make-number-term x) (Polynomial.MakeConstant (force-mpfr x)))

(define *-term Polynomial.Times)

(define +-term Polynomial.Plus)

(define ^-term Polynomial.IntPow)

(define (sum-term lo hi f)
  (let loop ((s (make-number-term 0))
             (i lo))
    (if (= i hi)
        (+-term (f i) s)
        (loop (+-term (f i) s)
              (+ i 1)))))

(define 1-term (make-number-term 1))
(define -1-term (make-number-term -1))
(define 0-term (make-number-term 0))

(define (make-m3-iterator poly)
  (Polynomial.Iterate poly))

(define (next-power iter)
  (let* ((tc      (rttype-typecode iter))
         (best-tc (closest-opped-supertype tc))
         (a       '(0 ()))
         (res     (modula-type-op best-tc 'call-method iter 'next a)))
    (if res
        a
        #f)))

(define (eval-yield poly Y)
  (let loop ((iter (Polynomial.Iterate poly))
             (sum (force-mpfr 0)))
    (let ((cur-mono (next-power iter)))
      (if cur-mono

          ;; we have a term
          (let ((term (*-mpfr (Y (force-mpfr (car cur-mono)))
                              (cadr cur-mono))))
            (loop iter (+-mpfr sum term)))

          ;; no more terms, just return the sum so far:
          sum
          )
      )
    )
  )


        
    
    

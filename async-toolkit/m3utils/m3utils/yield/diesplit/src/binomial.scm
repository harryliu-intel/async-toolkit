;;;;;;;;;;;;;;;;;;;;
;;
;; binomial yield
;;
;; instead of using the dependent formulas
;;

(define the-binomial-yield-model '())

(define (set-binomial-yield-model! Y)
  (set! the-binomial-yield-model Y))

(define (area-yield A) (the-binomial-yield-model A))

(define (modules-yield . x)
  (accumulate * 1 x))

(define (redundant-yield x N M)
  (sum M N (lambda(k) (* (choose N k) (pow x k) (pow (- 1 x) (- N k))))))


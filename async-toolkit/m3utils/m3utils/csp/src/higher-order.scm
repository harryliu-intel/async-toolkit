;;
;; generic higher-order Scheme stuff
;; 

(define (identity x) x)

(define (curry1 f x) (lambda(y)(f x y)))

(define (curry2 f x y) (lambda(z)(f x y z)))

(define (curry f . x) (lambda r (apply f (append x r))))

(define (make-fixpoint-func f)

  (define (iterate x)
    (let ((fx (f x)))
;;      (dis x " -> " fx dnl)
      (if (equal? fx x) x (iterate fx))))

  (lambda(x)(iterate x))
  )

;; filter ops
(define (filter-not filter-pred)
  (lambda(x)(not (filter-pred x))))

(define (filter-and filter-p0 filter-p1)
  (lambda(x)(and (filter-p0 x) (filter-p1 x))))
                         
(define (filter-or filter-p0 filter-p1)
  (lambda(x)(or (filter-p0 x) (filter-p1 x))))
                         
(define (iterate-until-false f)
  (let ((res (f)))
    (if res (iterate-until-false f) res)))

;; predicates
 (define (exists? pred? lst)
  (eval (apply or (map pred? lst)))) ;; why do we need eval?

(define (forall? pred? lst)
  (eval (apply and (map pred? lst)))) ;; why do we need eval?

(define (count-execute n f)
  ;; given n, execute (f 0) .. (f (- n 1) )

  (let loop ((i 0)
             (res '())
             )
    (if (= i n)
        (reverse res)
        (loop (+ i 1)
              (cons (f i) res))
        );;fi
    );;tel
  )
  
(define (compose f g)
  (lambda r (f (apply g r))))

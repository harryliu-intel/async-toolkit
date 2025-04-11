
(define (bigint-dbg . x)
;;    (apply dis x)
  )
  

(require-modules "fold.scm")

(define *big-m1* (BigInt.New -1))
(define *big-0*  (BigInt.New  0))
(define *big-1*  (BigInt.New  1))
(define *big-2*  (BigInt.New  2))
(define *big-tc* (rttype-typecode *big-1*))

(define (bigint? x)
  (cond ((null? x) #f)
        ((pair? x) #f)
        ((= *big-tc* (rttype-typecode x)) #t)
        (else #f)))

(define (make-big-op real-op big-op . init-val)

  (define (the-op a  b)
    (if (and (number? a) (number? b))
        (real-op a b)
        (big-op (if (bigint? a) a (BigInt.New a))
                (if (bigint? b) b (BigInt.New b)))))
  
  (lambda x
    (if (null? (cdr x))
        (if (not (null? init-val))
            (the-op (car init-val) (car x))
            (car x))
        (fold-left
         the-op
         (car x)
         (cdr x))
        )
    )
  )

(define (force-bigint x)
  (cond ((null? x) x)
        ((bigint? x) x)
        (else (BigInt.New x))))

(define (make-big-binop real-op big-op)
  (lambda(a . blst)
    (if (null? b)
    (if (and (number? a) (number? b))
                  (real-op a b)
                  (big-op (if (bigint? a) a (BigInt.New a))
                          (if (bigint? b) b (BigInt.New b)))))))
  
(define (make-big x)
  (cond ((null? x) (error "make-big of nil"))
        ((bigint? x) x)
        (else (BigInt.New x))))

(define (big<< x sa)
  (let ((pow2 (BigInt.Pow *big-2* (make-big sa))))
    (BigInt.Mul (make-big x) pow2)))

(define (big>> x sa)
  (let ((pow2 (BigInt.Pow *big-2* (make-big sa))))
    (BigInt.Div (make-big x) pow2)))

(define cartesian-product 
  (lambda (xs ys)
    (apply append (map (lambda (x) (map (lambda (y) (list x y)) ys)) xs))))

(define (dumb-binop-range op)
  (lambda (a b)
    (bigint-dbg "dumb-binop-range " op " " a " " b dnl)
    (let* ((all-pairs   (cartesian-product
                         (map force-bigint a)
                         (map force-bigint b)))

           
           (all-results (map eval (map (lambda(x)(cons op x)) all-pairs)))
           (min-res     (apply big-min all-results))
           (max-res     (apply big-max all-results)))
      (dbg "all-pairs   : " all-pairs dnl)
      (dbg "all-results : " all-results dnl)
      (dbg "min-res     : " min-res dnl)
      (dbg "max-res     : " max-res dnl)
      
      (list min-res max-res))))

(define (big-compare op) (lambda(a b) (op (BigInt.Compare
                                           (force-bigint a)
                                           (force-bigint b)) 0)))

(define big>  (big-compare >))
(define big<  (big-compare <))
(define big>= (big-compare >=))
(define big<= (big-compare <=))
(define big=  (big-compare =))


(let () ;; don't pollute the global environment
  
  (define +   (make-big-op +        BigInt.Add))
  (define *   (make-big-op *        BigInt.Mul))
  (define -   (make-big-op -        BigInt.Sub 0)) ;; special syntax
  (define /   (make-big-op /        BigInt.Div 1)) ;; special syntax
  (define min (make-big-op min      BigInt.Min))
  (define max (make-big-op max      BigInt.Max))
  (define pow (make-big-op Math.pow BigInt.Pow))   ;; associates wrong way?
      
  (define-global-symbol `big+    +  )
  (define-global-symbol `big-    -  )
  (define-global-symbol `big*    *  )
  (define-global-symbol `big/    /  )
  (define-global-symbol `big-min min)
  (define-global-symbol `big-max max)
  (define-global-symbol `big-pow pow)

  ;; ops on finite ranges...
  (define-global-symbol 'frange+ (dumb-binop-range +))
  (define-global-symbol 'frange- (dumb-binop-range -))
  (define-global-symbol 'frange* (dumb-binop-range *))
  (define-global-symbol 'frange/ (dumb-binop-range /))

  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; an extended number "xnum" is one of
;; 1. a BigInt.T
;; 2. -inf
;; 3. +inf
;; 4.  nan
;;
;; it is intended to represent any integer from the number line,
;; extended with +-inf and not-a-number.
;;

;; the following defs allow us to (eval x) for any xnum x
(define +inf '+inf)
(define -inf '-inf)

(define *xnum-special-values* '(nan -inf +inf))

(define (make-xnum x)
  (cond ((bigint? x) x)
        ((number? x) (force-bigint x))
        ((eq? '-inf x) x)
        ((eq? '+inf x) x)
        ((eq? 'nan  x) x)
        (else (error "make-xnum : not a legal xnum : " x)))
  )

(define (check-xnum x)
  (map (lambda(q)
         (if (not (or (bigint? x) (member x '(-inf +inf nan))))
             (error "not an xnum : " x)))))

(define (xnum-infinite? x) (member x '(-inf +inf)))

(define (xnum-nan? x) (eq? 'nan x))

(define (xnum-finite? x) (and (not (xnum-nan?)) (not (xnum-infinite? x))))

(define (xnum-finites? lst) (apply and (map xnum-finite? lst)))

(define (xnum-compare a b)
  (check-xnum a)
  (check-xnum b)
  (cond ((eq? a b) 0)  ;; nan = nan
        ((eq? a '-inf) -1)
        ((eq? a '+inf) +1)
        ((eq? b '-inf) +1)
        ((eq? b '+inf) -1)
        ((member 'nan (list a b)) 0)  ;; hmm.....
        (else (BigInt.Compare a b))))


(define (xnum-zero? x) (eq? x xnum-0))
(define (xnum-neg? x) (= -1 (xnum-compare x xnum-0)))
(define (xnum-pos? x) (= +1 (xnum-compare x xnum-0)))

(define xnum-0 *big-0*)
(define xnum-1 *big-1*)
(define xnum-m1 *big-m1*)

(define (xnum-+ a b)
  (let ((both (list a b)))
    (cond ((member 'nan both) 'nan)
          ((and (member '+inf both) (member '-inf both)) 'nan)
          ((member '+inf both) '+inf)
          ((member '-inf both) '-inf)
          ((eq? xnum-0 a) b)
          ((eq? xnum-0 b) a)
          (else (big+ a b)))))

(define (xnum-uneg a)
  ;; negation
  (cond ((eq? a 'nan) 'nan)
        ((eq? a '+inf) '-inf)
        ((eq? a '-inf) '+inf)
        (else (BigInt.Neg a))))

(define (xnum-sgn a)
  (cond ((eq? a 'nan)   0)
        ((eq? a '+inf) +1)
        ((eq? a '-inf) -1)
        (else (BigInt.Sign a))))

(define (xnum-abs a)
  (cond ((eq? a 'nan)   a)
        ((eq? a '+inf) '+inf)
        ((eq? a '-inf) '+inf)
        (else (BigInt.Abs a))))
        

(define (xnum-- a . b)
  (if (null? b)
      (xnum-uneg a)
      (xnum-+ a (xnum-uneg (car b)))))

(define (xnum-* a b)
  (let* ((both (list a b))
         (babs  (map xnum-abs both))
         (bsgn  (map xnum-sgn both))
         (sgn   (apply * bsgn))
         (have-neg?  (member -1 bsgn))
         (have-nan (member 'nan both)))

    (cond (have-nan 'nan)

          ((eq? a xnum-m1) (xnum-uneg b))
          
          ;; { no nan }
          (have-neg? (xnum-* (make-xnum sgn) (apply xnum-* babs)))

          ;; { no nan & no neg }
          ((member '+inf both)
           (if (member xnum-0 both) 'nan  ;; inf times 0
                                    '+inf ;; inf times finite
               ))

          ;; { no nan & no neg & no inf }
          (else (big* a b)))))

(define *xnum-special-values* '(nan +inf -inf))

(define (xnum-fin a)
  (cond ((member a *xnum-special-values*) a)
        ((eq? *big-0* a) 0)
        (else 'fin)))
  
(define (xnum-/ a b)
  (let* ((both      (list a b))
         (bsgn      (map xnum-sgn both))
         (bfin      (map xnum-fin both))
         (sgn       (apply * bsgn))
         (have-neg  (member -1 bsgn))
         (have-nan  (member 'nan both)))

    (bigint-dbg "bfin : " bfin dnl)
    
    (cond (have-nan 'nan)

          ;; { no nan }
          (have-neg (xnum-* (make-xnum sgn)
                            (apply xnum-/ (map xnum-abs both))))

          ;; { no nan & no neg }

          ((equal? bfin '(0 0)) 'nan) ;; 0/0

          ((eq? a xnum-0) xnum-0) ;; 0 / not-nan-not-zero

          ((eq? b xnum-0) '+inf) ;; not-nan-not-zero / 0
          
          ((equal? bfin '(fin fin)) (big/ a b))

          ((equal? bfin '(+inf fin)) '+inf)

          ((equal? bfin '(fin +inf)) xnum-0)

          ((equal? bfin '(+inf +inf)) 'nan)

          (else (error "xnum-/ : dunno how to divide : " a "/" b))

          )
    )
  )

(define (xnum-pow a b)
  (let* ((both (list a b))
         (bfin      (map xnum-fin both))
         (have-nan (member 'nan both)))

    (cond (have-nan 'nan) ;; should (pow 1 nan) = 1 or nan?
          
          ;; { no nan }
          ((eq? a xnum-1) xnum-1)

          ((equal? bfin '(0 0)) 'nan)

          ((equal? bfin '(fin 0)) xnum-1)

          ((xnum-neg? b) (if (eq? a xnum-0) 'nan xnum-0))

          ((equal? bfin '(-inf 0)) 'nan)

          ((equal? bfin '(+inf 0)) 'nan)

          ((equal? bfin '(fin +inf)) '+inf)

          ((equal? bfin '(0 +inf)) 'nan)

          (else (big-pow a b)))))

(define xnum-**  xnum-pow)

(define (xnum-< a b) (< (xnum-compare a b) 0))
(define (xnum-> a b) (> (xnum-compare a b) 0))
(define (xnum-<= a b) (<= (xnum-compare a b) 0))
(define (xnum->= a b) (>= (xnum-compare a b) 0))
(define (xnum-= a b) (= (xnum-compare a b) 0))

(define (xnum-msb-abs a)
  (case a
    ((nan) 'nan)
    ((-inf +inf) +inf)
    (else (BigInt.GetAbsMsb a))))

(define (xnum-clog2 x) (+ 1 (xnum-msb-abs (xnum-- x *big-1*))))

(define (xnum-max2 a b)
  (define blist (list a b))
  (cond 
        ((member '+inf blist) '+inf)
        ((member 'nan blist) 'nan)
        ((eq? a '-inf) b)
        ((eq? b '-inf) a)
        (else (BigInt.Max a b))))

(define (xnum-max a . b)
  (if (null? b) a
      (xnum-max2 a (apply xnum-max b)))
  )

(define (xnum-min2 a b)
  (define blist (list a b))
  (cond
        ((member '-inf blist) '-inf)
        ((member 'nan blist) 'nan)
        ((eq? a '+inf) b)
        ((eq? b '+inf) a)
        (else (BigInt.Min a b))))

(define (xnum-min a . b)
  (if (null? b) a
      (xnum-min2 a (apply xnum-min b)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-range min max)
  (check-xnum min)
  (check-xnum max)
  (list min max))

(define (make-point-range x) (list x x))

(define (range-min r) (car r))

(define (range-max r) (cadr r))

(define (range-neg-inf? r) (eq? '-inf (range-min r)))

(define (range-pos-inf? r) (eq? '+inf (range-max r)))

(define (range-empty? r) (xnum-< (range-max r) (range-min r)))

(define (range-infinite? r) (or (infinite-xnum? (range-min r))
                                (infinite-xnum? (range-max r))))

(define (range-nan? r) (member 'nan r))

(define (range-finite? r) (and (not range-nan? r)
                               (not (range-infinite? r))))

(define (range-eq? r s) (equal? r s))

(define (range-point? r) (and (not (range-nan? r))
                              (eq? (range-min r) (range-max r))))

(define (range-member? x r)
  (range-contains? r (make-point-range x)))

(define *range-zero*     `(,*big-0* ,*big-0*))
(define *range-one*      `(,*big-1* ,*big-1*))
(define *an-empty-range* `(,*big-1* ,*big-0*))

(define *range-pos*        `(,*big-0*     +inf))
(define *range-natural*    `(,*big-1*     +inf))
(define *range-neg-natural `(-inf    ,*big-m1*))
(define *range-neg*        `(-inf    ,*big-0* ))
(define *range-complete*   '(-inf        +inf))

(define (range-zero? r) (range-eq? r *range-zero*))

(define (range-one? f)  (range-eq? r *range-one*))

(define (range-contains? a b)
  (and (xnum-<= (range-min a) (range-min b))
       (xnum->= (range-max a) (range-max b))))

(define (make-simple-range-binop op)
  (lambda (a b)
    (bigint-dbg "simple-range-binop " op " " a " " b dnl)
    (let* ((all-pairs   (cartesian-product a b))
           (all-results (map eval (map (lambda(x)(cons op x)) all-pairs)))
           (min-res     (apply xnum-min all-results))
           (max-res     (apply xnum-max all-results)))
      (dbg "all-pairs   : " all-pairs dnl)
      (dbg "all-results : " all-results dnl)
      (dbg "min-res     : " min-res dnl)
      (dbg "max-res     : " max-res dnl)
      
      (list min-res max-res))))

(define (range-union ra rb)
  (make-range (xnum-min (range-min ra) (range-min rb))
              (xnum-max (range-max ra) (range-max rb))))
              
(define (range-intersect ra rb)
  (make-range (xnum-max (range-min ra) (range-min rb))
              (xnum-min (range-max ra) (range-max rb))))
              

(define range-+ (make-simple-range-binop xnum-+))

(define range-bin- (make-simple-range-binop xnum--))

(define (range-un- r)
  (make-range (xnum-- (cadr r)) (xnum-- (car r))))

(define (range-- a . b)
  (if (null? b) (range-un- a) (range-bin- a (car b))))

(define range-* (make-simple-range-binop xnum-*))
(define range-/ (make-simple-range-binop xnum-/))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (convert-eval-xnum-binop q)
  (eval (cond ((eq? q 'make-range)   make-range)
              (else  (symbol-append 'xnum- q)))))

(define (xnum-eval expr)
  (cond ((bigint? expr) expr)
        ((number? expr) (BigInt.New expr))
        ((string? expr) (BigInt.ScanBased expr 10 #f))

        ((and (list? expr) (= 3 (length expr)))
         (apply (convert-eval-xnum-binop (car expr))
                (map xnum-eval (cdr expr))))

        ((and (list? expr) (= 2 (length expr)) (eq? '- (car expr)))
         (xnum-uneg (xnum-eval (cadr expr))))

        ((= 2 (length expr))
         (apply (convert-eval-xnum-binop (car expr))
                (map xnum-eval (cdr expr))))
        
        (else (error "xnum-eval : can't handle : " expr))))

(define mult  1e11)
(define bmult (BigInt.New mult)) 

(define *r* #f)
(define *rr* #f)

(define (xnum-random range)
  (let* ((rr (range-intersect range *val-range*))
         (lo (car rr))
         (hi (cadr rr))
         (eq (eq? lo hi))
         (delta (if (eq? lo hi) *big-0* (xnum-- hi lo)))
         (r1    (random)))

    (cond ((< r1 0.1) (car range))  ;; may be -inf
          ((< r1 0.2) (cadr range)) ;; may be +inf
          (else
           (let* ((r2 (* mult (random)))
                  (b2 (BigInt.New (round r2)))
                  (x  (xnum-* b2 delta))
                  (y  (xnum-/ x  bmult))
                  (res (xnum-+ lo y))
                  )

             (set! *r* range)
             (set! *rr* rr)
             
             (bigint-dbg "range = " range dnl)
             (bigint-dbg "rr    = " rr dnl)
             (bigint-dbg "delta = " delta dnl)
             (bigint-dbg "eq    = " eq dnl)
             (bigint-dbg "r2    = " r2 dnl)
             (bigint-dbg "b2    = " b2 dnl)
             (bigint-dbg "x     = " x  dnl)
             (bigint-dbg "y     = " y  dnl)
             (bigint-dbg "lo    = " lo  dnl)
             (bigint-dbg "res   = " res dnl)
             res
             )
           )
          )
    )
  )

(define (range-random)
  ;; return a potentially infinite range
  (let* ((a0    (xnum-random *range-complete*))
         (a1    (xnum-random *range-complete*))
         (min   (xnum-min a0 a1))
         (max   (xnum-max a0 a1))
         (ra    (make-range min max)))
    (if (and (eq? min max)
             (xnum-infinite? min))
        ;; dont return -inf -inf or +inf +inf
        (range-random)
        ra)
    )
  )

(define *val-lo* (BigInt.New -100))
(define *val-hi* (BigInt.New +100))
(define *val-range* (make-range *val-lo* *val-hi*))



(define (make-random-range r)
  (let* ((rand (range-random))
         (res (range-intersect r rand)))

    (if (range-empty? res) (make-random-range r) res)))
    

(define (validate-range-binop op count rra rrb)
  ;; random testing of the range code

  (define xnum-op (eval (symbol-append 'xnum- op)))

  (define range-op (eval (symbol-append 'range- op)))
  
  (define (validate-specific ra rb rc count)
    ;; when the input ranges are ra, rb, draw some values and check them

    (if (= 0 count)
        'ok
        (begin
          (let* ((a (xnum-random ra))
                 (b (xnum-random rb))
                 (c (xnum-op a b)))
            (if (not (range-member? c rc))
                (error "not in range : (" xnum-op " " a " " b ") = " c
                       " NOT IN (" range-op " " ra " " rb ") = " rc)
                )
            )

          (validate-specific ra rb rc (- count 1))
          )
        )
    )

  
  (if (= 0 count)
      'ok
      (begin
        (let* ((ra    (make-random-range rra))
               (rb    (make-random-range rrb))
               (rc    (range-op ra rb))
               )

          (bigint-dbg "validate-specific " ra rb rc count dnl)
          (validate-specific ra rb rc count))
        (validate-range-binop op (- count 1) rra rrb))))




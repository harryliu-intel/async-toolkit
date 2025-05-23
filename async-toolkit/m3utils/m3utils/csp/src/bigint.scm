;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; bigint.scm
;;
;; Manipulate BigInt.T from Karl Papadantonakis's unlimited-precision
;; arithmetic library.
;;
;; Note that these types are only used internally in the compiler.
;; The Modula-3 generating back-end uses GNU's gmp/mpz format for the
;; generated code.
;;

(define (bigint-dbg . x)
;;    (apply dis x)
  )
  

;;
;; fundamental constants
;;

(define *bigm1* (BigInt.New -1))
(define *big0*  (BigInt.New  0))
(define *big1*  (BigInt.New  1))
(define *big2*  (BigInt.New  2))

(define *bigtc* (rttype-typecode *big1*)) ;; typecode of BigInt.T

(define (bigint? x)
  ;; test for bigint-ness
  (cond ((null? x) #f)
        ((pair? x) #f)
        ((= *bigtc* (rttype-typecode x)) #t)
        (else #f)))

(define (make-big-op real-op big-op . init-val)
  (define (the-op a  b)
    (if (and (number? a) (number? b))
        (real-op a b)
        (big-op (if (bigint? a) a (BigInt.New a))
                (if (bigint? b) b (BigInt.New b)))))
  
  (lambda x

;;    (dis x dnl)
    
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
  (BigInt.Shift x (BigInt.ToInteger sa)))


(define (big>> x sa)
  (BigInt.Shift x (- (BigInt.ToInteger sa))))

(define (dumb-binop-range op)
  (lambda (a b)
    (bigint-dbg "dumb-binop-range " op " " a " " b dnl)
    (let* ((all-pairs   (cartesian-product
                         (map force-bigint a)
                         (map force-bigint b)))

           
           (all-results (map eval (map (lambda(x)(cons op x)) all-pairs)))
           (min-res     (apply big-min all-results))
           (max-res     (apply big-max all-results)))
      (bigint-dbg "all-pairs   : " all-pairs dnl)
      (bigint-dbg "all-results : " all-results dnl)
      (bigint-dbg "min-res     : " min-res dnl)
      (bigint-dbg "max-res     : " max-res dnl)
      
      (list min-res max-res))))

(define (big-compare op) (lambda(a b) (op (BigInt.Compare
                                           (force-bigint a)
                                           (force-bigint b)) 0)))

(define big>  (big-compare >))
(define big<  (big-compare <))
(define big>= (big-compare >=))
(define big<= (big-compare <=))
(define big=  (big-compare =))

(define (big-neg? b) (big< b *big0*))
(define (big-zero? b) (big= b *big0*))

(define (big/ a b)
  (cond
   ((big-zero? b) *big0*) ;; weird CSP semantics for dbZ
   ((big-neg? a) (big- (big/ (big- a)       b )))
   ((big-neg? b) (big- (big/       a  (big- b))))
   (else (BigInt.Div a b))))

(define (big% a b)
  (cond ((big-zero? b) *big0*) ;; more weird CSP semantics
        ((big-neg? a) (big- (big% (big- a)       b )))
        ((big-neg? b)       (big%       a  (big- b)) )
        (else (BigInt.Mod a b))))

(define (big** a b)
  (cond
   ((big-zero? a) *big0*) ;; covers 0**0 - weird CSP semantics
   (else (BigInt.Pow a b))))

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
;;  (define-global-symbol `big/    /  )
  (define-global-symbol `big-min min)
  (define-global-symbol `big-max max)
  (define-global-symbol `big-pow pow)

  ;; ops on finite ranges...
  (define-global-symbol 'frange+ (dumb-binop-range +))
  (define-global-symbol 'frange- (dumb-binop-range -))
  (define-global-symbol 'frange* (dumb-binop-range *))
  (define-global-symbol 'frange/ (dumb-binop-range /))

  
  )

(define big| BigInt.Or) ;; |)

(define big& BigInt.And)

(define big^ BigInt.Xor)

(define (bigbits x a b)
  (let* ((w    (big+ *big1* (big- b a)))
         (sx   (big>> x a))
         (mask (big- (big<< *big1* w) *big1*)))
    (big& sx mask)))

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
;; xnums are used in the interval arithmetic to determine the potential
;; range of values resulting from a particular arithmetic expression
;; in the source code
;;

;; the following defs allow us to (eval x) for any xnum x
(define +inf '+inf)
(define -inf '-inf)
(define nan  'nan)

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

(define xnum-0 *big0*)
(define xnum-1 *big1*)
(define xnum-m1 *bigm1*)

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

(define (xnum-log2 a)
  ;; note this ROUNDS UP -- as in CSP.
  (cond ((eq? a 'nan)   a)
        ((eq? a '+inf) '+inf)
        ((eq? a '-inf) '+inf)
        (else (BigInt.New (xnum-clog2 a)))))


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
        ((eq? *big0* a) 0)
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

(define (xnum-clog2 x) (+ 1 (xnum-msb-abs (xnum-- x *big1*))))

(define (xnum-max a . b)
  (define (do2 a b)
    (define blist (list a b))
    (cond 
     ((member '+inf blist) '+inf)
     ((member 'nan blist) 'nan)
     ((eq? a '-inf) b)
     ((eq? b '-inf) a)
     (else (BigInt.Max a b))))

  (fold-left do2 a b))
    
(define (xnum-min a . b)

  (define (do2 a b)
    (define blist (list a b))
    (cond
     ((member '-inf blist) '-inf)
     ((member 'nan blist) 'nan)
     ((eq? a '+inf) b)
     ((eq? b '+inf) a)
     (else (BigInt.Min a b))))

  (fold-left do2 a b))

(define (xnum-<< x sa)
  (cond ((eq? *big0* sa) x)
        ((eq? x 'nan) 'nan)
        ((eq? x '-inf) '-inf)
        ((eq? x '+inf) '+inf)
        ((eq? sa 'nan) 'nan)
        ((eq? sa '+inf) '+inf)
        ((eq? sa '-inf) *big0*)
        (else (let* ((xlog2 (BigInt.GetAbsMsb x))
                     (rlog2 (+ (BigInt.ToInteger sa) xlog2)))
                (if (> rlog2 *maximum-size*) +inf (BigInt.Shift x (BigInt.ToInteger sa)))))))

(define (xnum->> x sa) (xnum-<< x (xnum-uneg sa)))

(define (xnum-| a b) ;; |)
  (let ((blist (list a b)))
    (cond
     ((eq? a *big0*) b)
     ((eq? b *big0*) a)
     ((member? '+inf blist) +inf)
     ((member? '-inf blist) -inf)
     ((member? 'nan blist)  nan)
     (else (BigInt.Or a b)))))

(define (xnum-^ a b) ;; |)
  (let ((blist (list a b)))
    (cond 
     ((eq? a *big0*) b)
     ((eq? b *big0*) a)
     ((member? '+inf blist) +inf)
     ((member? '-inf blist) -inf)
     ((member? 'nan blist)  nan)
     (else (BigInt.Xor a b)))))

(define (xnum-& a b) ;; |)
  (let ((blist (list a b)))
    (cond
     ((eq? a *big0*) *big0*)
     ((eq? b *big0*) *big0*)
     ((member? '+inf blist) nan)
     ((member? '-inf blist) nan)
     ((member? 'nan blist)  nan)
     (else (BigInt.And a b)))))

(define (xnum-~ a)
  (cond 
     ((eq? a *big0*) *bigm1*)
     ((eq? a *bigm1*) *big0*)
     ((eq? a '+inf) nan)
     ((eq? a '-inf) nan)
     (else (BigInt.Not a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The interval code itself.
;; We somewhat illiterately call the intervals "ranges" (which is a bit
;; confusing, sorry).
;;
;; ranges run from a minimum xnum to a maximum xnum.
;; either end of the range or both can be infinite.
;;
;; A range is empty if its min exceeds its max.
;;
;; Since we only worry about integers, all ranges are countable and the
;; issue of open vs. closed intervals does not come up.
;;

(define (make-range min max)
  (check-xnum min)
  (check-xnum max)
  (list min max))

(define (make-point-range x)
  ;; a point-range is a range of a single value
  (list x x))

(define *range1* (make-point-range *big1*))
(define *rangem1* (make-point-range *bigm1*))

(define (range-min r) (car r))

(define (range-max r) (cadr r))

(define (range-neg-inf? r) (eq? '-inf (range-min r)))

(define (range-pos-inf? r) (eq? '+inf (range-max r)))

(define (range-empty? r) (xnum-< (range-max r) (range-min r)))

(define (range-nonempty? r) (not (range-empty? r)))

(define (range-infinite? r) (or (xnum-infinite? (range-min r))
                                (xnum-infinite? (range-max r))))

(define (range-nan? r) (member 'nan r))

(define (range-finite? r) (and (not range-nan? r)
                               (not (range-infinite? r))))

(define (range-eq? r s) (equal? r s))

(define (range-point? r) (and (not (range-nan? r))
                              (eq? (range-min r) (range-max r))))

(define (range-member? x r)
  (range-contains? r (make-point-range x)))

(define *range-one*           `(,*big1* ,*big1*))
(define *an-empty-range*      `(   +inf   -inf )) 
(define *range-zero*          `(,*big0* ,*big0*))
(define *range-bit*           `(,*big0* ,*big1*))
(define *range-unsigned-byte* `(,*big0* ,(BigInt.New 255)))

;; natural, pos, and neg DO NOT include zero.
(define *range-natural*    `(,*big1*     +inf))
(define *range-pos* *range-natural*)
(define *range-neg*        `(-inf    ,*bigm1* ))
(define *range-nonneg*     `(,*big0*     +inf))

;; the entire number line:
(define *range-complete*   '(-inf        +inf))

(define (range-is-zero? r) (range-eq? r *range-zero*))

(define (range-contains-zero? r) (range-member? *big0* r))

(define (range-one? f)  (range-eq? r *range-one*))

(define (range-contains? a b)
  (and (xnum-<= (range-min a) (range-min b))
       (xnum->= (range-max a) (range-max b))))

;; operate on ranges in the default way (take cartesian product of
;; results on extreme values)
(define (make-simple-range-binop op)
  (lambda (a b)
    (bigint-dbg "simple-range-binop " op " " a " " b dnl)
    (cond ((range-empty? a) a)
          ((range-empty? b) b)
          (else
           (let* ((all-pairs   (cartesian-product a b))
                  (all-results (map eval (map (lambda(x)(cons op x)) all-pairs)))
                  (min-res     (apply xnum-min all-results))
                  (max-res     (apply xnum-max all-results)))
             (bigint-dbg "all-pairs   : " all-pairs dnl)
             (bigint-dbg "all-results : " all-results dnl)
             (bigint-dbg "min-res     : " min-res dnl)
             (bigint-dbg "max-res     : " max-res dnl)
             
             (list min-res max-res))))))

(define (range-union ra . rb)
  (define (do2 ra rb)
    (make-range (xnum-min (range-min ra) (range-min rb))
                (xnum-max (range-max ra) (range-max rb))))

  (fold-left do2 ra rb))
              
(define (range-intersection ra . rb)
  (define (do2 ra rb)
    (make-range (xnum-max (range-min ra) (range-min rb))
                (xnum-min (range-max ra) (range-max rb))))
  (fold-left do2 ra rb))

(define (range-pos? r) (range-eq? r (range-intersection r *range-pos*)))
(define (range-neg? r) (range-eq? r (range-intersection r *range-neg*)))

(define (range-nonneg? r) (not (range-neg? r)))
(define (range-nonpos? r) (not (range-pos? r)))

(define (range-lo x)  `(-inf ,x))

(define (range-hi x)  `(,x +inf))

(define (range-remove r x) ;; remove x if at top or bottom of range
  (cond ((range-empty? r) r)
        ((not (range-member? x r)) r)
        ((eq? (car r) x)  (list
                           (xnum-+ x *big1*)
                           (cadr r)))
        ((eq? (cadr r) x) (list
                           (car r)
                           (xnum-- x *big1*)))
        (else r)))

(define (split-range r x)
  ;; return list of ranges split at x
  (if (range-member? x r)
      (filter range-nonempty?
              (list (range-intersection  (range-remove (range-lo x) x) r)
                    (make-point-range x)
                    (range-intersection  (range-remove (range-hi x) x) r)))
      r)
  )

(define (negate-range r)
  (list (xnum-uneg (cadr r)) (xnum-uneg (car r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define range-+ (make-simple-range-binop xnum-+))

(define range-bin- (make-simple-range-binop xnum--))

(define (range-un- r)
  (make-range (xnum-- (cadr r)) (xnum-- (car r))))

(define (range-- a . b)
  (if (null? b) (range-un- a) (range-bin- a (car b))))



(define range-* (make-simple-range-binop xnum-*))

(define (range-extends? r x)
  ;; true iff r contains x but is not x
  (and (range-member? x r)
       (not (range-eq? r (make-point-range x)))))

(define (range-extend r x)
  ;; extend r by x
  (range-union r (make-point-range x)))

(define (range-/ a b)
  (let ((unsigned/ (make-simple-range-binop xnum-/)))
    (cond ((range-extends? a *big0*)
           (let* ((alist (split-range a *big0*))
                  (rlist (map (lambda(a)(range-/ a b)) alist)))
             (apply range-union rlist)))

          ((range-extends? b *big0*)
           (let* ((blist (split-range b *big0*))
                  (rlist (map (lambda(b)(range-/ a b)) blist)))
             (apply range-union rlist)))

          ((range-neg? a) (negate-range (range-/ (negate-range a) b)))

          ((range-neg? b) (negate-range (range-/ a (negate-range b))))

          ((and (eq? a *range-zero*) (eq? b *range-zero*))
           (unsigned/ a b ))
          
          (else (if (not (and (range-nonneg? a) (range-nonneg? b)))
                    (error
                     "attempting unsigned/ of non-positive ranges : " a b))
                (unsigned/ a b))
          )
    );tel
  )

(define (range-% a b)
  (cond ((range-extends? a *big0*)
         (let* ((alist (split-range a *big0*))
                (rlist (map (lambda(a)(range-% a b)) alist)))
           (apply range-union rlist)))
        
        ((range-extends? b *big0*)
           (let* ((blist (split-range b *big0*))
                  (rlist (map (lambda(b)(range-% a b)) blist)))
             (apply range-union rlist)))

        ((range-is-zero? a) a) ;; return zero range
        
        ((range-is-zero? b) b) ;; return zero range

        ;; result has sign of dividend

        ((range-neg? a) (negate-range (range-% (negate-range a) b)))

        ;; we get here, a is strictly positive,
        ;; b is strictly negative or positive

        ((range-neg? b) (range-% a (negate-range b)))

        ;; we get here, both ranges are positive

        (else (let* ((xa  (range-extend a *big0*))

                     (xb  (range-extend b *big0*))
                     
                     (xbr (range-remove xb (range-max xb)))
                     ;; remove the max point of xb, since the remainder
                     ;; is always at least one less than the divisor
                     )

                ;; we extend whatever is left to zero and intersect that
                ;; we don't take finer patterns into account!
                (range-intersection xa xbr)
                ))
        ))

(define (range->> a b)
  (range-<< a (negate-range b)))

(define (range-<< a b)
  (let ((pos<< (make-simple-range-binop xnum-<<)))
    (cond ((range-extends? a *big0*)
           (let* ((alist (split-range a *big0*))
                  (rlist (map (lambda(a)(range-<< a b)) alist)))
             (apply range-union rlist)))

          ((range-neg? a) (negate-range (range-<< (negate-range a) b)))


          ((and (eq? a *range-zero*) (eq? b *range-zero*))
           (pos<< a b ))
          
          (else (pos<< a b))
          )
    );tel
  )

(define (range-| a b) ;; both pos
  (define xop xnum-|) ;; |)

  (if (or (range-infinite? a) (range-infinite? b))
      *range-complete*
      (let* ((amin (range-min a))
             (amax (range-max a))
             (bmin (range-min b))
             (bmax (range-max b))
             (rmin (xnum-min amin bmin))
             (rmax (xop (xnum-setallbits amax) (xnum-setallbits bmax))))
        (make-range rmin rmax))))

(define (range-& a b) ;; both pos
  (define xop xnum-&) ;; |)

  (cond ((range-infinite? a) b)
        ((range-infinite? b) a)
        (else 
         (let* ((amin (range-min a))
                (amax (range-max a))
                (bmin (range-min b))
                (bmax (range-max b))
                (rmin (xop (xnum-clearallbits amin) (xnum-clearallbits bmin)))
                (rmax (xnum-max  ; |)
                       (xnum-setallbits amax) (xnum-setallbits bmax))))
           (make-range rmin rmax)))))

(define (range-^ a b)
  (define xop xnum-^) ;; |)

  (let* ((amin (range-min a))
         (amax (range-max a))
         (bmin (range-min b))
         (bmax (range-max b))

         (neg  (or (xnum-neg? amin) (xnum-neg? bmin)))
         (pos  (or (xnum-pos? amax) (xnum-pos? bmax)))
                
         (rmin (if neg
                   (xnum-min
                    (xnum-clearallbits amin)
                    (xnum-clearallbits bmin)
                    (xnum-~ (xnum-setallbits amax))
                    (xnum-~ (xnum-setallbits bmax))
                    )
                   *big0*))
         (rmax (if pos
                   (xnum-max
                    (xnum-setallbits amax)
                    (xnum-setallbits bmax)
                    (xnum-~ (xnum-clearallbits amin))
                    (xnum-~ (xnum-clearallbits bmin))
                    )
                   *bigm1*))
         )
    (make-range rmin rmax)))
  
(define (range-not a)
  (negate-range (range-- a *range1*)))

(define (range-pos-& a b)
  ;; a and b are positive ranges
  (make-range *big0* (xnum-min (range-max a) (range-max b))))

(define xnum-pos-& xnum-&)

(define (range-pos-| a b) ;|)
  ;; a and b are positive ranges
  (make-range *big0* (xnum-setallbits (xnum-max (range-max a) (range-max b)))))

(define xnum-pos-| xnum-|) ;)

                
(define (xnum-setallbits x)
  ;; set all bits up to and including the highest set bit in x
  (cond ((eq? x +inf) *bigm1*)

        ((xnum-pos? x)
         (xnum-- (xnum-pow *big2*
                           (BigInt.New (xnum-clog2 (xnum-+ x *big1*))))
                 *big1*))
        ((eq? x *big0*) *big0*)
        ((xnum-neg? x) *bigm1*)
        (else (error))))

(define (xnum-clearallbits x)
  ;; clear all bits up to and including the highest clear bit in x
  (cond ((eq? x -inf) *big0*) 
        ((xnum-neg? x)
         (xnum-~ (xnum-setallbits (xnum-~ x))))
        ((eq? x *big0*) *big0*)
        ((xnum-pos? x) *big0*)
        (else (error))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xnum-bits x lo hi)
  (cond ((eq? x *big0*) *big0*)
        ((eq? lo +inf) *big0*)
        ((eq? lo -inf) nan)
        ((eq? lo  nan) nan)

        ((eq? (xnum-> lo hi) #t) *big0*)
         
        ;; lo is finite
        ((not (eq? lo *big0*))
         (xnum-bits (xnum->> x lo) *big0* (xnum-+ (xnum-- hi lo) 1)))

        ;; lo is zero
        ((eq? hi +inf) x)
        ((eq? hi -inf) *big0*)
        ((eq? hi nan) nan)

        ;; hi is finite
        ((xnum-> hi (xnum-log2 x))
         x)

        (else
         (bigbits x lo hi))

        );;dnoc
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (convert-eval-xnum-binop q)
  (eval (cond ((eq? q 'make-range)   make-range)
              (else  (symbol-append 'xnum- q)))))

(define xex #f)
(define (xnum-eval expr)
  (set! xex expr)
  (cond ((bigint? expr) expr)
        ((number? expr) (BigInt.New expr))
        ((string? expr) (BigInt.ScanBased expr 10 #f))

        ((and (pair? expr)
              (eq? 'apply (car expr)))
         (let* ((proc-expr (cadr expr))
                (proc-id   (cadr proc-expr))
                (xproc     (eval (symbol-append 'xnum- proc-id)))
                (result    (apply xproc (cddr expr))))
           result))

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
  (let* ((rr (range-intersection  range *val-range*))
         (lo (car rr))
         (hi (cadr rr))
         (eq (eq? lo hi))
         (delta (if (eq? lo hi) *big0* (xnum-- hi lo)))
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
(define *val-pos* (make-range *big1* *val-hi*))
(define *val-neg* (make-range *val-lo* *bigm1*))


(define (make-random-range r)
  (let* ((rand (range-random))
         (res (range-intersection r rand)))

    (if (range-empty? res) (make-random-range r) res)))

(define *fail-op* #f)
(define *fail-ra* #f)
(define *fail-rb* #f)
(define *fail-a*  #f)
(define *fail-b*  #f)
(define *fail-rc* #f)
(define *fail-c*  #f)

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
                (begin
                  (set! *fail-op* xnum-op)
                  (set! *fail-ra* ra)
                  (set! *fail-rb* rb)
                  (set! *fail-a*  a)
                  (set! *fail-b*  b)
                  (set! *fail-rc* rc)
                  (set! *fail-c*  c)
                  
                  (error
                   (string-append
                    "not in range : (" xnum-op " " a " " b ") = " c
                    " NOT IN (" range-op " " ra " " rb ") = " rc))
                  )
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some testing stuff

;;(if #t
(define (bs16 s) (BigInt.ScanBased s 16))
(define (bf16 s) (BigInt.Format s 16))
(BigInt.Shift (bs16 "-c0edbabe") 4)
(BigInt.Shift (bs16 "-c0edbabe") -31)
;;(bf16 (BigInt.Shift (bs16 "-c0edbabe") -44))
;;)

(define bn BigInt.New)



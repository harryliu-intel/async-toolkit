(load "struct-yield.scm")
(load "tfc-yield-2.scm")
(load "simple.scm")

(define (yield-maker D)
  ;; make a Poisson yield for a given D
  ;; remember to consider n
  (lambda(A)(exp (* (- D) A))))

(define (make-one-yield yf)
  (lambda (area . optional)
    (let ((ram-area          0)
          (redundant-area    0)
          (channel-area      0)
          (serdes-area       0)
          (channel-nfactor-ratio   (/ 9 32))
          (repaired-ram-k   .6);;.6 ;;area includes ram-area
          (serdes-k         .8)
          (repair           'dummy)
          (repair-cost      'dummy)
          )
      
      (do-overrides! optional (current-environment)) 
      
      (let* ((logic-area
              (- area ram-area redundant-area channel-area serdes-area))
             
             (logic-yield
              (yf logic-area))
             
             (ram-repaired-yield
              (yf (* ram-area repaired-ram-k)))
             
             (serdes-yield
              (yf (* serdes-area serdes-k)))
             
             (channel-yield
              (yf (* channel-area channel-nfactor-ratio)))
             )
        
        (* logic-yield
           ram-repaired-yield
           channel-yield
           serdes-yield)
        ))
    )
  )

(define (compute-area-yield-directly struct f)
  (let ((repidx (find 'repair struct)))
    (if repidx
        (area-repaired-yield struct f)
        (apply f struct))))

(define (area-repaired-yield s f)
  (let ((r   (nth s (+ 1 (find 'repair s))))
        (rc  (nth s (+ 1 (find 'repair-cost s)))))
    (expt (apply f s)
          (* (- 1 r) (+ 1 rc)))))

(define (compute-redundant-yield-directly x N M)
  (sum M N (lambda(i) (* (choose N i)
                         (expt x i)
                         (expt (- 1 x) (- N i))))))
             
(define (compute-yield-directly struct f)
  (let ((key (car struct)))
    ;;(dis "doing " key dnl)
    (cond ((eq? '* key)
           (let ((nam (cadr struct))
                 (N   (caddr struct))
                 (M   (cadddr struct))
                 (sub (caddddr struct))
                 )
             (compute-redundant-yield-directly
              (compute-yield-directly sub f)
              N M)))

          ((eq? 'scale key)
           (expt (compute-yield-directly (caddr struct) f)
                 (cadr struct)))
          
          ((and (symbol? key) (number? (cadr struct)))
           (compute-area-yield-directly (cdr struct) f))

          ((and (symbol? key) (list? (cadr struct)))
           (apply *
                  (map (lambda(sub)(compute-yield-directly sub f))
                       (cdr struct))))

          (else (error "unknown spec " (stringify struct))))))

(define oy (make-one-yield (yield-maker (/ (* 30 0.10) 25.4 25.4))) )

(define (param-tfc-yield D)
  ;; this does not take n into account
  (let ((one-yield (make-one-yield (yield-maker (/ D 25.4 25.4)))))
    (compute-yield-directly (tfc-model) one-yield)
    )
  )

(define (Gamma-D alpha beta)
  (lambda (D)
    (* (/ 1 (Gamma alpha) (expt beta alpha))
       (expt D (- alpha 1))
       (exp (/ (- D) beta)))))

(define (make-D-dist D0 alpha)
  (Gamma-D alpha (/ D0 alpha)))

(define (integrate f a b)
  ;; integrate f from a to be in 2^lsteps
  (NR4p4.QromoMidpoint (make-lrfunc-obj f)
                       a
                       b))

;;(define f (make-D-dist 0.1 0.05))
;;(NR4p4.QromoMidpoint (make-lrfunc-obj f) 0.00001 10)

(define *minpow*  -100)
(define *maxpow*  +10)
(define *powstep*    3)

(define (integrate-in-pieces f)
  ;; integrate in pieces of 10^3 over the entire number line from (0,+\infty)
  ;; interior points methods are used: can handle improper integrals
  (let loop ((a *minpow*)
             (s 0)
             )
    (if (>= a *maxpow*) s
        (let* ((b (+ *powstep* a))
               (ps (integrate f (expt 10 a) (expt 10 b))))
          
          ;;(dis "integrate f : a " a " -> " b " : " ps dnl)
          (loop b
                (+ s ps))))))

    
;;(define (f D) (YieldModel.StapperGamma 0.05 (/ 0.075 0.05) D))
(define (f D) (YieldModel.StapperGamma (* 32 0.05)
                                       (/ 0.075 0.05)
                                       D))

(define (make-gamma D0 n alpha)
  (lambda (D)(YieldModel.StapperGamma (* n alpha)
                                      (/ D0 alpha)
                                      D)))

(define (pseudo-delta-05 D)
  (YieldModel.StapperGamma 10 (/ 0.05 10) D))

(define (tfc-integrand D)
  (cond ((< D 0.001) (f D))
        ((> D 10)    0)
        (else (* (f D) (param-tfc-yield D)))))

(define (tfc-integrand-poisson D)
  (cond ((< D 0.001) (pseudo-delta-05 D))
        ((> D 10)    0)
        (else (* (pseudo-delta-05 D) (param-tfc-yield D)))))

(define (limits a b f)
  (lambda (x)
    (cond ((< x a) 0)
          ((> x b) 0)
          (else (f x)))))

(define (solve-for-yield yf targ)
  ;; find the area where the yield function equals the target
  (let ((tf (lambda(A)(- targ (yf A)))))
    (solve tf 1e-100 1e+10)))

(define (integrate-yield hw-yield-func f)
  ;;
  ;; integrate the yield of a hw-yield-func, which is function of one
  ;; parameter, the defect density.  hw-yield-func shall return the
  ;; system yield of the hardware design under a Poisson yield model
  ;; with the given defect density
  ;; 
  ;; f is the p.d.f. of the defect density
  ;;

  ;; the yield function is often quite difficult to compute, so we
  ;; set it to 1 and 0 in the extremes
  (let ((min-lim (solve-for-yield hw-yield-func 0.999))
        (max-lim (solve-for-yield hw-yield-func 0.001)))

    (let ((integrand
           (lambda(D)
             (cond ((< D min-lim) (f D))
                   ((> D max-lim) 0)
                   (else (* (f D)(hw-yield-func D)))))))

      ;; integrate from 0 to +\infty
      (integrate-in-pieces integrand))))

( (make-gamma 0.05 32 3) 1.6)

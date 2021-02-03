(define (area-yield A)
  (if (= 0 A) 1 `(exp (* ,(- A) D))))

(define (modules-yield . x)
  (if (null? x) 1 `(* ,(car x) ,(apply modules-yield (cdr x)))))

(define (redundant-yield x N M)
  (let loop ((i    N)
             (res  0))
    (if (< i M)
        res
        (loop (- i 1)
              `(+ (* ,(choose N i)
                     (* (^ ,x ,i) (^ (+ 1 (* -1 ,x)) ,(- N i))))
                  ,res)))))

(define (scale-area x by)
  (cond ((null? x) '())
        ((eq? x 'D) `(* ,by D))
        ((pair? x) (cons (scale-area (car x) by) (scale-area (cdr x) by)))
        (else x)))

(define (0? x)
  (and (number? x) (= x 0)))

(define (1? x)
  (and (number? x) (= x 1)))

(define (binop? x) (member? x '(* + ^)))

(define (^ a b) (expt a b))  ;; not a standard Scheme function

(define (caddadr x) (caddr (cadr x)))

(define (simplify x)

  (if (pair? x)
      (let ((a (simplify (cadr x)))
            (b (if (binop? (car x)) (simplify (caddr x)) 0)))

        ;;(dis "op:" (car x) " a:" a " b:" b dnl)
        
        (if (and (number? a) (number? b))
            (apply (eval (car x)) (list a b))

            (case (car x)
              ((*) (cond ((0? a) 0)
                         ((0? b) 0)
                         ((1? a) b)
                         ((1? b) a)
                         ((and (number? a)
                               (pair? b)
                               (eq? '* (car b))
                               (number? (cadr b)))
                          `(* ,(* a (cadr b)) ,(simplify (caddr b))))
                         
                         ((and (pair? a) (pair? b)
                               (eq? 'exp (car a)) (eq? 'exp (car b)))
                          `(exp ,(simplify `(+ ,(cadr a) ,(cadr b)))))

                         ((and (pair? a) (pair? b) 
                               (eq? 'exp (car a))
                               (eq? '* (car b)) (pair? (cadr b))
                               (eq? 'exp (caadr b)))
                          (simplify `(* (exp (+ ,(cadr a) ,(cadadr b)))
                                        ,(caddr b))))

                        ;; ((and (pair? a) (eq? '+ (car a)))
                        ;;  (simplify `(+ (* ,(cadr a) ,b) (* ,(caddr a) ,b))))

                        ;; ((and (pair? b) (eq? '+ (car b)))
                        ;;  (simplify `(+ (* ,a ,(cadr b)) (* ,a ,(caddr b)))))
                                      
                         (else `(* ,a ,b))))
              
              ((+) (cond ((0? a) b)
                         ((0? b) a)
                         ((and (pair? a) (pair? b)
                               (eq? '* (car a)) (eq? '* (car b))
                               (number? (cadr a)) (number? (cadr b))
                               (eq? (caddr a) (caddr b)))
                          (simplify `(* ,(+ (cadr a) (cadr b)) ,(caddr a))))
                         (else `(+ ,a ,b))))
              
              ((^) (cond ((1? b) a)
                         ((0? b) 1)
                         ((and (pair? a) (eq? (car a) 'exp) (number? b))
                          (simplify `(exp (* ,b ,(cadr a)) (caddr a))))
                         
                         (else `(^ ,a ,b))))
              
              ((exp) (cond ((0? a) 0)
                           ((1? a) 1)
                           (else `(exp ,a))))

              ((log) (cond ((1? a) 0)
                           (else `(log ,a))))
              
              (else (error "unknown op " (car x)))))
        )
      x))

(define (deriv x wrt)
  (cond ((eq? x wrt) 1)
        ((pair? x)
         (let ((a (cadr x))
               (b (if (binop? (car x)) (caddr x) 0)))
         (case (car x)
           ((*) `(+ (* ,a ,(deriv b wrt)) (* ,(deriv a wrt) ,b)))
           ((+) `(+ ,(deriv a wrt) ,(deriv b wrt)))
           ((^) `(* ,x (+ (* ,(deriv b wrt) (log ,a))
                         (* ,b (* (^ ,a -1) ,(deriv a wrt))))))
           ((exp) `(* ,x ,(deriv a wrt)))
           ((log) `(* (^ ,a -1) ,(deriv a wrt)))
           (else (error "unknown op " (car x)))))
         )
        (else 0)))

;; (simplify (cadar (compute-yield (tfc-model) build-yield)))

(define (all-recs)
  (mergesort
   (compute-yield (tfc-model) build-yield)
   (lambda(r0 r1)(< (length (car r0)) (length (car r1))))))



(if #f (begin
(define all-recs
  (mergesort
   (compute-yield (tfc-model) build-yield)
   (lambda(r0 r1)(< (length (car r0)) (length (car r1))))))

(define base-rec
  (car all-recs))

(define base-rec
  (nth all-recs (- (length all-recs) 1)))

(define Pi-formula
  (scale-area (cadr base-rec) (/ 1 25.4 25.4)))

(eval
 `(define (Pi D)
    ,(simplify Pi-formula)))

(eval
 `(define (PiP D)
    (if (= 0 D) (PiP 1e-10)
        ,(simplify (deriv Pi-formula 'D)))))

(define (integrand D)
  (set! *evaluations* (+ 1 *evaluations*))
  (* -1 (PiP D) (YieldModel.GammaDistCdf 1.6 1.5 D)))

))


(define *evaluations* 0)

(define *dump-steps* 10000)

(define (dump-to-file-steps f lo hi fn steps)
  (dis "dumping function to " fn dnl)
  (let ((wr (FileWr.Open fn))
        (step (/ (- hi lo) steps)))
    
    (let loop ((p lo))
      (if (> p hi)
          (begin (Wr.Close wr) 'ok)
          (begin
            (dis p " " (f p) dnl wr)
            (loop (+ p step)))))))

(define (dump-to-file f lo hi fn)
  (dump-to-file-steps f lo hi fn *dump-steps*))


;;(dump-to-file integrand 0 100 "integrand.dat")
;;(dump-to-file PiP 0 100 "PiP.dat")
;;(dump-to-file Pi 0 100 "Pi.dat")
;;(dump-to-file (lambda(D)(YieldModel.GammaDistCdf 1.6 1.5 D)) 0 100 "F.dat")

;;(integrate integrand 0 10)

;; the following is all debug code
(define *integrand*          #f)
(define *exp-integrand*      #f)
(define *Pi-formula-0*       #f)
(define *Pip-formula*        #f)
(define *Pip*                #f)
(define *Pi*                 #f)
(define *F*                  #f)
(define *f*                  #f)
(define *a*                  #f)
(define *b*                  #f)

;; constants for algorithms
(define *the-slop*       0.0001) ;; yield slop

(define *deriv-step*     0.001)  ;; step size for numerical differentiation

(define (numerical-deriv-step f x step)
  ;; midpoint rule
  (/ (- (f (+ x (/ step 2))) (f (- x (/ step 2)))) step))

(define (numerical-deriv f)
  (lambda (x)(if (<= x (/ *deriv-step* 2))
                 (numerical-deriv-step f x x)
                 (numerical-deriv-step f x *deriv-step*))))

(define (interpolate-func f from to steps)
  (let ((step (/ (- to from) steps)))
    (let loop ((p (+ to step))
               (x '()))
      (if (< p (- from step))
          (let ((y (map f x)))
            (dis "x :" x dnl)
            (dis "y :" y dnl)
            (lambda(z)(cdr (assoc 'y (PolInt.Interpolate x y z)))))
          (loop (- p step) (cons p x))))))

(define *inmm* (/ 1 25.4 25.4))

(define (make-Pi poly)
  (let* ((Pi-formula-0   (scale-area poly *inmm*))
         (Pi-formula-1   (simplify Pi-formula-0))
         (Pi            (eval `(lambda(D) ,Pi-formula-1))))
    Pi))

(define (poly-yield poly ym . x)
  ;;(dis "poly-yield poly: " (stringify poly) " ym: " (stringify ym) dnl)
  (let* ((Pi-formula-0   (scale-area poly *inmm*))
         (Pi-formula-1   (simplify Pi-formula-0))
;;         (Pip-formula    (simplify (deriv Pi-formula-1 'D)))
         (Pi            (eval `(lambda(D) ,Pi-formula-1)))
;;         (Pip            (eval `(lambda(D) ,Pip-formula)))
;;         (Pip            (numerical-deriv Pi))

         (D0             (car ym))
         (alpha          (cadr ym))
         (n              (caddr ym))

         (D0p            (* n D0))
         (alphap         (* n alpha))
         (beta           (/ D0 alpha))

         (F              (lambda(D)(YieldModel.GammaDistCdf alphap beta D)))
         (f              (lambda(D)(YieldModel.GammaDistPdf alphap beta D)))

;;         (bp-integrand   (lambda(D)
;;                           (set! *evaluations* (+ 1 *evaluations*))
;;                           (* -1
;;                              (F D)
;;                                 (if (= D 0) (Pip 1e-10) (Pip D)))))

         (integrand     (lambda(D)
                           (set! *evaluations* (+ 1 *evaluations*))
                           (* 
                              (f D)
                              (Pi D))))
         (exp-integrand (exponential-transform integrand))
         (a             (solve (make-target (exponentiate-arg F) *the-slop*)
                               -400
                               +400))
         (b             (solve (make-target (exponentiate-arg Pi) *the-slop*)
                               -400
                               +400))
                                
         )
    ;;(dis "Pip-formula: " (stringify Pip-formula) dnl)
    (set! *Pi-formula-0* Pi-formula-0)
;;    (set! *Pip-formula* Pip-formula)
;;    (set! *Pip* Pip)
    (set! *Pi* Pi)
    (set! *integrand* integrand)
    (set! *exp-integrand* exp-integrand)
    (set! *F* F)
    (set! *f* f)
    (set! *a* a)
    (set! *b* b)
    (if (not (null? x))
        ;; do debugging
        (let ((pfx (car x)))
          (dump-to-file (exponentiate-arg Pi)
                        a b (string-append pfx "_xPi.dat"))
          (dump-to-file (exponentiate-arg f)
                        a b (string-append pfx "_xf.dat"))
          (dump-to-file (exponentiate-arg F)
                        a b (string-append pfx "_xF.dat"))
          (dump-to-file (exponential-transform integrand)
                        a b (string-append pfx "_xi.dat"))
          )
        )
    (integrate exp-integrand a b)
    )
  )

(define (exponential-transform f)
  (lambda (x)
    (let ((D (exp x)))
      (* D (f D)))))

(define (decorate-yield yr model ym)
  (let* ((config (car yr))
         (poly   (cadr yr))
         (latex  "**none**")
         (area   (compute-total-area model config))
         (y      (poly-yield poly ym)))
    (cons area (cons y (cons latex yr)))))

(define (ym D0 alpha)
  (list D0 alpha *n5-n*))

;; (poly-yield (simplify (cadar (compute-yield (tfc-model) build-yield))) (ym 0.075 0.05))

(define (multiply-funcs f g)
  (lambda(x)(* (f x) (g x))))

(define (exponentiate-arg f)
  (lambda(x)(f (exp x))))

(define (run-test)
  (set! *evaluations* 0)
  (poly-yield (simplify (cadar (compute-yield (tfc-model) build-yield))) (ym 0.075 0.01))
  (dump-to-file *Pi* 0 10 "pi.dat")
  (dump-to-file *f* 0 10 "f.dat")
  (dump-to-file *F* 0 10 "F.dat")
;;  (dump-to-file *Pip* 0 10 "pip.dat")
  )

(define (dump-extremes)
  (let* ((al-repairs (simplify (cadar (tail 1 (all-recs)))))
         (no-repairs (simplify (cadar (all-recs))))
         (al-Pi      (eval `(lambda(D) ,(scale-area al-repairs *inmm*))))
         (no-Pi      (eval `(lambda(D) ,(scale-area no-repairs *inmm*)))))
    (dump-to-file (exponentiate-arg al-Pi) *a* *b* "xAlPi.dat")
    (dump-to-file (exponentiate-arg no-Pi) *a* *b* "xNoPi.dat")))
         
        
    
(define (best-tfc-Pi) (make-Pi (simplify (cadar (tail 1 (all-recs))))))
(define (base-tfc-Pi) (make-Pi (simplify (cadar (all-recs)))))

(define (compute-poly-yield Pi D0 alpha n)
  (let* ((alphap         (* n alpha))
         (beta           (/ D0 alpha))
         (F              (lambda(D)(YieldModel.GammaDistCdf alphap beta D)))
         (f              (lambda(D)(YieldModel.GammaDistPdf alphap beta D)))
         (a             (solve (make-target (exponentiate-arg F) *the-slop*)
                               -400
                               +400))
         (b             (solve (make-target (exponentiate-arg Pi) *the-slop*)
                               -400
                              +400))
         (integrand     (lambda(D)
                          (set! *evaluations* (+ 1 *evaluations*))
                          (* 
                           (f D)
                           (Pi D))))
         (exp-integrand (exponential-transform integrand))
         
         )
    (dis "D0:" D0 " alpha:"alpha " n:"n" a:"a" b:"b dnl)
    (integrate exp-integrand a b)
))

(define (compute-yield-improvement fr to D0 alpha n)
  (let ((to-yield (compute-poly-yield to D0 alpha n))
        (fr-yield (compute-poly-yield fr D0 alpha n)))
    (dis "fr:" fr-yield " to:" to-yield dnl)
    (/ (- to-yield fr-yield) fr-yield)))

(define (D0-from-alpha D0-at at-A n)
  (lambda (alpha)
    (solve (make-target (lambda(D)(YieldModel.Stapper at-A D n alpha))
                        (YieldModel.BoseEinstein at-A D0-at n))
           1e-6 1e4)))
                        
(define (compute-yield-improvement-at-fixed-area-yield
         fr to
         D0-at at-area n
         alpha)
  (let ((D0 ((D0-from-alpha D0-at at-area n) alpha)))
    (compute-yield-improvement fr to D0 alpha n)))


        
         

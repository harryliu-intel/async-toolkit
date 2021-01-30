(define (area-yield A)
  (if (= 0 A) 1 `(exp (* ,(- A) D))))

(define (modules-yield . x)
  (if (null? x) 1 `(* ,(car x) ,(apply modules-yield (cdr x)))))

(define (redundant-yield x N M)
  (let loop ((i    N)
             (lst '()))
    (if (< i M)
        (cons '+ lst)
        (loop (- i 1)
              (cons (list '*
                          (choose N i)
                          `(* (^ ,x ,i)
                              (^ (+ 1 (* -1 ,x)) ,(- N i))))
                    lst)))))

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

(define (simplify x)

  (if (pair? x)
      (let ((a (simplify (cadr x)))
            (b (if (binop? (car x)) (simplify (caddr x)) 0)))

        ;;(dis "a:"a " b:"b dnl)
        
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
                         (else `(* ,a ,b))))
              
              ((+) (cond ((0? a) b)
                         ((0? b) a)
                         (else `(+ ,a ,b))))
              
              ((^) (cond ((1? b) a)
                         ((0? b) 1)
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

(define *evaluations* 0)

(define (integrand D)
  (set! *evaluations* (+ 1 *evaluations*))
  (* -1 (PiP D) (YieldModel.GammaDistCdf 1.6 1.5 D)))

(define *dump-steps* 1000)

(define (dump-to-file f lo hi fn)
  (let ((wr (FileWr.Open fn))
        (step (/ (- hi lo) *dump-steps*)))
    
    (let loop ((p lo))
      (if (> p hi)
          (begin (Wr.Close wr) 'ok)
          (begin
            (dis p " " (f p) dnl wr)
            (loop (+ p step)))))))


(dump-to-file integrand 0 100 "integrand.dat")
(dump-to-file PiP 0 100 "PiP.dat")
(dump-to-file Pi 0 100 "Pi.dat")
(dump-to-file (lambda(D)(YieldModel.GammaDistCdf 1.6 1.5 D)) 0 100 "F.dat")

(integrate integrand 0 10)

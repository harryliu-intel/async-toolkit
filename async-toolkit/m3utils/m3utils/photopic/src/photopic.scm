(require-modules "display")

(define c 299792458.0)
(define h 6.62607015e-34)
(define k 1.380649e-23)
(define pi (* 4 (atan 1)))

(define l0 380e-9)
(define l1 770e-9)
(define nu0 (/ c l0))
(define nu1 (/ c l1))

;; the following from tfc-yield.scm
(define (make-lrfunc-obj f)
  (let* ((func (lambda(*unused* x)(f x)))
         (min-obj (new-modula-object 'LRFunction.T `(eval . ,func))))
    min-obj))

(define (integrate f a b)
  ;; integrate f from a to be in 2^lsteps
  (NR4p4.QromoMidpoint (make-lrfunc-obj f)
                       a
                       b))

(define (make-Bnu T)
  (lambda(nu)(Blackbody.PlanckRadiance T nu)))

(define (make-Bl T)
  (lambda(l)
    (let ((nu (/ c l)))
      (/ (Blackbody.PlanckRadiance T nu) (/  c (* nu nu))))))

(define (plot f a b fn)
  (let* ((n 100)
         (step (/ (- b a) n))
         (wr (FileWr.Open fn))
         )
    (let loop ((p a))
      (if (< p b)
          (begin
            (Wr.PutText wr (string-append (stringify p)
                                          " "
                                          (stringify (f p))
                                          dnl))
            (loop (+ p step)))
          (Wr.Close wr)))))
    

(define (total-power-at-temp T)
  (integrate (make-Bl T) (/ l0 10) (* l1 100)))

(define (visible-power-at-temp T)
  (integrate (make-Bl T)  l0 l1))


(define (visible-fraction-at-temp T)
  (/ (visible-power-at-temp T) (total-power-at-temp T)))

(define (make-normal mu sigma)
  (lambda (x)
    (let* ((factor (/ 1 (* sigma (sqrt (* 2 pi)))))
           (s     (/ (- x mu) sigma))
           (y      (exp (* -0.5 s s))))
      (* factor y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lumens per watt calcs
;;

(define (calc-lumens f)
  (define (integrand x)
    (* (f x) (CieSpectrum.PhotoConv x)))

  (integrate integrand l0 l1))

(define (visible-power f)
  (integrate f l0 l1))

(define (total-power f)
  (+ (integrate f (/ l0 10) l0)
     (visible-power f)
     (integrate f l1 (* l1 100))))

(define (visible-lumens-per-watt f)
  (/ (calc-lumens f)
     (visible-power f)))

(define (total-lumens-per-watt f)
  (/ (calc-lumens f)
     (total-power f)))

(define (total-blackbody-lpW T)
  (total-lumens-per-watt (make-Bl T)))

(define (visible-blackbody-lpW T)
  (visible-lumens-per-watt (make-Bl T)))

(plot total-blackbody-lpW 1000 10000 "total_blackbody_lpw.dat")

(plot visible-blackbody-lpW 1000 10000 "visible_blackbody_lpw.dat")

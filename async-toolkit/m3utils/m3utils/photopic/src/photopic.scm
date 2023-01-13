(require-modules "display")

;;
;; code to compute energy content of a few types of visible light
;;
;; Mika Nystrom <mika@alum.mit.edu> January 2023
;;
;; This code is meant to interface with Modula-3 implementations
;; in the m3utils or cm3 repos.
;;

;;
;; some constants of nature
;; all units are SI base units unless otherwise noted
;;
(define c 299792458.0)       ;; speed of light in vacuum
(define h 6.62607015e-34)    ;; Planck's constant
(define k 1.380649e-23)      ;; Boltzmann's constant
(define pi (* 4 (atan 1)))   ;; the ratio of circumference to diameter of a circle  

(define l0 380e-9)           ;; blue limit of human vision 380nm
(define l1 770e-9)           ;; red limit of human vision  770nm
(define nu0 (/ c l0))        ;; frequency of blue
(define nu1 (/ c l1))        ;; frequency of red

;; the following from tfc-yield.scm
;; glue function to make it possible to pass Scheme functions into
;; Modula-3 code
(define (make-lrfunc-obj f)
  (let* ((func (lambda(*unused* x)(f x)))
         (min-obj (new-modula-object 'LRFunction.T `(eval . ,func))))
    min-obj))


;; helper function to integrate a function
(define (integrate f a b)
  ;; integrate f from a to be in 2^lsteps
  (NR4p4.QromoMidpoint (make-lrfunc-obj f)
                       a
                       b))

(define (make-Bnu T)
  ;; B_nu(T) 
  ;; blackbody radiance per frequency
  (lambda(nu)(Blackbody.PlanckRadiance T nu)))

(define (make-Bl T)
  ;; blackbody radiance per wavelength
  (lambda(l)
    (let ((nu (/ c l)))
      (/ (Blackbody.PlanckRadiance T nu) (/  c (* nu nu))))))

(define (plot f a b fn)
  ;; produce a file in gnuplot data format 
  ;; plot function f from a to b into file called fn
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
  ;; integrate the power of a blackbody at temp T (per square meter?)
  (integrate (make-Bl T) (/ l0 10) (* l1 100)))

(define (visible-power-at-temp T)
  ;; integrate the power of a blackbody at temp T in the visible spectrum
  ;; per square meter?
  (integrate (make-Bl T)  l0 l1))

;; note that it doesn't really matter what the area is that we integrate over
;; since all we care about is the fraction of the light that is in a particular
;; wavelength range

(define (visible-fraction-at-temp T)
  (/ (visible-power-at-temp T) (total-power-at-temp T)))

(define (make-normal mu sigma)
  ;; a Gaussian that we can use for various test purposes
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

(define (R9 l)
  ;; R9 color test spectrum
  (cdr (assoc 9 (Tcs.R l))))

(define (R n)
  ;; R(n) color test spectrum
  (lambda (l)
    (cdr (assoc n (Tcs.R l)))))

(define (make-plots)
  ;; run this to make some graphs
  (plot total-blackbody-lpW 1000 10000 "total_blackbody_lpw.dat")

  (plot visible-blackbody-lpW 1000 10000 "visible_blackbody_lpw.dat")
)

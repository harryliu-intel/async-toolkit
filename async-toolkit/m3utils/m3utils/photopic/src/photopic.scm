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

(define (reload) (load "photopic.scm"))

;; the following from tfc-yield.scm
;; glue function to make it possible to pass Scheme functions into
;; Modula-3 code
(define (make-lrfunc-obj f)
  (let* ((func (lambda(*unused* x)(f x)))
         (min-obj (new-modula-object 'LRFunction.T `(eval . ,func) `(evalHint . ,func))))
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

(define (FL n)
  (lambda (l)
    (cdr (assoc n (FlIlluminant.F l)))))

(define (make-plots)
  ;; run this to make some graphs
  (plot total-blackbody-lpW 1000 10000 "total_blackbody_lpw.dat")

  (plot visible-blackbody-lpW 1000 10000 "visible_blackbody_lpw.dat")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CIE XYZ
;;

;; a "spectrum" is a procedure of one argument that returns
;; an energy density in W/nm (units?)

(define (get-channel-integral spectrum channel)
  (let* ((c         (lambda(l) (cdr (assoc channel (CieXyz.Interpolate l) ))))
         (integrand (lambda(l) (* (c l) (spectrum l)))))
    (integrate integrand l0 l1)))

(define (xyz->Yxy xyz)
  (let* ((xint (car xyz))
         (yint (cadr xyz))
         (zint (caddr xyz))
         (sum (+ xint yint zint)))
    (list sum (/ xint sum) (/ yint sum))))

(define (calc-Yxy spectrum)
  (let* ((xint (get-channel-integral spectrum 'x))
         (yint (get-channel-integral spectrum 'y))
         (zint (get-channel-integral spectrum 'z)))
    (xyz->Yxy (list xint yint zint))))

(define (Yxy->uv Yxy)
  (let* ((x         (cadr  Yxy))
         (y         (caddr Yxy))
         (uv-denom  (+ (* -2 x) (* 12 y) 3))
         (u         (/ (* 4 x) uv-denom))
         (v         (/ (* 6 y) uv-denom)))
    (list u v)))

(define (Yxy->Yuv Yxy)
  (let* ((Y         (car Yxy))
         (x         (cadr  Yxy))
         (y         (caddr Yxy))
         (uv-denom  (+ (* -2 x) (* 12 y) 3))
         (u         (/ (* 4 x) uv-denom))
         (v         (/ (* 6 y) uv-denom)))
    (list Y u v)))

(define (calc-uv spectrum)
  ;; MacAdam 1960 UCS coordinates
  ;; note, NOT the same as 1976 CIE Luv
  ;; used for CCT calcs
  (let* ((Yxy       (calc-Yxy spectrum)))
    (Yxy->uv Yxy)))
         
(define (make-T-uv-tbl step wr)
  ;; print the UV values for different temperatures
  ;; in Modula-3 syntax, ready to be made into an Interface
  (let ((lo 0)
        (hi 20000)
        (fmt (lambda(x) (Fmt.LongReal x 'Auto 6))))
    
    (let loop ((T lo))
      (if (> T hi)
          'ok
          (begin
            (let ((uv (calc-uv (make-Bl T))))
              (dis " T { " (fmt T) ", UV { " (fmt (car uv)) ", " (fmt (cadr uv)) " } }, " dnl wr))
            (loop (+ step T)))))))

(define (make-T-uv-interface)
  (let ((wr (FileWr.Open "temp_uv.m3")))
    (make-T-uv-tbl 50 wr)
    (Wr.Close wr)))

(define (temp-uv T)
  (let* ((tuv (TempUv.Interpolate T))
         (u   (cdr (assoc 'u (assoc 'uv tuv))))
         (v   (cdr (assoc 'v (assoc 'uv tuv)))))
    (list u v)))

(define (uv-norm uv T)
  (let* ((tuv (temp-uv T))
         (du  (- (car  uv) (car  tuv)))
         (dv  (- (cadr uv) (cadr tuv)))
         (dsq (+ (* du du) (* dv dv))))
    (sqrt dsq)))
        
(define (search-T uv)
  (let* ((f  (lambda(T)(uv-norm uv T)))
         (mf (make-lrfunc-obj f))
         (T (Bracket.SchemeBrent '((a . 10) (b . 1000) (c . 20000))
                                 mf
                                 1e-6)))
    (list (cdr (assoc 'x T)) (cdr (assoc 'y T)))
    )
  )

(define (Yuv->UVW Yuv uv0)
  ;; uv0 is the white point
  (let* ((Y (car Yuv))
         (u (cadr Yuv))
         (v (caddr Yuv))

         (u0 (car uv0))
         (v0 (cadr uv0))

         (W* (- (* 25 (Math.pow Y (/ 1 3))) 17))
         (U* (* 13 W* (- u u0)))
         (V* (* 13 W* (- v v0))))
    (list U* V* W*)))

(define (scale-spectrum a fact)
  (lambda(l)(* fact (a l))))

(define (multiply-spectra a b)
  (lambda(l)(* (a l)(b l))))

(define (reflected-tcs-spectrum i illuminant-spectrum)
  
  (multiply-spectra illuminant-spectrum (R i)))


(define (normalize-spectrum spectrum)
  (let* ((Yxy0 (calc-Yxy spectrum))
         (Y    (car Yxy0)))
    (lambda(l)(* (/ 100 Y)(spectrum l)))))

(define (calc-reflected-UVW sample normalized-spectrum uv0)
  (Yuv->UVW
   (Yxy->Yuv (calc-Yxy (multiply-spectra sample normalized-spectrum)))
   uv0))

(define (uv->cd uv)
  (let* ((u (car  uv))
         (v (cadr uv))
         (c (/ (+ 4 (- u) (* -10 v)) v))
         (d (/ (+ (* 1.708 v)(* -1.481 u) 0.404) v)))
    (list c d)))

(define (adapted-uv ref-uv test-uv reflected-uv)
  (let* ((cd-r (uv->cd ref-uv))
         (cr   (car cd-r))
         (dr   (cadr cd-r))
         
         (cd-t (uv->cd test-uv))
         (ct   (car cd-t))
         (dt   (cadr cd-t))

         (cd-ti (uv->cd reflected-uv))
         (cti   (car cd-ti))
         (dti   (cadr cd-ti))

         (denom (+ 16.518 (* 1.481 (/ cr ct) cti) (* -1 (/ dr dt) dti)))
         
         (uci   (/ (+ 10.872 (* 0.404 (/ cr ct) cti) (* -4 (/ dr dt) dti))
                   denom))
                 

         (vci   (/ 5.520
                   denom)))

    (list uci vci)))

(define (euclidean-3 a b)
  (let* ((dx (- (car a) (car b)))
         (dy (- (cadr a) (cadr b)))
         (dz (- (caddr a) (caddr b))))
    (Math.sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(define debug #f)

(define (calc-cri spectrum)
  (let* ((norm-spectrum     (normalize-spectrum spectrum))
         (test-Yxy          (calc-Yxy norm-spectrum))
         (test-uv           (Yxy->uv test-Yxy))
         (ref-temp-res      (search-T test-uv))
         (ref-temp          (car ref-temp-res))
         (ref-uv            (temp-uv ref-temp))
         (norm-ref-spectrum (normalize-spectrum (make-Bl ref-temp)))
         )

    (define (calc-one tcsi)
      (let* ((sample (R tcsi))

             (ref-reflected     (multiply-spectra sample norm-ref-spectrum))
             (ref-reflected-Yxy (calc-Yxy ref-reflected))
             (ref-UVW           (Yuv->UVW (Yxy->Yuv ref-reflected-Yxy) ref-uv))
             
             (reflected-spectrum (multiply-spectra sample norm-spectrum))
             (reflected-Yxy      (calc-Yxy reflected-spectrum))
             (reflected-Yuv      (Yxy->Yuv reflected-Yxy))
             (Y                  (car reflected-Yuv))
             
             (reflected-uv       (cdr reflected-Yuv))
             (reflected-UVW      (Yuv->UVW (cons Y reflected-uv) ref-uv))
             
             (cat-uv             (adapted-uv ref-uv test-uv reflected-uv))
             (cat-UVW            (Yuv->UVW (cons Y cat-uv) ref-uv))
             (delta-EUVW         (euclidean-3 cat-UVW ref-UVW))
             (Ri                 (+ 100 (* -4.6 delta-EUVW)))
             )
        (if debug
            (dis "TCS " tcsi " reflected-Yxy " reflected-Yxy dnl
                 "TCS " tcsi " reflected-Yuv " reflected-Yuv dnl
                 "TCS " tcsi " cat-uv        " cat-uv dnl
                 "TCS " tcsi " reference-UVW " ref-UVW dnl
                 "TCS " tcsi " reflected-UVW " reflected-UVW dnl
                 "TCS " tcsi " cat-UVW       " cat-UVW dnl
                 "TCS " tcsi " delta-EUVW    " delta-EUVW dnl
                 "R" tcsi " = " Ri dnl
                 
                 ))
        Ri)
      )
      
    (if debug
        (dis "test-Yxy     " test-Yxy dnl
             "test-uv      " test-uv dnl
             "ref-temp-res " ref-temp-res dnl
             "ref-uv       " ref-uv dnl))
    (list ref-temp-res (map calc-one '(1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
    )
  )

(define (calc-specs spectrum)
  (let* ((full-cri (calc-cri spectrum))
         (ref-temp (caar full-cri))
         (Duv      (cadar full-cri))
         (ri-8     (head 8 (cadr full-cri)))
         (worst-ri (apply min ri-8))
         (cri-ra   (/ (apply + ri-8) 8)))
    (append (list cri-ra worst-ri) full-cri)))

(define (trunc-spectrum spectrum lo hi)
  (lambda(l)
    (cond ((< l lo) 0)
          ((> l hi) 0)
          (else (spectrum l)))))


         

    
         
         
    
            

    

      



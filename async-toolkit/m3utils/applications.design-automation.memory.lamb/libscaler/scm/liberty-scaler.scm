
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; liberty-scaler.scm
;;
;; Driver code for Liberty file scaling from template to final lib
;;
;; Author : mika.nystroem@intel.com
;;          March, 2022
;;
;; requires use of editliberty program from m3utils/liberty area of 
;; m3utils repo.
;;
;;
;; Example command line:
;;
;; ../../m3utils/liberty/AMD64_LINUX/editliberty -scm ../../m3utils/liberty/src/types.scm -scm ../../m3utils/liberty/src/liberty-utils.scm -scm ../scm/liberty-scaler.scm -scm ../scm/do-scale.scm -w 10 -d 4 -tech n3b -template ../templates/cdp_lamb_1w1afr_template.lib -name cdp_lamb_n3bhd_1r1w1c_4d_10b -path cdp_lamb_n3bhd_1r1w1c_4d_10b.lib -temp 25 -v 0.675 -sicorner 0 -rcorner -1.5 -ccorner -1.5 -pvtname 0p675_tt_rcworst
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *all-timings*
  '("rise_constraint"
    "fall_constraint"
    "rise_transition"
    "fall_transition"
    "cell_rise"
    "cell_fall"))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-sigma-delay-scaling       sigma) (+ 1 (/ sigma -6)))

(define (default-metal-sigma-delay-scaling sigma) (+ 1 (/ sigma -24)))

(define (default-volt-delay-scaling            v) (/ 1 v))

(define (default-temp-delay-scaling           tk) (pow tk 0.5))
;; temp in kelvin here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TECH DEFINITIONS
;;

(define n7-tech-constants
  ;; most of this is just wild guesses
  
  `((tech-name                    "n7")
    (from-n7-cap-scaling          1)

    (from-n7-delay-scaling        1)
    
    (volt-delay-scaling          ,default-volt-delay-scaling)

    (temp-delay-scaling          ,default-temp-delay-scaling)

    (proc-sigma-delay-scaling    ,default-sigma-delay-scaling)

    (metal-r-sigma-delay-scaling ,default-metal-sigma-delay-scaling)

    (metal-c-sigma-delay-scaling ,default-metal-sigma-delay-scaling)
    )
  )
    
(define n5-tech-constants
  ;; most of this is just wild guesses
  
  `((tech-name                    "n5")
    (from-n7-cap-scaling          0.8)

    (from-n7-delay-scaling        0.85)
    
    (volt-delay-scaling          ,default-volt-delay-scaling)

    (temp-delay-scaling          ,default-temp-delay-scaling)

    (proc-sigma-delay-scaling    ,default-sigma-delay-scaling)

    (metal-r-sigma-delay-scaling ,default-metal-sigma-delay-scaling)

    (metal-c-sigma-delay-scaling ,default-metal-sigma-delay-scaling)
    )
  )
    
(define n3e-tech-constants
  ;; most of this is just wild guesses
  
  `((tech-name                    "n3e")
    (from-n7-cap-scaling          0.5)

    (from-n7-delay-scaling        0.7)
    
    (volt-delay-scaling          ,default-volt-delay-scaling)

    (temp-delay-scaling          ,default-temp-delay-scaling)

    (proc-sigma-delay-scaling    ,default-sigma-delay-scaling)

    (metal-r-sigma-delay-scaling ,default-metal-sigma-delay-scaling)

    (metal-c-sigma-delay-scaling ,default-metal-sigma-delay-scaling)
    )
  )
    
(define n3b-tech-constants
  ;; most of this is just wild guesses
  
  `((tech-name                    "n3b")
    (from-n7-cap-scaling          0.55)

    (from-n7-delay-scaling        0.8)
    
    (volt-delay-scaling          ,default-volt-delay-scaling)

    (temp-delay-scaling          ,default-temp-delay-scaling)

    (proc-sigma-delay-scaling    ,default-sigma-delay-scaling)

    (metal-r-sigma-delay-scaling ,default-metal-sigma-delay-scaling)

    (metal-c-sigma-delay-scaling ,default-metal-sigma-delay-scaling)
    )
  )
    
(define n3-tech-constants n3b-tech-constants)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c->k c) (+ 273.15 c)) ;; Celsius to Kelvin

(define *base-volt*     0.750) ;; this is the template's voltage
(define *base-temp-c*  85    ) ;; this is the template's temperature in C


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; gen-lib : MAIN ENTRY POINT FOR GENERATING DERIVED LIBS
;;

(define (gen-lib template-path   ;; Liberty template
                 named           ;; name of generated library
                 path            ;; path of generated library
                 width           ;; width in bits
                 depth           ;; depth in words
                 tech            ;;
                 volt            ;; voltage in volts
                 temp            ;; temperature in degrees Celsius
                 proc-sigma      ;; proc. sigma (-3 = SSGNP, +3 = FFGNP)
                 metal-r-sigma   ;;
                 metal-c-sigma   ;;
                 pvt-name        ;; descriptive name for PVT
                 )
  (debug "generating library " named " ..." dnl)
  (debug "reading template lib " template-path " ..." dnl)
  
  (let* ((rd        (FileRd.Open template-path))
         (template  (let ((t (LibertyParse.Parse rd))) (Rd.Close rd) t))
         (worklib   (deep-copy template))
         (tech-name (cadr (assoc 'tech-name tech)))
         (wr        (FileWr.Open path))
         )

    (debug "updating headers..." dnl)
    (update-lib-name! worklib named)
    (update-lib-date! worklib)
    (update-lib-pvt!  worklib volt temp pvt-name)

    (debug "updating capacitances..." dnl)
    (let ((cap-ratio (cadr (assoc 'from-n7-cap-scaling tech))))
      (update-lib-simple-attr! worklib
                               "capacitance"
                               (lambda(x)(* x cap-ratio))))

    (let* ((base-delay-ratio   (cadr (assoc 'from-n7-delay-scaling tech)))
           (volt-delay-scaler  (cadr (assoc 'volt-delay-scaling tech)))
           (temp-delay-scaler  (cadr (assoc 'volt-delay-scaling tech)))
           (proc-delay-scaler  (cadr (assoc 'proc-sigma-delay-scaling tech)))

           (metal-r-delay-scaler
            (cadr (assoc 'metal-r-sigma-delay-scaling tech)))

           (metal-c-delay-scaler
            (cadr (assoc 'metal-c-sigma-delay-scaling tech)))

           (volt-delay-ratio   (/ (volt-delay-scaler volt)
                                  (volt-delay-scaler *base-volt*)))
           (base-k             (c->k *base-temp-c*))
           (temp-k             (c->k temp))

           (temp-delay-ratio   (/ (temp-delay-scaler temp-k)
                                  (temp-delay-scaler base-k)))

           (proc-delay-ratio   (/ (proc-delay-scaler proc-sigma)
                                  (proc-delay-scaler 0)))

           (metal-r-delay-ratio   (/ (metal-r-delay-scaler metal-r-sigma)
                                     (metal-r-delay-scaler 0)))

           (metal-c-delay-ratio   (/ (metal-c-delay-scaler metal-c-sigma)
                                     (metal-c-delay-scaler 0)))

           (overall-delay-ratio (* base-delay-ratio
                                   volt-delay-ratio
                                   temp-delay-ratio
                                   proc-delay-ratio
                                   metal-r-delay-ratio
                                   metal-c-delay-ratio)))
           
      (debug "overall-delay-ratio " overall-delay-ratio dnl)
      (debug "updating timings " *all-timings* " ..." dnl)
      
      (map (lambda(timing)(debug "updating timings " timing dnl)
                  (update-lib-timings!
                   worklib
                   timing
                   (lambda(dt group) (* dt overall-delay-ratio))))
           *all-timings*)
      )
    (debug "updating lib size..." dnl)
    (update-lib-size! worklib width depth tech-name)
      
    (debug "formatting results..." dnl)
    (Wr.PutText wr (format-comp worklib))
    (debug "closing output..." dnl)
    (Wr.Close wr)
    (debug "***lib generation done***" dnl)
    'ok
    )
  )

(define (doit-1)

  (gen-lib "cdp_lamb_1w1sr_template.lib"
           "cdp_lamb_1w1sr_32w_21b__n3_tt_0p675v_0c_typical_gen"
           "cdp_lamb_1w1sr_32w_21b__n3_tt_0p675v_0c_typical_gen.lib"
           21
           32
           n3e-tech-constants
           0.675
           0
           -3
           0
           0
           "PVT_0P675V_0C")
  )

(define (doit-2)

  (gen-lib "cdp_lamb_1w1afr_template.lib"
           "cdp_lamb_1w1afr_32w_21b__n3_tt_0p675v_0c_typical_gen"
           "cdp_lamb_1w1afr_32w_21b__n3_tt_0p675v_0c_typical_gen.lib"
           21
           32
           n3e-tech-constants
           0.675
           0
           -3
           0
           0
           "PVT_0P675V_0C")
  )

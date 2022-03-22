
(define *all-timings*
  '("rise_constraint"
    "fall_constraint"
    "rise_transition"
    "fall_transition"
    "cell_rise"
    "cell_fall"))
 

(define n3e-tech-constants
  ;; most of this is just wild guesses
  
  `((tech-name                    "n3e")
    (from-n7-cap-scaling          0.5)

    (from-n7-delay-scaling        0.7)
    
    (volt-delay-scaling          ,(lambda(v)(/ 1 v)))

    (temp-delay-scaling          ,(lambda(tk)(pow tk 0.5)))
    ;; temp in kelvin

    (proc-sigma-delay-scaling    ,(lambda(sigma)(+ 1 (/ sigma -6))))

    (metal-r-sigma-delay-scaling ,(lambda(sigma)(+ 1 (/ sigma -24))))

    (metal-c-sigma-delay-scaling ,(lambda(sigma)(+ 1 (/ sigma -24))))
    )
  )
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c->k c) (+ 273.15 c)) ;; Celsius to Kelvin

(define *base-volt*     0.750) ;; this is the template's voltage
(define *base-temp-c*  85    ) ;; this is the template's temperature in C

(define (gen-lib template-path   ;; Liberty template
                 named           ;; name of generated library
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
  (let* ((rd        (FileRd.Open template-path))
         (template  (let ((t (LibertyParse.Parse rd))) (Rd.Close rd) t))
         (worklib   (deep-copy template))
         (tech-name (cadr (assoc 'tech-name tech)))
         (wr        (FileWr.Open (string-append named ".lib")))
         )
    (update-lib-name! worklib named)
    (update-lib-date! worklib)
    (update-lib-pvt!  worklib volt temp pvt-name)

    (let ((cap-ratio (cadr (assoc 'from-n7-cap-scaling tech))))
      (update-lib-simple-attr! worklib
                               "capacitance"
                               (lambda(x)(* x cap-ratio))))

    (let* ((base-delay-ratio   (cadr (assoc 'from-n7-delay-scaling tech)))
           (volt-delay-scaler  (cadr (assoc 'volt-delay-scaling tech)))
           (temp-delay-scaler  (cadr (assoc 'volt-delay-scaling tech)))
           (proc-delay-scaler  (cadr (assoc 'proc-sigma-delay-scaling tech)))
           (metal-r-delay-scaler  (cadr (assoc 'metal-r-sigma-delay-scaling tech)))
           (metal-c-delay-scaler  (cadr (assoc 'metal-c-sigma-delay-scaling tech)))

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
           
      (dis "overall-delay-ratio " overall-delay-ratio dnl)
      
      (map (lambda(timing)(update-lib-timings!
                           worklib
                           timing
                           (lambda(dt group) (* dt overall-delay-ratio))))
           *all-timings*)
      )
    (update-lib-size! worklib width depth tech-name)
      
    (Wr.PutText wr (format-comp worklib))
    (Wr.Close wr)
    )
  )

(define (doit-1)

  (gen-lib "cdp_lamb_1w1sr_template.lib"
           "cdp_lamb_1w1sr_32w_21b__n3_tt_0p675v_0c_typical_gen"
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

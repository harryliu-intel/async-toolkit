;;
;;
;; ACC yield calcs
;;

(load "reports-21ww38.scm") ;; setup for N3 and N5
(load "reports-21ww45.scm") ;; setup for N3 and N5

(define cdm-block-area (/ 73.08 193))

(define (acc-core-model)
  `(acc-core
    (gpio1        1.26 channel-area 1.26)
    (sbc-lvds-pll 1.5  serdes-area  0.75)  ;; use serdes as stand-in for analog
    (tm-core  51.45    ram-area 31) ;; 60% RAM guess
    (pipe-top 90.81    ram-area 54) ;; 60% RAM guess
    (cdm (* cdm-spare 193 192
            (cdm-block ,cdm-block-area ram-area ,cdm-block-area)))
    (cdm-1        0.638)
    (sbb-tcu-gpio 3.6)
    (koz-spacing  0.84448    channel-area 0.84448)
    (koz-side     1.568      channel-area 1.568)
    (koz-keepout  0.98       channel-area 0.98)
    (routing-overhead 18.916 channel-area 18.916)
    (ftr-ddr 29.0304 serdes-area 29.0304)
    (ftr-ddr-pll 0.8064 serdes-area 0.8064) ;; use serdes as stand-in for analog
    )
  )

;; DDR accounting
;;
;; 1-die
;;
;;    FTR--(DDR DOUBLE-CORE DDR)--FTR
;;          ^^^^^^^^^^ ^^^^^^^^
;;         core model   core model
;;
;; 2-die
;;    FTR--(DDR CORE DDR)--(DDR CORE DDR)--FTR
;;          ^^^^^^^             ^^^^^^^^
;;          core model          core model

(define (acc-2-die)
  `(acc-2-die
    ,(acc-core-model)
    (d2d-ddr 29.0304 serdes-area 29.0304)
    (d2d-ddr-pll 0.8064 serdes-area 0.8064) ;; use serdes as stand-in for analog
    )
  )

(define (acc-1-die)
  (make-scaled-model `acc-1-die (acc-core-model) 2)
  )

              
(define (run-reports)
  (report-yields-for-params (acc-1-die) *n5-params* '())
  (report-yields-for-params (acc-2-die) *n5-params* '())
  )
  

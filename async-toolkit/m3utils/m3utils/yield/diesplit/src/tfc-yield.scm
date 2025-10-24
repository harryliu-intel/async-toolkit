; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

;;
;; yield calculator tfc-yield.scm
;; based on Pat's tfc-yield.lisp as of 1/17/2021
;;
;; modified by Mika
;; translate Lisp to Scheme
;; attempt to use yield methodology from paper
;;

(require-modules "display")
(load "yield.scm")
(load "yield-calc.scm")

(define (do-overrides! lst env)
  (let ((tc (closest-opped-supertype (rttype-typecode env))))
    
    (let loop ((p lst))
      (if (null? p)
          #t
          (begin
            (modula-type-op tc 'call-method env 'set (list (car p) (cadr p)))
            (loop (cddr p)))))))

(define (make-lrfunc-obj f)
  (let* ((func (lambda(*unused* x)(f x)))
         (min-obj (new-modula-object 'LRFunction.T `(eval . ,func))))
    min-obj))

  
(define *n5-d0*         .07)
(define *n5-nfactor*  32   );;34.25 for N7

(define (n5-area-yield area . optional)

  (let* ((ram-area 0)
         (channel-area 0)
         (nfactor *n5-nfactor*
                  ;;(- 34.25 3.25) ;;est N6
                  )
         (channel-nfactor 9)
         (d0      *n5-d0*)
         (ram-d0 (* 2 d0)) ;;area includes ram-area -- what's this?
         )

    (do-overrides! optional (current-environment)) ;; do CLisp style overrides
    
    (teg-area-yield area
                    'ram-area          ram-area
                    'channel-area      channel-area
                    'nfactor           nfactor
                    'channel-nfactor   channel-nfactor
                    'd0                d0)
    )
  )

(define 1/4 0.25)
(define 1/2 0.5)

(define (teg-area-yield area . optional)
  (let ((ram-area          0)
        (channel-area      0)
        (channel-nfactor   9)
        (repaired-ram-k   .6);;.6 ;;area includes ram-area
        )

    (do-overrides! optional (current-environment)) ;; do CLisp style overrides

    (let* ((logic-area    (- area ram-area channel-area))

           (logic-yield
            (area-yield logic-area))
           
           (ram-repaired-yield
            (area-yield (* ram-area repaired-ram-k)))
           
           (channel-yield
            (area-yield (* channel-area (/ channel-nfactor nfactor))))
           )
      
      (modules-yield logic-yield ram-repaired-yield channel-yield)
      
      ))
  )

(define (1+ x) (+ 1 x))

(define (%teg-yield area d0 nfactor alpha)
  "yield = (1 + area * d0 / 25.4**2) ** -nfactor,
   yield = (expt (1+ (/ (* area nfactor d0) alpha)) (- (* n alpha)))
   which is approximately e ** -(area * d0 * nfactor / 25.4**2)
   area is in mm**2. so the 25.4**2 converts to 
   d0 defects per square inch on each of nfactor layers"
  (let*((d0-mm    (/ d0 25.4 25.4)))
    (expt (1+ (/ (* area d0-mm) alpha)) (- (* nfactor alpha)))))

(define (tfc-yield yield-func . optional)
  (let ((summary-only?   #t))

    (do-overrides! optional (current-environment)) ;; do CLisp style overrides

  "yield improvements:
   Spare mac per quadrant
   Mark one cdm memory block bad in s/w (out of 192)
   Use spare bit in cdm for logic/wiring redundancy
   Mark cdm block bad if address logic/wiring bad to that block
   Use spare bit in stm for memory and logic defect tolerance
   Use spare wire in mac channels
   25T options:
   Even/odd pipes, still requires both tm_cores, all cdm functional
   Left/right pipes, requires only one tm_core, half cdm functional.
  "
    (let*(
          ;;sram module areas
          (stm-unit-sram-area (* .0373830  .1264480))
          (ppu-unit-tcam-area (* .1292850  .0382200))

          ;;ppu params
          (npipes                16)
          (ppu-nstages           13)
          (ppu-num-stm-rows      12) ;;ingress + egress
          (ppu-num-ram-cols/stg   8)
          (num-tcams/stage       16)
          (stm-row-y           (/ 1.059424 8))
          (parde-x              .98)  ;;leftover x in 4.7mm after 13ppu stages.
          ;;just an estimate
          (ppu-x            .339456);;per stage
          (ppu-core-y       .3234)
          (tcam-array-y     .40656)
          
          ;;cdm params
          (cdm-unit-sram-x  .0233070)
          (cdm-unit-sram-y  .1222480)
          (cdm-unit-sram-area (* cdm-unit-sram-x cdm-unit-sram-y))
          (num-cdm-ram-cols 128)
          (num-cdm-ram-rows/subwd 6)
          (num-cdm-ram-subwords 34)
          (num-cdm-ram-rows (* num-cdm-ram-rows/subwd num-cdm-ram-subwords))
          (num-cdm-unit-rams  (* num-cdm-ram-rows num-cdm-ram-cols))
          (cdm-x-overhead   (/ (* 50 .204) (* 2 23.652)))
          ;; 0.21562658 50 gates per rampair
          (cdm-y-overhead   (/ (+  (* 5 52)(* 1 148))
                               (* 6 588) 1.0))
          ;; 5x52G + 1x148G per 6 ram rows = 0.11564626 y overhead
          (cdm-area-mlpr    (* (1+ cdm-x-overhead)(1+ cdm-y-overhead)))
          (cdm-x      (* cdm-unit-sram-x num-cdm-ram-cols (1+ cdm-x-overhead)))
          (cdm-y      (* cdm-unit-sram-y num-cdm-ram-rows (1+ cdm-y-overhead)))
          (cdm-sram-area    (* cdm-unit-sram-area num-cdm-unit-rams))
          (cdm-area   (* cdm-sram-area cdm-area-mlpr))
          (cdm-logic-area   (- cdm-area cdm-sram-area))

          ;;tm core
          (tm-core-area   (* 6.3 6.9))
          (tm-core-ramfraction  .33)  ;;just a guess

          ;;mac
          (mac-area   (* 1.5 .675))

          ;;mac channels
          (mac-channel-lr-width .6) ;;est 30mm tall
          (mac-channel-tb-width .5) ;;est 20mm wide

          ;;serdes
          (serdes-area    (+ (* 32 3.7 1.5)(* 8 5 1.5)))
          (serdes-kfactor   .8)

          ;;misc blocks
          (misc-areas   (+ 2.72         ;;host+sbc
                           3.4          ;;risc
                           (* 1.5 .675) ;;eth800G
                           .45          ;;ethcpu
                           .45))        ;;tcu
          
          (gpio-area    (+ 1.96 5.69))

          ;;sizes and yields
          ;;misc/gpio
          (misc-yield
           (yield-func misc-areas
                       'ram-area (* misc-areas .25)
                       'channel-area (* misc-areas .25)))
          
          (gpio-yield     (yield-func gpio-area 'channel-area gpio-area))
          
          ;;serdes
          (serdes-yield   (yield-func (* serdes-area serdes-kfactor)))
          
          ;;mac
          (onemac-yield   (yield-func mac-area))
          (mac-yield      (yield-func (* 64 mac-area)))

          ;;mac channel
          (mac-channel-area (* 2 (+ (* mac-channel-lr-width 30)
                                    (* mac-channel-tb-width 20))))
          (mac-channel-yield  (yield-func mac-channel-area
                                          'channel-area mac-channel-area))

          ;;ppu, parde
          (ppu-ysize
           (+ (* ppu-num-stm-rows stm-row-y)(* 2 ppu-core-y) tcam-array-y))
          (parde-area   (* parde-x ppu-ysize npipes))
          (parde-ramfraction  .33)
          (parde-yield
           (yield-func parde-area 'ram-area (* parde-area parde-ramfraction)))

          (ppu-core-area    (* ppu-nstages 2 npipes ppu-x ppu-core-y))
          (ppu-core-yield   (yield-func ppu-core-area))
          
          ;;tcam
          (tcam-area    (* tcam-array-y ppu-x ppu-nstages npipes))
          (tcam-memory-area
           (* ppu-unit-tcam-area num-tcams/stage ppu-nstages npipes))
          (tcam-array-yield (yield-func tcam-area 'ram-area tcam-memory-area))

          ;;stm
          (stm-area   (* ppu-nstages ppu-num-stm-rows npipes ppu-x stm-row-y))
          (stm-ram-area
           (* stm-unit-sram-area ppu-num-stm-rows ppu-num-ram-cols/stg
              ppu-nstages npipes))
          (stm-ram-array-efficiency (/ stm-ram-area stm-area))
          (stm-ram-yield    (yield-func stm-ram-area 'ram-area stm-ram-area))
          (stm-logic-yield  (yield-func (- stm-area stm-ram-area)))
          (stm-yield
           (modules-yield stm-ram-yield
                          stm-logic-yield))
          (ppu-pipe-yield
           (modules-yield parde-yield
                          stm-yield
                          ppu-core-yield
                          tcam-array-yield))

          ;;tm core
          (tm-core-yield
           (yield-func (* tm-core-area 2)
                       'ram-area (* tm-core-area 2 tm-core-ramfraction)))

          ;;cdm
          (cdm-sram-yield
           (yield-func cdm-sram-area 'ram-area cdm-sram-area))
          
          (cdm-logic-yield  (yield-func cdm-logic-area))
          
          (cdm-yield    (modules-yield cdm-sram-yield
                                       cdm-logic-yield))

          ;;mac redundancy
          (mac-withspare-yield  (redundant-yield
                                 (redundant-yield onemac-yield 17 16)
                                 4 4))
          
;;          (macspare-deltayield  (/ mac-yield-withspare mac-yield))
          
          ;;mac channel spare wire
          (mac-channel-repairable .95)
;;          (mac-channel-repaired-yield
;;           (- 1 (* (- 1 mac-channel-yield) (- 1 mac-channel-repairable))))

          (mac-channel-repaired-yield
           (scale-area mac-channel-yield (- 1 mac-channel-repairable)))
          
;;          (mac-channel-deltayield
;;           (/ mac-channel-repaired-yield mac-channel-yield))

          ;;cdm memory
          (cdm-numblocks    (* num-cdm-ram-rows/subwd num-cdm-ram-cols 1/4 1.0))

          (cdm-oneblock-yield
           (scale-area cdm-sram-yield (/ cdm-numblocks)))
          
          (cdm-sram-yield-1bad
           (redundant-yield cdm-oneblock-yield
                            (+ cdm-numblocks 1)
                            cdm-numblocks))
          
;;          (cdm-sram-deltayield  (/ cdm-sram-yield-1bad cdm-sram-yield))

          (cdm-logic-repairable .95)
;;          (cdm-logic-repaired-yield
;;           (- 1 (* (- 1 cdm-logic-yield) (- 1 cdm-logic-repairable))))

          (cdm-logic-repaired-yield
           (yield-func (* cdm-logic-area (- 1 cdm-logic-repairable))))
          
;;          (cdm-logic-deltayield (/ cdm-logic-repaired-yield cdm-logic-yield))
          (cdm-repaired-yield
           (modules-yield cdm-sram-yield-1bad cdm-logic-repaired-yield))

          ;;stm memory
          (stm-ram-repairable .9)  ;;assume spare bit
          (stm-logic-repairable .87) ;;20b address not repairable out of 137b data + 20b address
;;          (stm-ram-repaired-yield
;;           (- 1 (* (- 1 stm-ram-yield) (- 1 stm-ram-repairable))))


          (stm-ram-repaired-yield
           (scale-area stm-ram-yield (- 1 stm-ram-repairable)))
          
;;          (stm-logic-repaired-yield
;;           (- 1 (* (- 1 stm-logic-yield)  (- 1 stm-logic-repairable))))
          (stm-logic-repaired-yield
           (scale-area stm-logic-yield (- 1 stm-logic-repairable)))

          (stm-repaired-yield (modules-yield stm-logic-repaired-yield
                                             stm-ram-repaired-yield))
          
;;          (stm-ram-deltayield (/ stm-ram-repaired-yield stm-ram-yield))
;;          (stm-logic-deltayield (/ stm-logic-repaired-yield stm-logic-yield))

          (ppu-pipe-repaired-yield
           (modules-yield parde-yield
                          stm-ram-repaired-yield
                          stm-logic-repaired-yield
                          ppu-core-yield
                          tcam-array-yield))

          ;;total
;;          (yield-improvement
;;           (* macspare-deltayield
;;              cdm-sram-deltayield
;;              cdm-logic-deltayield
;;              stm-ram-deltayield
;;              stm-logic-deltayield
;;              mac-channel-deltayield))

;;          (final-yield    (* base-yield yield-improvement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          ;;half starting w/ base
;;          (base-evenodd-yield   (/ base-yield 
;;                                   tm-core-yield
;;                                   misc-yield
;;                                   gpio-yield))

          (base-evenodd-yield
           (modules-yield serdes-yield
                          mac-yield
                          parde-yield
                          stm-yield
                          ppu-core-yield
                          tcam-array-yield
                          cdm-yield
                          mac-channel-yield))
          ;;base yield
          (base-yield   (modules-yield base-evenodd-yield
                                       tm-core-yield
                                       misc-yield
                                       gpio-yield))

          (final-evenodd-yield
           (modules-yield serdes-yield
                          mac-withspare-yield
                          parde-yield
                          stm-repaired-yield
                          ppu-core-yield
                          tcam-array-yield
                          cdm-repaired-yield
                          mac-channel-repaired-yield))
          
          ;; final yield
          (final-yield   (modules-yield final-evenodd-yield
                                        tm-core-yield
                                        misc-yield
                                        gpio-yield))

          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          

          ;;half
          (evenodd-yield    (modules-yield serdes-yield
                                           mac-withspare-yield
                                           ppu-pipe-repaired-yield
                                           ;;tm-core-yield
                                           cdm-repaired-yield
                                           ;;misc-yield
                                           ;;gpio-yield
                                           mac-channel-repaired-yield))

          (tohalf-yield       (modules-yield evenodd-yield tm-core-yield))
          (halfchip-yield   (scale-area tohalf-yield 1/2))
          
          (either-lrhalf-yield  (modules-yield
                                 (redundant-yield halfchip-yield 2 1)
                                 misc-yield
                                 gpio-yield))
          
;;          (lrhalf-25t-+yield  (- either-lrhalf-yield final-yield))
          (evenodd-half-yield (scale-area evenodd-yield 1/2))

          (either-eohalf-yield  (modules-yield
                                 (redundant-yield evenodd-half-yield 2 1)
                                 tm-core-yield
                                 misc-yield
                                 gpio-yield))
          
;;          (eohalf-25t-+yield  (- either-eohalf-yield final-yield))

          (base-half-yield    (modules-yield
                               base-evenodd-yield
                               tm-core-yield))
          
          (base-halfchip-yield    (scale-area base-half-yield 1/2))
          
          (base-either-lrhalf-yield (modules-yield
                                     (redundant-yield base-halfchip-yield 2 1)
                                     misc-yield
                                     gpio-yield))
          
;;          (base-lrhalf-25t-+yield   (- base-either-lrhalf-yield base-yield))
          (base-evenodd-half-yield  (scale-area base-evenodd-yield 1/2))

          (base-either-eohalf-yield (modules-yield
                                     (redundant-yield
                                      base-evenodd-half-yield 2 1)
                                     tm-core-yield
                                     misc-yield
                                     gpio-yield))
;;          (base-eohalf-25t-+yield   (- base-either-eohalf-yield base-yield))
          )

      ;; code goes here
      (list base-yield final-yield)
      
      )
    )
)


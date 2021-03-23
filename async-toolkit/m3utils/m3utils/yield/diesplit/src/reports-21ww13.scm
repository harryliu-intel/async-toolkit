;; do reports-21ww07.scm before this one

(define (tile-model)

  (let* ((serdes-area  (* 11 3.7 1.5))
         (d2d-area     (* 20 1.75))
         (routing-area (- 115 serdes-area d2d-area)))

    
    `(io-tile
      (channels ,routing-area channel-area ,routing-area)
      (serdes   ,serdes-area  serdes-area  ,serdes-area)
      (d2d      ,d2d-area     serdes-area  ,d2d-area))))

(define (tfc-core-model)
  
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
          ;;(serdes-area    (+ (* 32 3.7 1.5)(* 8 5 1.5)))

          ;;misc blocks
          (misc-areas   (+ 2.72         ;;host+sbc
                           3.4          ;;risc
                           (* 1.5 .675) ;;eth800G
                           .45          ;;ethcpu
                           .45))        ;;tcu
          
          (gpio-area    (+ 1.96 5.69))

          ;;mac channel
          (mac-channel-area (* 2 (+ (* mac-channel-lr-width 30)
                                    (* mac-channel-tb-width 20))))
          ;;ppu, parde
          (ppu-ysize
           (+ (* ppu-num-stm-rows stm-row-y)(* 2 ppu-core-y) tcam-array-y))
          (parde-area   (* parde-x ppu-ysize npipes))
          (parde-ramfraction  .33)

          (ppu-core-area    (* ppu-nstages 2 npipes ppu-x ppu-core-y))
          
          ;;tcam
          (tcam-area    (* tcam-array-y ppu-x ppu-nstages npipes))
          (tcam-memory-area
           (* ppu-unit-tcam-area num-tcams/stage ppu-nstages npipes))

          ;;stm
          (stm-area   (* ppu-nstages ppu-num-stm-rows npipes ppu-x stm-row-y))
          (stm-ram-area
           (* stm-unit-sram-area ppu-num-stm-rows ppu-num-ram-cols/stg
              ppu-nstages npipes))
          (stm-ram-array-efficiency (/ stm-ram-area stm-area))

          ;;mac channel spare wire
          (mac-channel-repairable .95)
          (mac-channel-repair-cost .01)

          ;;cdm memory
          (cdm-numblocks    (* num-cdm-ram-rows/subwd num-cdm-ram-cols 1/4 1.0))


          (cdm-logic-repairable .95)
          (cdm-logic-repair-cost .01)

          ;;stm memory
          (stm-ram-repairable .9)  ;;assume spare bit
          (stm-ram-repair-cost .01)
          
          (stm-logic-repairable .87) ;;20b address not repairable out of 137b data + 20b address
          (stm-logic-repair-cost .01)
          (d2d-area (* 2 1.2 32))
          )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                                     ;;
      ;;      Definition of TFc follows      ;;
      ;;                                     ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      `(tfc-core
        (misc ,misc-areas
              ram-area     ,(* misc-areas .25)
              channel-area ,(* misc-areas .25))
        
        (tm-core
         (* #f 2 2
            (tm-core-core ,tm-core-area
                          ram-area ,(* tm-core-area tm-core-ramfraction))))

        (gpio ,gpio-area
              channel-area ,gpio-area)

        (evenodd
         (d2d ,d2d-area serdes-area ,d2d-area)
         
         (mac (* #f 4 4 (* mac-spare ,(if *spare-mac* 17 16) 16 (onemac ,mac-area))))
         
         (mac-channel ,mac-channel-area
                      channel-area ,mac-channel-area
                      repair ,mac-channel-repairable
                      repair-cost ,mac-channel-repair-cost)
         
         (ppu-pipe
          
          (parde ,parde-area
                 ram-area ,(* parde-area parde-ramfraction))
          
          (ppu-core ,ppu-core-area)
          
          (tcam-array ,tcam-area ram-area ,tcam-memory-area)
          
          (stm
           (stm-ram ,stm-ram-area
                    ram-area ,stm-ram-area
                    repair ,stm-ram-repairable
                    repair-cost ,stm-ram-repair-cost)
           (stm-logic ,(- stm-area stm-ram-area)
                      repair ,stm-logic-repairable
                      repair-cost ,stm-logic-repair-cost))
          );;ppu-pipe
         
         (cdm
          (cdm-logic ,cdm-logic-area
                     repair ,cdm-logic-repairable
                     repair-cost ,cdm-logic-repair-cost)
          
          (cdm-sram (* cdm-spare ,(+ 1 cdm-numblocks) ,cdm-numblocks 
                       (cdm-block ,(/ cdm-sram-area cdm-numblocks)
                                  ram-area ,(/ cdm-sram-area cdm-numblocks))))
          );; cdm
         );;evenodd
        );;quote
      );;let*
    );;define

(define (report-tile)
  (report-yields-for-params
   (tile-model)

   params
   `()
   )
  )

(define (core-lrhalf-25t-model)
  (make-downbin tfc-core-model
                `(lrhalf-25t
                  (* lr-spare 2 1
                     (lr-half (scale ,1/2 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define (core-lhalf-25t-model)
  (make-downbin tfc-core-model
                `(lhalf-25t
                  (* lr-spare 1 1
                     (lr-half (scale ,9/16 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define (core-lhalf-8/16-25t-model) ;; pipe 0 not needed
  (make-downbin tfc-core-model
                `(lhalf-8/16-25t
                  (* lr-spare 1 1
                     (lr-half (scale ,1/2 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define (core-lrhalf-16t-model)
  (make-downbin tfc-core-model
                `(lrhalf-16t
                  (* lr-spare 2 1
                     (lr-half (scale ,6/16 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define (core-lhalf-16t-model)
  (make-downbin tfc-core-model
                `(lhalf-16t
                  (* lr-spare 1 1
                     (lr-half (scale ,6/16 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define (core-15/16-pipe-model)
  (make-downbin tfc-core-model
                `(15/16-pipe
                  (* lr-spare 1 1
                     (lr-half (scale ,15/16 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )


(define (report-core)
  (report-yields-for-params
   (tfc-core-model)

   params
   `(   ;; downbins
   ;;,(lrhalf-25t-model)
   ,(core-15/16-pipe-model)
   ,(core-lhalf-25t-model)
   ,(core-lhalf-8/16-25t-model)
   ,(core-lhalf-16t-model)
   ,(core-lrhalf-25t-model)
   ,(core-lrhalf-16t-model)
   )
  )
)

(define (core-1.15-model)
  (make-downbin tfc-core-model
                '(tfc-core-bloated (big (scale 1.15 tfc-core)))
                )
  )

(define (report-bloat)
  (report-yields-for-params
   (tfc-core-model)

   params
   `( ;; upbin!
   ,(core-1.15-model))))

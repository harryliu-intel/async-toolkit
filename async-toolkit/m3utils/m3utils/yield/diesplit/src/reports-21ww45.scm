;;
;;
;; email from Anurag 10/29/21
;;
;;
;;
;; IMPORTANT NOTE
;; the functions used don't really let you change "n" very easily.
;;
;; this is a bad defect! currently, you have to change the n between the
;; N5 and N3 values manually.
;;

(load "reports-21ww38.scm")
        
(define (n5-core-model)
  '(ftr-core
    (hard-io 258.52 serdes-area 258.52)
    
    (soft-io
     (* #f 8 8 (* mac-spare 17 16 (onemac 0.9591))))
    ;; is this right?  where are the channels?
    
    (fwd 
     
     (ppu-stm-tcm 328.34)
     
     (parde 41.12 ram-area 13.6)
     
     (pkt-body-learn 9.01)
     ;; comments from Anurag?
     )
    
    (tm
     
     (* #f 2 2 (pkt-buffer-sram (* cdm-spare 193 192
                                   (cdm-block 0.382 ram-area 0.382))))
     ;; pktbuffer from Anurag
     
     (queueing 102.9 ram-area 60 channel-area 16)
     ;; TM from Anurag, ram-area guessed, channel-area GPIO from before?
     )
    
    (misc     26.2  ram-area 6.6 channel-area 6.6)
    ;; scaled 

    (space 44.62 repair 1 repair-cost 0.0)
    )
  )

  
(define (n5-ftr-split-die-model)
  `(ftr-split-die
    ,(make-scaled-model `half-ftr-core (n5-core-model) 0.5)

    (tile-d2d 29.84 serdes-area 29.84)             ;; from Anurag
    )
  )

(define (n3-ftr-split-die-model)
  (convert-n5-to-n3 (n5-ftr-split-die-model)))

(define (n3-ftr-monolithic-die-model)
  (convert-n5-to-n3 (remove-labeled-block 'hard-io (n5-core-model))))


(define (n5-ftr-tiled-die-model)
  ;; this is FTR without soft-io, without hard-io, but extra io-d2d
  ;; in N5 for now

  `(ftr-without-io
    ,(remove-labeled-block 'soft-io (remove-labeled-block 'hard-io (n5-core-model)))
    (io-d2d 40 serdes-area 40)))

(define (n5-ftr-tiled-split-die-model)
  `(ftr-tiled-split-die
    ,(make-scaled-model `half-ftr-core (n5-ftr-tiled-die-model) 0.5)

    (tile-d2d 29.84 serdes-area 29.84)             ;; from Anurag
    )
  )

(define (n3-ftr-tiled-die-model)
  (convert-n5-to-n3 (n5-ftr-tiled-die-model)))

(define (n3-ftr-tiled-split-die-model)
  (convert-n5-to-n3 (n5-ftr-tiled-split-die-model)))

(define (n5-io-tile-model)
  (make-scaled-model 'split-io-tile
                     `(io-stuff
                       (io-d2d 40 serdes-area 40)
                       ,(extract-labeled-block 'hard-io (n5-core-model))
                        (soft-io
                         (* #f 8 8 (* mac-spare 16 16 (onemac 0.9591)))))
                     (* (/ 58 52.66)(/ 1 8))))
   

(define (run-reports)
  (report-yields-for-params (n5-ftr-split-die-model) *n5-params* '())

  (report-yields-for-params (n5-io-tile-model) *n5-params* '())

  (report-yields-for-params (n3-ftr-split-die-model) *n3-params* '())

  (report-yields-for-params (n3-ftr-tiled-die-model) *n3-params* '())

  (report-yields-for-params (n3-ftr-tiled-split-die-model) *n3-params* '())
  )

(define *n5-dols-per-mm2* 0.217)
(define *n3-dols-per-mm2* 0.362)

(define (simple-si-cost cost-per-mm2
                        area
                        yield
                        need-per-prod)

  (let* ((tgt-dice    (* need-per-prod 100))
         (make-dice   (round (/ tgt-dice yield)))
         (markup      1.03) ;; always?
         (si-cost (round (* markup (/ (* make-dice cost-per-mm2 area) 100))))
         
         )

    (dis "tgt-dice " tgt-dice dnl
         "make-dice " make-dice dnl
         "tot-si-cost " si-cost dnl)
    si-cost
    ))

(define (scale-to-area model area)
  (let* ((orig-area (compute-total-area model '()))
         (factor (/ area orig-area)))

    (make-scaled-model (string->symbol (string-append "scaled"
                                                      (stringify factor)))
                       model
                       factor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cost of N5 tile
(simple-si-cost *n5-dols-per-mm2* 58 0.8396 8)

;; N3 monolithic 51.2T

(report-yields-for-params
 (scale-to-area (n3-ftr-split-die-model) 483)
 *n3-params*
 '())


(simple-si-cost *n3-dols-per-mm2* 483 0.332 2)
(simple-si-cost *n3-dols-per-mm2* 483 0.423 2) ;; ww46
(tally-costs '(852))

;; N3 halfcore

(report-yields-for-params (scale-to-area (n3-ftr-tiled-split-die-model) 396) *n3-params* '())

(simple-si-cost *n3-dols-per-mm2* 396 0.370 2)
(simple-si-cost *n3-dols-per-mm2* 396 0.462 2)
(tally-cost '(639 124))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 102.4T
;;

;; falcon tile yield
(report-yields-for-params (make-scaled-model `n3-falcon-tile (convert-n5-to-n3 (n5-io-tile-model)) (/ 82 50.04)) *n3-params* '())

;; cost of Falcon N3 tile
(simple-si-cost *n3-dols-per-mm2* 81 0.8100 8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (n5-core-102p4-model)
  ;; this is kind of synthetic, to match N3 monolithic (unbuildable) design
  '(ftr-102p4-core
    (hard-io 423.7 serdes-area 423.7)
    
    (soft-io
     (* #f 8 8 (* mac-spare 33 32 (onemac 0.9591))))
    ;; is this right?  where are the channels?
    
    (fwd 
     
     (ppu-stm-tcm 328.34)
     
     (parde 41.12 ram-area 13.6)
     
     (pkt-body-learn 9.01)
     )
    
    (tm
     
     (* #f 4 4 (pkt-buffer-sram (* cdm-spare 193 192
                                   (cdm-block 0.382 ram-area 0.382))))
     ;; pktbuffer from Anurag
     
     (queueing 102.9 ram-area 60 channel-area 16)
     ;; TM from Anurag, ram-area guessed, channel-area GPIO from before?
     )
    
    (misc     26.2  ram-area 6.6 channel-area 6.6)
    ;; scaled 

    (space 44.62 repair 1 repair-cost 0.0)
    )
  )

(define (n5-ftr-102p4-split-model)
  `(ftr-split-102p4
    ,(make-scaled-model `half-ftr-102p4 (n5-core-102p4-model) 0.5)

        (split-d2d 29.84 serdes-area 29.84)             ;; from Anurag
    )
  )

(define (n3-ftr-102p4-split-model)
  (scale-to-area (convert-n5-to-n3 (n5-ftr-102p4-split-model)) 680))

(report-yields-for-params (n3-ftr-102p4-split-model) *n3-params* '())

(simple-si-cost *n3-dols-per-mm2* 680 0.357 2)
(tally-cost '(1420))

;; just the core


;;N5
(define (n5-ftr-102p4-split-core-model)
  (scale-to-area `(ftr-split-core-102p4
                   (io-d2d 20 serdes-area 20)
                   ,(remove-labeled-block
                     'hard-io
                     (remove-labeled-block
                      'soft-io
                      (n5-ftr-102p4-split-model))))
                 556))

(simple-si-cost *n5-dols-per-mm2* 556 0.380 2)

;; N3
;; some of these scale-to-area efforts are troublingly large,
;; fairly big discrepancy between Anurag's area numbers and what comes
;; out of the converter
(report-yields-for-params (scale-to-area (convert-n5-to-n3 (n5-ftr-102p4-split-core-model)) 480) *n3-params* '())

(simple-si-cost *n3-dols-per-mm2* 480 0.356 2)
(simple-si-cost *n3-dols-per-mm2* 480 0.449 2)

(define (tally-cost si-costs)
  (let* ((si-sum (apply + si-costs))
         (pkg    150)
         (packaged (+ si-sum pkg))
         (test    20)
         (tested   (+ test packaged))
         (yield 0.98)
         (out-the-door (/ tested yield)))
    out-the-door))

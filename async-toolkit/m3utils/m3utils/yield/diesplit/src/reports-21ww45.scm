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
                     (/ 1 8)))
   

(define (run-reports)
  (report-yields-for-params (n5-ftr-split-die-model) *n5-params* '())

  (report-yields-for-params (n5-io-tile-model) *n5-params* '())

  (report-yields-for-params (n3-ftr-split-die-model) *n3-params* '())

  (report-yields-for-params (n3-ftr-tiled-die-model) *n3-params* '())

  (report-yields-for-params (n3-ftr-tiled-split-die-model) *n3-params* '())
  )



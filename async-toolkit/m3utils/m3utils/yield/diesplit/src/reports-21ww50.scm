;;
;;
;; Questions from Anurag and Ram 12/7/21
;;

(load "reports-21ww38.scm") ;; setup for N3 and N5
(load "reports-21ww45.scm") ;; setup for N3 and N5

(define (n5-112G-tile-model)
  (scale-to-area
   `(n5-112G-tile
     (space 25.165 repair 1 repair-cost 0.0)
     ,(make-scaled-model 'split-io-tile
                      `(io-stuff
                        (io-d2d 40 serdes-area 40)
                        ,(extract-labeled-block 'hard-io (n5-core-model))
                        (soft-io
                         (* #f 8 8 (* mac-spare 16 16 (onemac 0.9591)))))
                      (* (/ 58 52.66)(/ 1 4))))
   (* 8.8 15.3)))

   
(define (bloated-falcon-tile-model)
  (scale-to-area
   `(falcon-spaced
     ,(falcon-tile-model)
     (space 14.04 repair 1 repair-cost 0.0))

     (* 7.2 19)))

(simple-si-cost *n5-dols-per-mm2* 135 0.679 4)

(simple-si-cost *n5-dols-per-mm2* 135 0.724 4)

(simple-si-cost *n3-dols-per-mm2* 137 0.678 8)

(simple-si-cost *n3-dols-per-mm2* 137 0.715 8)

(tally-cost '(399 178)) ;; PCOS of 4x112G tile + core

(tally-cost '(399 167)) ;; PCOS of 4x112G tile + core

(tally-cost '(796 603)) ;; PCOS of 8x224G tile + core

(tally-cost '(796 572)) ;; PCOS of 8x224G tile + core

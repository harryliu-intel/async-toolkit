
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hierarchical yield calculation example.
;;
;; The HAL9000 multiprocessor consists of 10 individual HAL1000
;; processors, one of which is spare (i.e., 9 are needed for correct
;; operation).  The HAL9000 further has 100 square millimeters of
;; non-redundant hardware.
;;
;; A HAL1000 single processor consists of 9 slices, each of which
;; contains 4 square millimeters.  1 of these is spare (i.e., 8 are
;; needed for correct operation).  A HAL1000 further contains 14
;; square millimeters of non-redundant hardware.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define HAL1000-slice-yield (area-yield 10))

(define HAL1000-yield
  (modules-yield
   (area-yield 5) ;; non-redundant
   (redundant-yield HAL1000-slice-yield 9 8)))

(define HAL9000-yield
  (modules-yield
   (area-yield 50) ;; non-redundant
   (redundant-yield HAL1000-yield 10 9)))

;;;;;;;;;;;;;;;;;;;;
;;
;; representative high-end process as of 2020
;;

(define D0    0.10)
(define n       30)
(define alpha 0.02)

;; let's use Stapper's formula 
(define (the-yield-model A) (YieldModel.Stapper A D0 n alpha))

(define computed-HAL-9000-yield
  (eval-yield HAL9000-yield the-yield-model))

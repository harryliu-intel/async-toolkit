; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; simple example
;;
;; mika.nystroem@intel.com 
;; January 23, 2021
;;

(define simple-D0    0.10)
(define simple-n       30)
(define simple-alpha 0.02)
  
(define (stapper A)
  (YieldModel.Stapper A simple-D0 simple-n simple-alpha))

;; imagine a system needing 800 parts of 1 mm^2
;; 1 spare, 801 total
;;
;; system works P(x >= 800) =
;; P(801) = x^801
;; P(800) = C(801,800) x^800(1-x)^1

(define x (stapper 1))

(define (improvement from to) (/ (- to from) from))

(define (solve f start-lo start-hi)
  ;; solve eq f = 0 from start (start-lo , start-hi)
    (Solve.WDB (make-lrfunc-obj f) start-lo start-hi 1e-8))

;; binomial calc

(define P801 (expt x 801))

(define P800 (* (choose 801 800) (expt x 800)(expt (- 1 x) 1)))

(define Y800 (expt x 800))

(define Y801 (+ P801 P800))

(define YI801 (improvement Y800 Y801))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; "correct" calc
;;

(define Ys800 (stapper 800))

(define Ys801 (- (* 801 (stapper 800)) (* 800 (stapper 801))))

(define YsI801 (improvement Ys800 Ys801))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; now make a Poisson with same yield @ 801 mm^2
;;

(define (compute-equivalent-poisson-d at-area stapper-D0 stapper-alpha n)
  (define (error-func D)
    (- (YieldModel.Poisson at-area D n)
       (YieldModel.Stapper at-area stapper-D0 n stapper-alpha)))

  (solve error-func 0 stapper-D0))

(define equiv-poisson-D
  (compute-equivalent-poisson-d 801 simple-D0 simple-alpha simple-n))

(define (poisson A)
  (YieldModel.Poisson A equiv-poisson-D simple-n))

(define y (poisson 1))

(define Q801 (expt y 801))
(define Q800 (* (choose 801 800) (expt y 800) (expt (- 1 y) 1)))

(define Z800 (expt y 800))
(define Z801 (+ Q801 Q800))

(define ZI801 (improvement Z800 Z801))

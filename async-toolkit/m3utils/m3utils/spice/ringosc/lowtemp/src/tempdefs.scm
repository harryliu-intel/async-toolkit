; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0


;;
;; extra defines for temp optimization
;;

(define *min-temp* -70)
(define *max-temp* (- *base-temp* 1))
(define *significant-temp-delta* 1)

(define (range-penalty-factor x0 delta-x)
  (lambda(x)
    (+ 1 (exp (/ (- x x0) delta-x)))))

(define (range-penalizer x0 delta-x)
  (lambda(value x)
    (* value ((range-penalty-factor x0 delta-x) x))))

(define (double-range-penalizer lo hi delta-x)
  (let ((lo-p (range-penalizer lo (- delta-x)))
        (hi-p (range-penalizer hi (+ delta-x))))
    (lambda(value x)
      (lo-p (hi-p value x) x))))

(define *temp-penalizer*
  (double-range-penalizer *min-temp* *max-temp* *significant-temp-delta*))

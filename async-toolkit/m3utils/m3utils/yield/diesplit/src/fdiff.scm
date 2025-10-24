; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (stapper-func A D0 n alpha)
  (expt (+ 1 (/ (* A D0) alpha)) (* (- n) alpha)))

(define (stapper-log-deriv A k D0 n alpha)
  (*
   (/ (Gamma (+ (* n alpha) k))
      (Gamma    (* n alpha)))
   (expt -1 k)
   (expt
    (/ D0 (+ alpha (* A D0)))
    k)
   )
  )

(define (stapper-deriv A k D0 n alpha)
  (* (stapper-func A D0 n alpha)
     (stapper-log-deriv A k D0 n alpha)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-D0 D0-per-sq-in)
  (/ D0-per-sq-in 25.4 25.4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; example

(define ex-D0 (map-D0 0.1))
(define ex-n 30)
(define ex-alpha 0.02)

(define f (lambda(A)(stapper-func A ex-D0 ex-n ex-alpha)))

(define f-log-deriv (lambda(A k)(stapper-log-deriv A k ex-D0 ex-n ex-alpha)))

(define f-deriv (lambda(A k)(stapper-deriv A k ex-D0 ex-n ex-alpha)))


  
                     

; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(load "term-defs.scm")
;;(load "term.scm")
(load "poly-term.scm")


(define (area-yield A)
  (make-power-term A))

(define (modules-yield . x)
  (accumulate *-term (make-number-term 1) x))

(define (redundant-yield x N M)
  (sum-term M N (lambda(k)(*-term (make-number-term (choose N k))
                                  (*-term (^-term x k)
                                          (^-term 
                                           (+-term 1-term (*-term -1-term x))
                                           (- N k))))
                       )))
                                                   
                                   
(define (scale-area x by)
  ;; return the yield of x, of the same structure, but where all the areas
  ;; are scaled by by
  (scale-exponents x by))

(define (unmodule-yield y to-remove)
  ;; given yield expression y, remove to-remove
  (/-term y to-remove))



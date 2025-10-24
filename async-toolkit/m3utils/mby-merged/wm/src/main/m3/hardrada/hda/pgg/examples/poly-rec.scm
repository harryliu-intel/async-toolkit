; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define main
  (lambda (x)
    (+ (g x) 1)))

(define-without-memoization g
  (lambda-poly (x)
    (if (= x (+ x 1)) 1 (g x))))

;;; use:
; (define genext (cogen-driver '("examples/poly-rec.scm") '(main 1)))
; (writelpp genext "/tmp/poly-rec0.scm")
; (prepare!)
; (reset (begin
;	   (load "/tmp/poly-rec0.scm")
;	   (specialize-after-prepare $goal '(main 1) (list 'x1))))
; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define-primitive apply - apply)
(define-primitive cons - pure)
(define-primitive car - pure)

(define (main s d)
  (let loop ((s s) (d d) (r '()))
    (if (null? s)
	(apply (lambda z z) r)
	(loop (cdr s) (cdr d) (cons (car d) r)))))

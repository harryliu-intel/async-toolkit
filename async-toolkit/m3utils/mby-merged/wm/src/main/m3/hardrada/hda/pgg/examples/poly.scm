; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (main s1 s2 d)
  (let ((f (lambda-poly (x y) (+ x y))))
    (* (f s1 d)
       (f s2 d)
       (f s1 d)
       (f s2 d)
       (g f (+ d d)))))

(define (g f d)
  (+ (f 7 d) (f 11 d)))


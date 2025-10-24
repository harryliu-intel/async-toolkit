; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (app x y)
  (if (null? x)
      y
      (cons (car x) (app (cdr x) y)))) 

(define (rapp y x)
  (app x y))

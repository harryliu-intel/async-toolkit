; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (bits? x) (and (pair? x) (eq? 'bits (car x))))

(define (get-bits-expr x)(cadr x))

(define (get-bits-min x) (caddr x))

(define (get-bits-max x) (cadddr x))
  

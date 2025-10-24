; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (symbol-append . x) ;; permissive form, allow both symbol and string
  (string->symbol
   (eval
    (cons 'string-append
          (map (lambda (s)
                 (cond ((symbol? s) (symbol->string s))
                       ((string? s) s)
                       (else (error (string-append
                                     "not a string or symbol : " s)))))
               x)))))

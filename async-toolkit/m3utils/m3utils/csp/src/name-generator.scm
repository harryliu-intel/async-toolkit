; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *name-counter* 0) ;; guarantees all the names are unique

(define (make-name-generator rootstring)
  ;;
  ;; return a temporary-name-generator
  ;; a procedure of zero arguments, which generates incrementing temp names
  ;;
  (let ((root                rootstring)
        (my-last-name        #f)
        )


    (define (make-symbol n)
      (string->symbol(string-append root (stringify n))))

    (lambda(cmd . x)
      (case cmd
        ((names)
         (let loop ((i (- *name-counter* 1))
                    (res '()))
           (if (= i -1)
               res
               (loop (- i 1) (cons (make-symbol i) res))))
               
         )

        ((last) my-last-name)
        
        ((next)
         (let* ((z (make-symbol *name-counter*))
                (res (if (null? x) z (symbol-append (car x) z))))
           (set! my-last-name res)
           (set! *name-counter* (+ *name-counter* 1))
           res))))))



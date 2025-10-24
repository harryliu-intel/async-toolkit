; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (spaces n)
  (if (= n 0) "" (string-append " " (spaces (- n 1)))))

(define *one-liners*
  '(lock unlock waitfor label goto var1 assign recv send id call-intrinsic apply range assign-operate decl1))

(define (pp-depth x n nsp dis)

  (define (default)
    (dis (spaces nsp) (stringify x) dnl))
  
  (cond ((not (pair? x))       (default))

        ((member (car x) *one-liners*)
                               (default))

        ((list? x)
         (dis (spaces nsp) "(" (stringify (car x)) dnl )
         (map (if (= n 0)
                  (lambda (c) (dis (spaces (+ 4 nsp)) (stringify c) dnl))
                  (lambda (c) (pp-depth c (- n 1) (+ nsp 4) dis)))
              (cdr x))
         (dis (spaces nsp) ")" dnl))
        
        (else (default))
        )
  )


(define (pp lst . n) ;; pretty-print
  (pp-depth lst (if (null? n) 100 (car n)) 0 dis)
  #t)

(define (qq lst)
  (map pp lst)
  'ok
  )

(define (dis1 . x)
  (apply dis x)
  (dis dnl)
  'ok
  )

(define (ss lst . n)
  (define result "")

  (define (dis . x)
    (set! result (sa result (apply sa x))))

  (pp-depth lst (if (null? n) 100 (car n)) 0 dis)
  result
  )  

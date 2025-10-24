; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (compile-to-c x)
  (define i 0)

  (define bwr (TextWr.New))

  (define (emit-temp x)
    (let ((nam (string-append "t" (number->string i))))
      (dis nam " = " x ";" dnl bwr)
      (set! i (+ i 1))
      nam))

  (define (recurse x)
    (cond ((number? x)
           (emit-temp (number->string x)))
          
          ((pair? x)
           (let ((a (cadr x))
                 (b (if (binop? (car x)) (caddr x) #f)))
             (case (car x)
               ((* +) (emit-temp
                       (string-append
                        (recurse a)
                        " " (car x) " "
                         (recurse b))))
               ((^) (emit-temp
                     (string-append "pow(" (recurse a) ", " (recurse b) ")")))
               
               ((exp log) (emit-temp
                           (string-append  (car x) "(" (recurse a) ")")))
               
               (else (error "unknown op " (car x)))))
           )

          (else (emit-temp x))))

  (let ((cwr (TextWr.New))
        (result (recurse x)))
    (write-header i cwr)
    (dis (TextWr.ToText bwr) cwr)
    (write-footer result cwr)
    (TextWr.ToText cwr)
    ))

(define (write-header n wr)
  (dis "#include <math.h>" dnl
       dnl
       "double f(double D)" dnl
       "{" dnl
       wr)
  (let loop ((i 0))
    (if (= i n)
        'ok
        (begin
          (dis "double t" (number->string i) ";" dnl wr)
          (loop (+ i 1))))))
        
  

(define (write-footer result wr)
  (dis "return " result ";" dnl
       "}" dnl wr
       ))

;;(compile-to-c (nth (car (do-report-yield 'xxx (tfc-model) 0 1 1 '() '(0.05 10 32))) 4))



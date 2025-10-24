; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

;;
;; math.scm
;;
;; abandoned code
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum-math lo hi f)
  (let loop ((s 0)
             (i lo))
    (if (= i hi)
        (+-math (f i) s)
        (loop (+-math (f i) s)
              (+ i 1)))))

(define (binomial-yield-math A N M)
  (let ((ym (^-math 'Y A)))
    (sum-math M N (lambda(k)(*-math (choose N k)
                                    (*-math (^-math ym k)
                                            (^-math (+-math 1 (*-math -1 ym))
                                                    (- N k))))))))

(define (+-math x y)
  (cond ((and (number? x) (number? y)) (+ x y))

        ((number? y) (+-math y x))

        ((and (number? x) (= x 0)) y)

        ((and (+? x) (+? y))
         (let loop ((p (cdr x))
                    (q y))
           (if (null? p) q (loop (cdr p) (+-term-math (car p) q)))))

        ((+? y)
         (+-term-math x y))
        
        (else (list '+ x y))))

(define *d* '())
(define *e* '())

(define (+-term-math x y)

  ;;(dis "x: " x dnl)
  ;;(dis "y: " y dnl)

  ;; x is a term, y is a +-expression
  (let loop ((t   x)
             (res '())
             (lst (cdr y)))
    (cond ((null? t) (cons '+ (append res lst)))
          ((null? lst) (cons '+ (cons t res)))
          (else
           (let ((head (car lst)))
             (cond ((equal? head t)
                    (loop '() (cons (*-math 2 t) res) (cdr lst)))

                   ((and (*? head)
                         (number? (cadr head))
                         (equal? t (caddr head)))
                    (loop '() (cons (*-math (+ 1 (cadr head)) t) res) (cdr lst)))

                    ((and (*? t)
                          (number? (cadr t))
                          (equal? head (caddr t)))
                    (loop '() (cons (*-math (+ 1 (cadr t)) head) res) (cdr lst)))

                    ((and (*? t) (*? head)
                          (number? (cadr t)) (number? (cadr head))
                          (equal? (caddr head) (caddr t)))
                     (loop '()
                           (cons
                            (*-math (+ (cadr head) (cadr t)) (caddr head)) res)
                           (cdr lst)))
                    
                    (else (loop t (cons head res) (cdr lst)))))))))

(define *a* '())
(define *b* '())

(define (atom-< x y)
  (set! *a* x)
  (set! *b* y)
  (cond ((and (number? x) (not (number? y))) #t)
        ((and (not (number? x)) (number? y)) #f)
        ((and (number? x) (number? y)) (< x y))
        (else (string<? (symbol->string x) (symbol->string y)))))
        
(define (*-math x y)
  (cond ((and (number? x) (number? y)) (* x y))
        ((and (atom? y) (not (atom? x))) (*-math y x))
        ;; if either x or y is a number, it will be on the outside

        ((and (atom? x) (atom? y) (atom-< y x))
         (*-math y x))
        ((and (number? x) (= x 0)) 0)
        ((and (number? x) (= x 1)) y)
        ((and (number? x) (*? y) (number? (cadr y)))
         (*-math (* x (cadr y)) (caddr y)))
        ((and #t (+? y)) (*-+-math x y))
        ((and (atom? x) (eq? x y)) (^-math x 2))

        ((and (^? x) (not (^? y))) (*-math y x)) ;; powers come second

        ((and (*? x) (^? y) (equal? (caddr x) (cadr y)))
         (*-math (cadr x) (^-math (cadr y) (+-math 1 (caddr y)))))

        ((and (*? x) (^? y) (equal? (cadr x) (cadr y)))
         (*-math (caddr x) (^-math (cadr y) (+-math 1 (caddr y)))))
              
        ((and (^? x) (^? y) (equal? (cadr x) (cadr y)))
         (^-math (cadr x) (+-math (caddr x) (caddr y))))
        (else (list '* x y))))

(define (*-+-math x y)
  ;; y is a + expression
  (accumulate +-math 0 (map (lambda (yi) (*-math x yi)) (cdr y))))

(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define (binomial? x)
  (cond ((not (pair? x))        #f)
        ((eq? '- (car x))       #t)
        ((and (eq? '+ (car x))
              (= 3 (length x))) #t)
        (else                   #f)))

(define (op? op)
  (lambda (a)
    (cond ((not (pair? a)) #f)
          ((eq? op (car a)) #t)
          (else #f)))
  )

(define ^? (op? '^))
(define +? (op? '+))
(define *? (op? '*))

(define (^-math a b)
  (cond ((and (number? b) (= b 1)) a)
        ((and (number? b) (= b 0)) 1)
        ((and (number? b) (binomial? a)) (^-binomial-math a b))
        ((and (number? a) (number? b) (pow a b)))
        ((*? a) (*-math (^-math (cadr a) b) (^-math (caddr a) b)))
        ((and (number? b) (^? a))
         (^-math (cadr a) (*-math (caddr a) b)))
        (else (list '^ a b))))

(define (^-binomial-math a b)
  ;; a binomial, b number
  (let ((q (if (eq? (car a) '-) -1 1)))
    (sum-math 0 b
              (lambda (i)
                (*-math (* (pow q i)
                           (choose b i))
                        (*-math
                         (^-math (cadr a)        i)
                         (^-math (caddr a) (- b i)))))
              )
    ))

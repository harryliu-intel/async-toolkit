; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(require-modules "m3")

;; requires the Modula-3 interface Mpfr to be imported

(define the-mpfr-rounding-mode 'N)
(define the-mpfr-default-precision 4000)

(define (force-mpfr a)
  (if (number? a)
      (let ((res (Mpfr.New the-mpfr-default-precision)))
        (Mpfr.SetLR res a the-mpfr-rounding-mode)
        res)
      a
      ))

(define (make-list-mpfr-op op zero)
  (lambda(. a)
    ;;(dis "a: " a dnl)
    (let loop ((target (force-mpfr zero))
               (p a))
      ;;(dis "target: " target dnl)
      ;;(dis "p: " p dnl)
      (if (null? p)
          target
          (loop
           (begin
             (op target target (force-mpfr (car p)) the-mpfr-rounding-mode)
             target)
             
           (cdr p))))))
                

(define (make-binary-mpfr-op op)
  (lambda(a b)
    (let ((res (Mpfr.New the-mpfr-default-precision)))
      (op res (force-mpfr a) (force-mpfr b) the-mpfr-rounding-mode)
      res)))

(define (make-unary-mpfr-op op)
  (lambda(a)
    (let ((target (Mpfr.New the-mpfr-default-precision)))
      (op target (force-mpfr a) the-mpfr-rounding-mode)
      target)
    ))


(define (make-relation-mpfr-op op)
  (lambda(a b)
    (op (force-mpfr a) (force-mpfr b))))

(define +-mpfr (make-list-mpfr-op Mpfr.Add 0))
(define --mpfr (make-list-mpfr-op Mpfr.Sub 0))
(define *-mpfr (make-list-mpfr-op Mpfr.Mul 1))

(define /-mpfr (make-binary-mpfr-op Mpfr.Div))
(define pow-mpfr (make-binary-mpfr-op Mpfr.Pow))


(define (fmt-mpfr m) (Mpfr.Format m 10 the-mpfr-rounding-mode))

(define (mpfr->number m) (Mpfr.GetLR m the-mpfr-rounding-mode))

(define sqrt-mpfr (make-unary-mpfr-op Mpfr.Sqrt))
(define neg-mpfr (make-unary-mpfr-op Mpfr.Neg))
(define abs-mpfr (make-unary-mpfr-op Mpfr.Abs))
(define log-mpfr (make-unary-mpfr-op Mpfr.Log))
(define exp-mpfr (make-unary-mpfr-op Mpfr.Exp))
(define cos-mpfr (make-unary-mpfr-op Mpfr.Cos))
(define sin-mpfr (make-unary-mpfr-op Mpfr.Sin))
(define tan-mpfr (make-unary-mpfr-op Mpfr.Tan))
(define gamma-mpfr (make-unary-mpfr-op Mpfr.Gamma))

(define =-mpfr (make-relation-mpfr-op Mpfr.EqualP))
(define >-mpfr (make-relation-mpfr-op Mpfr.GreaterP))
(define <-mpfr (make-relation-mpfr-op Mpfr.LessP))

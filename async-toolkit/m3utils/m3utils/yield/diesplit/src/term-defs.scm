; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(load "mpfr.scm")

(define do-mpfr #t)

(define +-op +)
(define *-op *)
(define --op -)
(define /-op /)
(define pow-op pow)
(define =-op =)
(define >-op >)
(define <-op <)
(define +-sym '+)
(define *-sym '*)

(if do-mpfr
    (begin
      (dis "setting up MPFR operations" dnl)
      (set! +-op +-mpfr)
      (set! --op --mpfr)
      (set! *-op *-mpfr)
      (set! /-op /-mpfr)
      (set! pow-op pow-mpfr)
      (set! =-op =-mpfr)
      (set! >-op >-mpfr)
      (set! <-op <-mpfr)
      (set! +-sym '+-mpfr)
      (set! *-sym '*-mpfr)

      )
    )

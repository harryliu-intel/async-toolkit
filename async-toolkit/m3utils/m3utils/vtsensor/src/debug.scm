; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0


(require-modules "display")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; debugging definitions
;;
;; this is really too simplistic, should hook into
;; Debug Modula-3 interface instead
;;

(define *do-debug* #f) ;; override this with (set! *do-debug* #t) to debug

(define (debug . x)    ;; generic debug statement
  (if *do-debug* (apply dis "DEBUG " x) x))

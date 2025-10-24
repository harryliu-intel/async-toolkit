; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

;; from liberty-utils.scm
;; requires loading typecode-table.scm

(define (get-field x tc fn)
  (modula-type-op tc 'get-field x fn))

(define (get-concrete-field x fn)
  ;; requires that we have stubs for the concrete type
  (get-field (rttype-typecode x) fn))

(define (set-field! x tc fn val)
  ;; requires that we have stubs for tc and that
  ;; x is an instance of a subtype of tc
  (modula-type-op tc 'set-field! x fn val))

(define (set-concrete-field! x fn val)
  ;; requires that we have stubs for the concrete type
  (set-field! x (rttype-typecode x) fn val))


(define (list-fields obj)
  (let ((c-tc (rttype-typecode obj)))
    (modula-type-op c-tc 'list-fields obj)))

(define (have-field? obj fn)
  (member? fn (list-fields obj)))

(define (list-methods obj)
  (let ((c-tc (rttype-typecode obj)))
    (modula-type-op c-tc 'list-methods obj)))

(define (have-method? obj mn)
  (member? mn (list-methods obj)))

(define (call-method obj mn . args)
  (let ((c-tc (rttype-typecode obj)))
    (modula-type-op c-tc 'call-method obj mn args)))

(define (deep-copy obj)
  ;; use pickles to make a deep copy for modifications
  ;; works for any traced object (almost all M3 objects i.o.w.)
  ;;
  ;; 'parent field in copy is left null (if it exists)
  ;; requires stubs for Pickle interface
  
  (define (do-copy)
    (let ((wr (TextWr.New)))
      (Pickle.Write wr obj)
      (Pickle.Read (TextRd.New (TextWr.ToText wr)))))
  
  (if (have-field? obj 'parent)
      (let* ((save (get-field obj 'parent))
             (null (set-field! obj 'parent '()))
             (res  (do-copy)))
        (set-field! obj 'parent save)
        res)
      (do-copy)))

(define (get-field-or-false obj fn)
  (if (have-field? obj fn) (get-field obj fn) #f))

(define (force-string x)
  (if (string? x) x (stringify x)))
(define (only x)
  ;; get the only element from a singleton list
  (if (not (= 1 (length x)))
      (error "only : " (length x) " elements : " x)
      (car x)))

(define (schemify-wrap x)
   (if (closest-opped-supertype (rttype-typecode x))
       (cons 'm3 (schemify x))
       x)
   )


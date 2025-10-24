; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

;;
;;
;;

(define (fold-right procedure initial-value list-of-lists)
  (if (null? list-of-lists)
      initial-value
      (procedure (car list-of-lists)
                 (fold-right procedure 
                             initial-value 
                             (cdr list-of-lists)))))

(define (fold-left proc initial-value list-of-lists)
  (if (null? list-of-lists)
      initial-value
      (fold-left proc 
                 (proc initial-value (car list-of-lists))
                 (cdr list-of-lists))))

(define (fold-map proc initial-value list-input)
  (fold-left 
   (lambda (accumulator item)
     (cons (proc item) accumulator))
   initial-value 
   list-input))



(define (find-max lst)
  (fold-left (lambda (a b) (if (> b a) b a))
             
             (car lst)
             (cdr lst)))



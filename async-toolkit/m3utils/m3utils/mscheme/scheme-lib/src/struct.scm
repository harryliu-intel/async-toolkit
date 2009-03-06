;;
;; $Id$
;;

;;
;; "struct" types
;;
;; Copyright (c) 2009, Generation Capital Ltd.  All rights reserved.
;;
;; Author: Mika Nystrom <mika@alum.mit.edu>
;;
;; struct types consist of two different kinds of procedure-wrapped objects:
;;
;; 1. struct type objects, made with make-struct-type and optionally
;;    accessed with get-struct-type (we keep a global list)
;;
;;    (define t (make-struct-type 't type-specifier-list))
;;
;;    The type-specifier-list is a list of two-member lists.
;;    The first entry is a symbol, the name of a field; the second
;;    entry specifies the default value of the field.  If the second
;;    entry denotes a procedure, that procedure will be called (with no
;;    arguments) to make a new default value.
;;
;;    A (simple) real-life example:
;;
;;    (make-struct-type 
;;     'notifier-state
;;     `((last-notify         0                            )
;;       (last-state         ()                            )
;;       (history            ,(lambda()(make-history 10))  )
;;       )
;;     )
;;
;;
;;    operations on struct type objects:
;;
;;    (t 'fields)             ;; list the fields
;;   
;;    (t 'display)            ;; debug display
;;
;;    (t 'tag)                ;; the type name
;;
;;    (t 'new)                ;; make a new object of type t
;;
;; 2. instance objects, with operations:
;;    
;;    (i 'display)            ;; debug display
;;
;;    (i 'get <fn>)           ;; get field <fn>
;;
;;    (i 'set! <fn> <nv>)     ;; set field <fn> to new value <nv>
;;                                                                     
;;    (i 'inc! <fn> [<by>])   ;; for numerical fields: increment by 1 or <by>
;;                            
;;    (i 'inc! <fn> <lambda>) ;; apply <lambda> to <fn> and store in <fn>
;;
;;    (i 'type)               ;; retrieve type object (see above)
;;

(define struct-type-list '())

(define (make-struct-type name lst)
  (let ((new-type '()))   ;; could just use letrec here instead...
    (set! new-type
          (lambda x 
           (case (car x) 
             ((fields) (map car lst))

             ((display) lst)

             ((tag) name)

             ((new) 
              (let ((accessor '())  ;; will overwrite, could use letrec here 2
                    (val
                     (map (lambda (field-def)
                            (let ((initializer (cadr field-def)))
                              (if (procedure? initializer)
                                  (initializer)
                                  initializer)))
                          lst)))

                (set! accessor
                      (lambda y
                        (case (car y)
                          ((display) val)
                          
                          ((get)
                           (let ((fname (cadr y)))
                             (let loop ((vp val)
                                        (np lst))
                               (cond ((null? np)    
                                      (error "Unknown field " fname))
                                     ((eq? fname (caar np)) 
                                      (car vp))
                                     (else 
                                      (loop (cdr vp) (cdr np)))))))

                          ((type) new-type)
                          
                          ((inc!) (accessor 'set! 
                                            (cadr y) 
																						((cond ((null? (cddr y))
																										(lambda (x) (+ x 1)))
																									 ((procedure? (caddr y))
																										(caddr y))
																									 (else 
																										(lambda (x) (+ x (caddr y)))))

																						 (accessor 'get (cadr y))
																						)))
                          
                          ((set!)
                           (let ((fname (cadr y)))
                             (let loop ((vp val)
                                        (np lst))
                               (cond ((null? np)    
																			(error "Unknown field " fname))
                                     ((eq? fname (caar np)) 
                                      (set-car! vp (caddr y)))
                                     (else 
                                      (loop (cdr vp) (cdr np)))))))

                          (else (error "Unknown command " (car x)))

                          )
                        )
                      )
                accessor))

             (else (error "Unknown command " (car x) " on type " name))

             )))
    
    (set! struct-type-list (cons (cons name new-type) struct-type-list)) ))

(define (get-struct-type n) (cdr (assoc n struct-type-list)))


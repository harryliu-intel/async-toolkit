;;
;; $Id$
;;

;; set up Scheme - Modula-3 bindings

(define (install-procedures-without-exception-handlers)
  (map (lambda(pn) 
         (define-global-symbol (string->symbol
                                (string-append (car pn) "." (cdr pn)))
           (lambda x
             
             (scheme-procedure-stubs-call pn x '()))))
       (scheme-procedure-stubs-list)))

(install-procedures-without-exception-handlers)

;; basic support for wrapping objects, only to do methods
(define (obj-method-wrap obj type)
   (lambda args
      (modula-type-op type 'call-method obj (car args) (cdr args))))

;; does this type have any type ops registered?
(define (have-type-ops? tc) (not (null?  (list-modula-type-ops tc))))

(define (closest-opped-supertype tc)
   (cond ((have-type-ops? tc) tc)
         ((> tc (rttype-maxtypecode)) #f)
         (else (closest-opped-supertype (rttype-supertype
 tc)))))

(define (closest-type-op a . op)
   (let ((ctc (closest-opped-supertype (rttype-typecode a))))
        (apply modula-type-op (append (list ctc (car op) a) (cdr op)))))




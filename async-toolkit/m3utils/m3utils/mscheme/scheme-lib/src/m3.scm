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



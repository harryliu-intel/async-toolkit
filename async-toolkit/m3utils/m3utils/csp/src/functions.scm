;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; function stuff
;;

(define (function? func)
  (and (pair? func) (eq? 'function (car func))))

(define (check-is-function func)
  (if (not (function? func))
      (error "not a function : " func)))

(define (get-function-name func)
  (check-is-function func)
  (cadr func))

(define (get-function-formals func)
  (check-is-function func)

  ;; function decls are unconverted, so desugar declarators
  (map CspDeclarator.Lisp
       (map convert-declarator
            (apply append (caddr func)))))

(define (get-function-return func)
  (check-is-function func)
  (cadddr func))

(define (get-function-text func)
  (check-is-function func)

  ;; function decls are unconverted, so desugar here
  (desugar-stmt (caddddr func)))

(define (get-function-interfaces func)
  ;; list of captured identifiers of this function
  (let ((param-captures (map cadr (map cadr (get-function-formals func)))))
    (if (null? (get-function-return func))
        param-captures
        (cons (get-function-name func) param-captures))))

(define *uft-cp* #f)

(define (uniquify-function-text func sfx cell-info initvars)

  ;;
  ;; the identifiers referenced by functions can be put in three classes
  ;;
  ;; 1. local variables
  ;; 2. interface variables (parameters and return value)
  ;; 3. captured globals
  ;;
  ;; in CSP, captured globals are just CAST constants and channel
  ;; identifiers all else are either locals or interface variables
  ;;
  ;; When uniquifying a function (in preparation for inlining), we
  ;; rename all the locals and interfaces.  Captured globals are kept
  ;; as originally named.
  ;;
  ;; This code doesn't generate the function prolog or epilog.
  ;; That needs to be done elsewhere.
  ;;
  
  (let*((intf-vars          (get-function-interfaces func))
        (function-text      (get-function-text func))
        (body-vars          (find-referenced-vars function-text))
        (body-dummies       (get-all-dummies function-text))
        (body-var-captures  (set-intersection body-vars initvars))
        (body-chan-captures (set-intersection body-vars (get-port-ids cell-info)))
        (rename-ids         (set-diff (set-union body-vars
                                                 intf-vars)
                                      (set-union body-var-captures
                                                 body-chan-captures))))
    
  (dis "uniquify-function-text " (get-function-name func) " ====> " dnl)
  (dis "uniquify-function-text intf-vars          : " intf-vars dnl)
  (dis "uniquify-function-text body-vars          : " body-vars dnl)
  (dis "uniquify-function-text body-var-captures  : " body-var-captures dnl)
  (dis "uniquify-function-text body-chan-captures : " body-chan-captures dnl)
  (dis "uniquify-function-text rename-ids         : " rename-ids dnl)


  (let loop ((cp   rename-ids)
             (text (get-function-text func)))
    (if (null? cp)
        text
        (loop (cdr cp)
              (rename-id text
                         (car cp)
                         (symbol-append (car cp) sfx)))))))

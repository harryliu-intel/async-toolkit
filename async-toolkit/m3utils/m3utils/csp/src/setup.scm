(require-modules "basic-defs" "m3" "hashtable" "set"
                 "display")

(define *the-example* "parents_p2") ;; this is the test file

(define *big-m1* (BigInt.New -1))
(define *big-0*  (BigInt.New  0))
(define *big-1*  (BigInt.New  1))
(define *big-tc* (rttype-typecode *big-1*))

;; this stuff is really experimental.
(define *cell*        '())

(define *data*        '())
(define *cellinfo*    '())

(define funcs       '())
(define structs     '())
(define refparents  '())

(define declparents '()) ;; this is a special thing for env blocks

(define inits       '())
(define text        '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *the-text* #f)
(define *the-structs* #f)
(define *the-funcs* #f)
(define *the-inits* #f)
(define *the-initvars* #f)

(define *the-func-tbl* #f)
(define *the-struct-tbl* #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dis dnl "  ===  LOADING COMPILER ..." dnl dnl)
(load "cspc.scm")
(dis dnl "  ===  COMPILER LOADED." dnl dnl)

(define setup-loaded #t)

;;(loaddata! "arrays_p1")
;;(loaddata! "functions_p00")


(define a (BigInt.New 12))

;;(define csp (obj-method-wrap (convert-prog data) 'CspSyntax.T))

(define (do-analyze)
   (analyze-program lisp1 *cellinfo* *the-initvars*))


;; (reload)(loaddata! "castdecl_q")
;; (try-it *the-text* *cellinfo* *the-inits*)

(set-rt-error-mapping! #f)

(set-warnings-are-errors! #t)

(define lisp0 #f)
(define lisp1 #f)

;;(loaddata! *the-example*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if #f (begin
(define lispm1 (close-text *data*))
(define lisp0 (desugar-prog *data*))

(define lisp1 (simplify-stmt lisp0))

(define lisp2 (simplify-stmt
               ((obj-method-wrap (convert-stmt lisp1) 'CspSyntax.T) 'lisp)))

(define lisp3 (simplify-stmt
               ((obj-method-wrap (convert-stmt lisp2) 'CspSyntax.T) 'lisp)))

(define lisp4 (simplify-stmt
               ((obj-method-wrap (convert-stmt lisp3) 'CspSyntax.T) 'lisp)))

(if (not (equal? lisp1 lisp4)) (error "lisp1 and lisp4 differ!"))
))

;; (define b36 (BigInt.New 36))
;; (filter (lambda(s)(and (eq? 'SUPERSET (get-designator-id s)) (BigInt.Equal b36 (caddadr s)) (BigInt.Equal b15 (caddr s)))) z)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define inits1 #f)
(define text1 #f)
(define text2 #f)

(dis "  ===  TO RUN COMPILER: " dnl)
(dis "  ===  (loaddata! <csp-prefix>)" dnl)
(dis "  ===  (compile!)" dnl)
(dis dnl dnl dnl)



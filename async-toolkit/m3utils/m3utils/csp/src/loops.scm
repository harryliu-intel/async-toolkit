(define (loopex? x)
  (and (pair? x) (eq? 'loop-expression (car x))))

(define (get-loopex-dummy x) (cadr x))
(define (get-loopex-range x) (caddr x))
(define (get-loopex-op    x) (cadddr x))
(define (get-loopex-expr  x) (caddddr x))

(define (construct-loopex-binop x)
  ;; take a loop-expression and construct a dummy binary operation
  ;; ---> this will have the same type as the loop expression
  `(,(get-loopex-op x) ,(get-loopex-expr x)  ,(get-loopex-expr x) ))

(define (make-loopex-frame loopex syms)
  ;; construct a frame for use inside a loopex
  (let* ((new-frame (make-hash-table 1 atom-hash))
         (new-syms  (cons new-frame syms)))
    (define-var! new-syms (get-loopex-dummy loopex) *default-int-type*)
    new-syms)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-loop-dummy x) (cadr x))
(define (get-loop-range x) (caddr x))
(define (get-loop-stmt  x) (cadddr x))


(define (loop? s)
  (and (pair? s) (member (car s) '(loop parallel-loop sequential-loop))))

  
(define (get-loop-dummies prog)
  ;; multiset
  (define ids '())
  
  (define (s-visit s)
    (if (loop? s)
        (set! ids (cons (get-loop-dummy s) ids))
        )
    s
    )

  (visit-stmt prog s-visit identity identity)
  ids
  )

(define (get-loopex-dummies prog)
  ;; multiset
  (define ids '())
  
  (define (x-visit x)
    (if (loopex? x)
        (set! ids (cons (get-loopex-dummy x) ids)))
    x
    )

  (visit-stmt prog identity x-visit identity)
  ids
  )

(define (uniquify-loop-dummies stmt)
  ;; we need to uniquify the loop dummies in all kinds of angle-bracket
  ;; loops -- before we do anything else!
  ;; note that we *only* want to call this pass when we detect that
  ;; there are non-unique dummies.  Because it renames all the dummies!
  ;;
  ;; -- otherwise the entire compiler will loop!

  (dis "uniquify-loop-dummies" dnl)
  
  (define tg (make-name-generator "uniqify-loop"))
  
  (define (s-visitor s)
    (if (loop? s) (rename-id s (get-loop-dummy s) (tg 'next)) s)
    )

  (define (x-visitor x)
    (if (loopex? x)
        (visit-expr x

                    identity

                    (make-renaming-expr-visitor
                     (get-loopex-dummy x)
                     (tg 'next))

                    identity)
        x)
    )
  
  (visit-stmt stmt
              s-visitor
              x-visitor
              identity)
  )

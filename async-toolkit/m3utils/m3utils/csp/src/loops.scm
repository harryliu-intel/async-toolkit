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

(define (get-loop-dummy s) (cadr s))
(define (get-loop-range s) (caddr s))
(define (get-loop-stmt  s) (cadddr s))


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

(define (simple-range? r)
  (and (simple-operand? (cadr r)) (simple-operand? (caddr r))))


(define (build-integer-ass-sequence lhs rhs)
  (list
   (make-var1-decl (cadr lhs) *default-int-type*)
   (make-assign lhs rhs)
   )
  )

(define (unfold-loop-ranges prog)
  ;; this transformation introduces a new variable for the
  ;; loop range if the range consists of complex things.

  (define tg (make-name-generator "unfold-loop-range"))

  (define (s-visit s)
    (if (and (loop? s)
             (not (simple-range? (get-loop-range s))))
        
        (let* ((kw (car s))
               (dummy (get-loop-dummy s))
               (range (get-loop-range s))
               (stmt  (get-loop-stmt s))
               (rmin  (cadr range))
               (rmax  (caddr range))
               (rminv `(id ,(tg 'next)))
               (rmaxv `(id ,(tg 'next)))
               
               (minseq  (build-integer-ass-sequence rminv rmin))
               (maxseq  (build-integer-ass-sequence rmaxv rmax))

               (newrange `(range ,rminv ,rmaxv))

               (newloop         `(,kw ,dummy ,newrange ,stmt))
               )

          `(sequence ,@minseq ,@maxseq ,newloop)
          );;tel*
        s
        )
    )

  (visit-stmt prog s-visit identity identity)
  )
          

(define *lx* #f)


(define (generate-loop-statement-from-expression lhs rhs
                                                 syms func-tbl struct-tbl
                                                 cell-info)
  (set! *lx* rhs)
  (let* ((loop-op    (get-loopex-op rhs))
         (loop-idx   (get-loopex-dummy rhs))
         (loop-range (get-loopex-range rhs))
         (loop-expr  (get-loopex-expr rhs))
         (loop-frame (make-loopex-frame rhs syms))
         (loop-type  (derive-type loop-expr loop-frame func-tbl struct-tbl cell-info))
         (zero-elem  (op-zero-elem loop-op loop-type)))
    `(sequence
       (assign ,lhs
               ,zero-elem )
       (sequential-loop ,loop-idx
                        ,loop-range
                        (assign-operate ,loop-op ,lhs ,loop-expr)))))

(define (remove-loop-expression s syms vals tg func-tbl struct-tbl cell-info)

  (if (and (eq? 'assign (get-stmt-type s))
           (check-side-effects (get-assign-lhs s)))
      
      (let ( (lhs (get-assign-lhs s))
             (rhs (get-assign-rhs s))
             )
        (if (loopex? rhs)
            (generate-loop-statement-from-expression lhs rhs
                                                     syms func-tbl struct-tbl
                                                     cell-info)
            s))
      s)
  )


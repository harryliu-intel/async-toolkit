
(define (rename-id lisp from to)
  (define (expr-visitor x)

    (cond ((and (ident? x) (eq? from (cadr x)))
           (list 'id to))

          ((loopex? x)
           `(loop-expression
             ,(let ((dummy (get-loopex-dummy x)))
                (if (eq? dummy from) to dummy))
             ,(get-loopex-range x)
             ,(get-loopex-op x)
             ,(get-loopex-expr x)))
          
          (else x)
         )
   )
  
  (define (stmt-visitor s)
    ;; the only place that an identifier appears outside of expressions
    ;; is in "var1" statements -- and in function decls
    ;; and in loops...

    ;; and in waiting-if
    
    (case (get-stmt-type s)
      ((var1) (if  (equal? `(id ,from) (cadadr s))
                   `(var1 (decl1 (id ,to) ,@(cddadr s)))
                   s)
       )

      ((sequential-loop parallel-loop)
       (let ((kw     (car s))
             (idxvar (cadr s))
             (range  (caddr s))
             (stmt   (cadddr s)))

         (list kw (if (eq? idxvar from) to idxvar) range stmt)
         )
       )

      ((waiting-if)
       (define (visit-waiting-clause cl)
         (let ((dummy     (car cl))
               (sens      (cadr cl))
               (guardtext (caddr cl))
               (cmd       (cadddr cl)))
           (list (if (eq? dummy from) to dummy)
                 sens
                 guardtext
                 cmd)
           )))
      
      (else s)
      )
    )

  (visit-stmt lisp stmt-visitor expr-visitor identity)
)


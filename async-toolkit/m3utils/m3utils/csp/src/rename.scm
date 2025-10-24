; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define  (rename-dbg . x)
;;      (apply dis x)
    )

(define (make-renaming-range-visitor from to expr-visitor)
  (lambda(r)
        (rename-dbg "range-visitor " r dnl)
        (let ((res
               `(range ,(visit-expr (cadr r)  identity expr-visitor identity)
                       ,(visit-expr (caddr r) identity expr-visitor identity)))
              )
          (rename-dbg "range-visitor res " res dnl)
          res)
        )
  )
        
(define (make-renaming-expr-visitor from to)
  (lambda (x)
    (let expr-visitor ((x x))
      
      (define range-visitor (make-renaming-range-visitor from to expr-visitor))
      
      (cond ((and (ident? x) (eq? from (cadr x)))
             (list 'id to))
            
            ((loopex? x)
             `(loop-expression
               ,(let ((dummy (get-loopex-dummy x)))
                  (if (eq? dummy from) to dummy))
               ,(range-visitor (get-loopex-range x))
               ,(get-loopex-op x)
               ,(get-loopex-expr x)))
            
            (else x)
            )
      )
    )
  )

(define (rename-id stmt from to)

  (define x-visit (make-renaming-expr-visitor from to))
    
  (define (stmt-visitor s)
    ;; the only place that an identifier appears outside of expressions
    ;; is in "var1" statements -- and in function decls
    ;; and in loops...

    ;; and in waiting-if

    ;; and in ranges...

    (case (get-stmt-type s)
      ((var1) (if  (equal? `(id ,from) (cadadr s))
                   `(var1 (decl1 (id ,to) ,@(cddadr s)))
                   s)
       )

      ((sequential-loop parallel-loop)
       (let* ((kw      (car s))
              (idxvar  (cadr s))
              (r-visit (make-renaming-range-visitor from to x-visit))
              (range   (r-visit (caddr s)))
              (stmt    (cadddr s)))

         (rename-dbg "rename visitor " s dnl)
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

  (visit-stmt stmt
              stmt-visitor
              x-visit
              identity)
)


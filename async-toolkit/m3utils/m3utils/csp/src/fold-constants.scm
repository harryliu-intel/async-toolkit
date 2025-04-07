(define (handle-integer-binop x constant? constant-value)
  ;; x is a binary expression such as (+ a 2)
  ;; constant? is a procedure that checks whether an expression is constant
  (let ((op (car x))
        (a  (cadr x))
        (b  (caddr x)))
    (if (and (constant? a) (constant? b))
        (let* ((the-op-id (symbol-append 'big op))
               (the-op    (eval the-op-id)))
          (apply the-op (list (constant-value a) (constant-value b))))
        x
        )
    )
  )

(define *fold-stmts* '(var1 assign))

(define (constant-type? t)
  (and (pair? t)
       (member (car t) '(integer boolean string struct))
       (cadr t)))
        
(define (fold-constants-* stmt syms vals tg func-tbl struct-tbl)
;;  (dis "fold-constants-* " (if (pair? stmt) (car stmt) stmt) dnl)
  
  (define (constant? x)

    (dis "constant? " x dnl)
;;    (dis "symbols : " (map (lambda(tbl)(tbl 'keys)) syms) dnl)
    
    (cond ((bigint? x) #t)
          ((ident? x)
           (let* ((defn (retrieve-defn (cadr x) syms))
                  (is-const (constant-type? defn))
                 )
             (dis "constant? : x defn   : " defn dnl)
             (dis "constant? : is-const : " is-const defn dnl)
             is-const
             )
           )
          (else #f)))

  (define (constant-value x)
    (if (pair? x)
        (retrieve-defn (cadr x) vals)
        x)
  )

  (define (expr-visitor x)
    (dis "x = " x dnl)

    (cond ((not (pair? x)) x)

          ((member (car x) *integer-ops*)
           (handle-integer-binop x constant? constant-value))
          
          (else x)))

  (if (member (get-stmt-type stmt) *fold-stmts*)
      (let ((res (prepostvisit-stmt stmt
                                    identity identity
                                    expr-visitor identity
                                    identity identity)))
        
        (dis "res = " res dnl)
        res
        )
      stmt
      )
  )

(define (xx) (run-pass (list '* fold-constants-*) text2 *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl*))

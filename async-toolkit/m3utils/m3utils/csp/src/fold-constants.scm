(define (handle-integer-binop x constant? constant-value)
  ;; x is a binary expression such as (+ a 2)
  ;; constant? is a procedure that checks whether an expression is constant
  (let ((op (car x))
        (a  (cadr x))
        (b  (caddr x)))
    (if (and (constant? a) (constant? b))
        (let* ((the-op-id (symbol-append 'big op))
               (the-op    (eval the-op-id)))


;;          (dis "handle-integer-binop   op : " op dnl)
;;          (dis "handle-integer-binop   a  : " a dnl)
;;          (dis "handle-integer-binop   b  : " b dnl)

          (let ((ca (constant-value a))
                (cb (constant-value b)))
          ;; if literals, we apply the op, else we inline the value
          ;;
          (if (and (bigint? a) (bigint? b))
              (apply the-op (list ca cb))
              (list op ca cb)))))
          
        x
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

;;    (dis "constant? " x dnl)
;;    (dis "symbols : " (map (lambda(tbl)(tbl 'keys)) syms) dnl)
    
    (cond ((bigint? x) #t)
          ((ident? x)
           (let* ((defn (retrieve-defn (cadr x) syms))
                  (is-const (constant-type? defn))
                 )
;;             (dis "constant? : x defn   : " defn dnl)
;;             (dis "constant? : is-const : " is-const dnl)
             is-const
             )
           )
          (else #f)))

  (define (constant-value x)
    (let ((res 
           (if (pair? x)
               (let ((val (retrieve-defn (cadr x) vals)))
                 (dis "constant-value replacing " x " -> " val dnl)
                 val
                 )
               x)))
;;      (dis "constant-value x   : " x dnl)
;;      (dis "constant-value res : " res dnl)
      res
      )
  )

  (define (expr-visitor x)
;;    (dis "expr-visitor x = " x dnl)

    (cond ((not (pair? x)) x)

          ((member (car x) *integer-ops*)
           (handle-integer-binop x constant? constant-value))

          ((ident? x)
           (let ((res
                  (if (constant? x) (constant-value x) x)))

;;             (dis "expr-visitor ident? returning " res dnl)
             res))
           
          (else x)))

  (define (stmt-visitor s)
    (case (get-stmt-type s)
      ((assign)

       (dis "visiting assign " s dnl)
       
       `(assign
         ,(get-assign-lhs s)
         ,(expr-visitor (get-assign-rhs s))
         ))
      ((var1) (visit-stmt s identity expr-visitor identity))
      (else s)
      )
    )

  (if (member (get-stmt-type stmt) *fold-stmts*)
      (let ((res (prepostvisit-stmt stmt
                                    stmt-visitor identity
                                    identity identity
                                    identity identity)))
        
;;        (dis "fold-* res = " res dnl)
        res
        )
      stmt
      )
  )

(define (xx) (run-pass (list '* fold-constants-*) text2 *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl*))

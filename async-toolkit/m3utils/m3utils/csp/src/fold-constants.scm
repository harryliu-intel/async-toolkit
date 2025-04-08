;; handle-XXX-binop handles an operation that RETURNS an XXX
;; inputs can be polymorphic

(define debug #f)

(define (handle-integer-binop x constant? constant-value)
  ;; x is a binary expression such as (+ a 2)
  ;; constant? is a procedure that checks whether an expression is constant
  (let ((op (car x))
        (a  (cadr x))
        (b  (caddr x)))
    (if (and (constant? a) (constant? b))
        (let* ((the-op-id (symbol-append 'big op))
               (the-op    (eval the-op-id)))


          (dis "handle-integer-binop   op : " op dnl)
          (dis "handle-integer-binop   a  : " a dnl)
          (dis "handle-integer-binop   b  : " b dnl)

          (let ((ca (constant-value a))
                (cb (constant-value b)))
          ;; if literals, we apply the op, else we inline the value
          ;;
          (if (and (bigint? ca) (bigint? cb))
              (begin (let ((res (apply the-op (list ca cb))))
                       (dis "performing op " the-op " : " res dnl)
                       res)
                     )
              (list op ca cb))))
          
        x
        )
    )
  )

(define (make-boolean-binop nm arg-type text)
  (list nm arg-type text))

(define (xor a b)(not (eq? a b)))

(define *boolean-binops*
  (list
   `(== boolean ,eq?)
   `(== integer ,BigInt.Equal)
   `(!= integer ,(lambda(a b)(not (BigInt.Equal a b))))
   `(<  integer ,(lambda(a b)(<   (BigInt.Compare a b) 0)))
   `(>  integer ,(lambda(a b)(>   (BigInt.Compare a b) 0)))
   `(>= integer ,(lambda(a b)(>=  (BigInt.Compare a b) 0)))
   `(<= integer ,(lambda(a b)(<=  (BigInt.Compare a b) 0)))
   `(^  boolean ,xor)
   `(&  boolean ,and)
   `(|  boolean ,or) ;; |)
   `(&& boolean ,and)
   `(|| boolean ,or)))

(define (handle-boolean-binop x constant? constant-value syms func-tbl struct-tbl)
  (let ((op (car x))
        (a  (cadr x))
        (b  (caddr x)))
    (if (and (constant? a) (constant? b))
        (let* ((the-op-id (symbol-append 'boolean op)))


          (if debug
              (dis "handle-boolean-binop   op : " op dnl)
              (dis "handle-boolean-binop   a  : " a dnl)
              (dis "handle-boolean-binop   b  : " b dnl)
              )

          (let* ((ca (constant-value a))
                 (cb (constant-value b))
                 (ty (derive-type a syms func-tbl struct-tbl))
                 (type (if (pair? ty) (car ty) ty)))
                     
          ;; if literals, we apply the op, else we inline the value
            ;;
            (dis "ca = " ca dnl)
            (dis "cb = " cb dnl)
            (dis "type = " type dnl)

            (let loop ((p *boolean-binops*))
              (cond ((null? p)      x ;; not found
                     )

                    ((and (eq? op (caar p))
                          (eq? type (cadar p)))
                     (let ((res  ((caddar p) ca cb)))
                       (dis "boolean-binop (" op " " ca " " cb ") = " res dnl)
;;                       (error)
                       res))

                    (else (loop (cdr p)))))
            ))
        
        x
        )
    )
  )

(define (handle-string-binop x constant? constant-value)
  ;; x is a binary expression such as (+ a 2)
  ;; constant? is a procedure that checks whether an expression is constant
  (let ((op (car x))
        (a  (cadr x))
        (b  (caddr x)))
    (if (and (constant? a) (constant? b))
        (let* ((the-op-id (symbol-append 'string op))
               (the-op    (eval the-op-id)))


          (dis "handle-string-binop   op : " op dnl)
          (dis "handle-string-binop   a  : " a dnl)
          (dis "handle-string-binop   b  : " b dnl)

          (let ((ca (constant-value a))
                (cb (constant-value b)))
          ;; if literals, we apply the op, else we inline the value
          ;;
          (if (and (string? ca) (string? cb))
              (begin (let ((res (apply the-op (list ca cb))))
                       (dis "performing op " the-op " : " res dnl)
                       res)
                     )
              (list op ca cb))))
          
        x
        )
    )
  )

(define *fold-stmts* '(var1 assign sequential-loop parallel-loop))

(define (constant-type? t)
  (and (pair? t)
       (member (car t) '(integer boolean string struct))
       (cadr t)))
        
(define (fold-constants-* stmt syms vals tg func-tbl struct-tbl)
  (if debug (dis "fold-constants-* " (if (pair? stmt) (car stmt) stmt) dnl))
  
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
      (if debug
          (dis "constant-value x   : " x dnl)
          (dis "constant-value res : " res dnl)
          )
      res
      )
  )

  
  (define (expr-visitor x)
    (if debug (dis "expr-visitor x = " x dnl))

    (cond ((not (pair? x)) x)

          ((ident? x)
           (let ((res
                  (if (constant? x) (constant-value x) x)))

             (dis "expr-visitor ident? returning " res dnl)
             res))
           
          ((integer-expr? x syms func-tbl struct-tbl)
           (handle-integer-binop x constant? constant-value))

          ((boolean-expr? x syms func-tbl struct-tbl)
           (handle-boolean-binop x constant? constant-value syms func-tbl struct-tbl))

          ((string-expr? x syms func-tbl struct-tbl)
           (handle-string-binop x constant? constant-value))

          (else x)))

  (define (stmt-visitor s)
    (if #t (dis "fold-* stmt-visitor " s dnl))

    ;;
    ;; This is dumb: we are replicating parts of "stmt-check-enter"
    ;; from the main compiler loop here.
    ;;
    
    (case (get-stmt-type s)
      ((assign)


       (set! debug #t)
       (if debug (dis "visiting assign " s dnl))
       (let ((res `(assign
                    ,(get-assign-lhs s)
                    ,(visit-expr (get-assign-rhs s) identity expr-visitor identity))))
         (if debug (dis "returning assign " res dnl))
         (if (ident? (get-assign-lhs s))

             (define-var! vals (cadr (get-assign-lhs res)) (get-assign-rhs res))
             )
         res
         )
       )
      
      ((var1)
       (define-var! syms (get-var1-id s) (get-var1-type s))
       (dis "defining " (get-var1-id s) dnl)
       (set! debug #f)
       (if debug (dis "visiting var1 " s dnl))
       (let ((res (visit-stmt s identity expr-visitor identity)))
         (if debug (dis "returning var1 " res dnl))
         (set! debug #f)
         res
         )
       )

      ((loop-expression)
       (define-var! syms (get-loopex-dummy s) *default-int-type*)
       s
       )

      ((sequential-loop parallel-loop)
       (let ((loop-type  (car s))
             (loop-idx   (cadr s))
             (loop-range (caddr s))
             (loop-stmt  (cadddr s)))
         (dis "loop " s dnl)
         (define-var! syms loop-idx *default-int-type*)
         `(,loop-type
           ,loop-idx
           ,(visit-range loop-range identity expr-visitor identity)
           ,loop-stmt)))
      
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

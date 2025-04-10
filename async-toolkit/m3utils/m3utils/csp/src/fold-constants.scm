;; handle-XXX-binop handles an operation that RETURNS an XXX
;; inputs can be polymorphic

(define (dbg . x)  (if debug (apply dis x)))

(define (handle-integer-binop x constant? constant-value)
  ;; x is a binary expression such as (+ a 2)
  ;; constant? is a procedure that checks whether an expression is constant
  (let ((op (car x))
        (a  (cadr x))
        (b  (caddr x)))
    (if (and (constant? a) (constant? b))
        (let* ((the-op-id (symbol-append 'big op))
               (the-op    (eval the-op-id)))


          (dbg "handle-integer-binop   op : " op dnl)
          (dbg "handle-integer-binop   a  : " a dnl)
          (dbg "handle-integer-binop   b  : " b dnl)

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

(define (handle-integer-unop x constant? constant-value)
  ;; x is a unary expression such as (- a)
  ;; constant? is a procedure that checks whether an expression is constant
  (let ((op (car x))
        (a  (cadr x)))

    (if (and (constant? a))
        (let* ((the-op-id (symbol-append 'big op))
               (the-op    (eval the-op-id)))

          (dbg "handle-integer-unop   op : " op dnl)
          (dbg "handle-integer-unop   a  : " a dnl)

          (let ((ca (constant-value a)))
          ;; if literals, we apply the op, else we inline the value
          ;;
          (if (and (bigint? ca))
              (begin (let ((res (apply the-op (list ca))))
                       (dis "performing op " the-op " : " res dnl)
                       res)
                     )
              (list op ca))))
          
        x
        )
    )
  )

(define (make-boolean-binop nm arg-type text)
  (list nm arg-type text))

(define (xor a b)(not (eq? a b)))

;; there is one annoyance with boolean binary operators
;; we have ==, test for equality.  Its arguments can either be
;; boolean or integers.

(define *boolean-unops*
  (list
   `(not boolean ,not)))

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

(define (handle-boolean-binop
         x constant? constant-value syms func-tbl struct-tbl)
  (let ((op (car x))
        (a  (cadr x))
        (b  (caddr x)))
    (if (and (constant? a) (constant? b))
        (let* ((the-op-id (symbol-append 'boolean op)))


          (if debug
              (dbg "handle-boolean-binop   op : " op dnl)
              (dbg "handle-boolean-binop   a  : " a dnl)
              (dbg "handle-boolean-binop   b  : " b dnl)
              )

          (let* ((ca (constant-value a))
                 (cb (constant-value b))
                 (ty (derive-type a syms func-tbl struct-tbl))
                 (type (car ty)))
                     
          ;; if literals, we apply the op, else we inline the value
            ;;
            (dbg "ca = " ca dnl)
            (dbg "cb = " cb dnl)
            (dbg "type = " type dnl)

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

(define (handle-boolean-unop
         x constant? constant-value syms func-tbl struct-tbl)
  (let ((op (car x))
        (a  (cadr x)))
    (if (and (constant? a))
        (let* ((the-op-id (symbol-append 'boolean op)))


          (if debug
              (dbg "handle-boolean-unop   op : " op dnl)
              (dbg "handle-boolean-unop   a  : " a dnl)
              )

          (let* ((ca (constant-value a))
                 (ty (derive-type a syms func-tbl struct-tbl))
                 (type (car ty)))
                     
            ;; if literal, we apply the op, else we inline the value
            ;;
            (dbg "ca = " ca dnl)
            (dbg "type = " type dnl)

            (let loop ((p *boolean-unops*))
              (cond ((null? p)      x ;; not found
                     )

                    ((and (eq? op (caar p))
                          (eq? type (cadar p)))
                     (let ((res  ((caddar p) ca)))
                       (dis "boolean-unop (" op " " ca ") = " res dnl)
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


          (dbg "handle-string-binop   op : " op dnl)
          (dbg "handle-string-binop   a  : " a dnl)
          (dbg "handle-string-binop   b  : " b dnl)

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

(define *fold-stmts* '(var1 assign sequential-loop parallel-loop eval local-if))

(define (constant-type? t)
  (and (pair? t)
       (member (car t) '(integer boolean string structure))
       (cadr t)))

(define (make-constant-type t)
  ;; take type t but mark it as constant

  (if (and (pair? t)
           (member (car t) '(integer boolean string structure)))
      (cons (car t) (cons #t (cddr t)))
      (error "make-constant-type : cant make constant from : " t dnl)
      ))

(define (constant-simple? x syms)
  
  ;;    (dbg "constant? " x dnl)
  ;;    (dbg "symbols : " (map (lambda(tbl)(tbl 'keys)) syms) dnl)
    
  (cond ((literal? x) x)
        ((ident? x)
         (let* ((defn (retrieve-defn (cadr x) syms))
                (is-const (constant-type? defn))
                )
             (dbg "constant? : x defn   : " defn dnl)
             (dbg "constant? : is-const : " is-const dnl)
           (if is-const defn #f)
           )
         )
        (else #f)))

(define fold-stmt-visit #f)

(define (call-intrinsic? x)
  (and (pair? x) (eq? 'call-intrinsic (car x))))

(define (fold-constants-* stmt syms vals tg func-tbl struct-tbl)
  (dbg "fold-constants-* " (if (pair? stmt) (car stmt) stmt) dnl)

  (define (constant? x) (constant-simple? x syms))

  (define (constant-value x)
    (let ((res 
           (if (pair? x)
               (let ((val (retrieve-defn (cadr x) vals)))
                 (dbg "constant-value replacing " x " -> " val dnl)
                 val
                 )
               x)))
      (if debug
          (dbg "constant-value x   : " x dnl)
          (dbg "constant-value res : " res dnl)
          )
      res
      )
  )

  
  (define (expr-visitor x)
    (dbg "expr-visitor x = " x dnl)

    (cond ((not (pair? x)) x)

          ((ident? x)

;;           (dis "expr-visitor x " x dnl)
;;           (dis "expr-visitor constant? " (constant? x) dnl)
           
           (let ((res
                  (if (constant? x) (constant-value x) x)))

;;             (dbg "expr-visitor ident? returning " res dnl)
             res))
           
          ((call-intrinsic? x)
           (let ((res
                  (cons 'call-intrinsic
                        (cons (cadr x) 
                              (map
                               (lambda(xx)(visit-expr xx
                                                      identity
                                                      expr-visitor
                                                      identity))
                               (cddr x))))))
             
             (dis "call-intrinsic x   " x dnl)
             (dis "call-intrinsic res " res dnl)
             res))
      
          ((integer-expr? x syms func-tbl struct-tbl)
           (cond ((= (length x) 3)
                  (handle-integer-binop x constant? constant-value))
                 ((= (length x) 2)
                  (handle-integer-unop x constant? constant-value))
                 (else (error)))
           )

          ((boolean-expr? x syms func-tbl struct-tbl)
           (cond ((= (length x) 3)
                  (handle-boolean-binop
                   x constant? constant-value syms func-tbl struct-tbl))
                 ((= (length x) 2)
                  (handle-boolean-unop
                   x constant? constant-value syms func-tbl struct-tbl))))

          ((string-expr? x syms func-tbl struct-tbl)
           (handle-string-binop x constant? constant-value))

          (else x)))

  (define (expr x)
    (visit-expr x identity expr-visitor identity)
    )
  
  (define (stmt-visitor s)
;;    (if #t (dis "fold-* stmt-visitor " s dnl))

    ;;
    ;; This is dumb: we are replicating parts of "stmt-check-enter"
    ;; from the main compiler loop here.
    ;;

    (set! fold-stmt-visit s)
    
    (case (get-stmt-type s)
      ((assign)


;;       (dbg "visiting assign " s dnl)
       (let ((res `(assign
                    ,(get-assign-lhs s)
                    ,(expr (get-assign-rhs s)))))
;;         (dbg "returning assign " res dnl)
         (if (ident? (get-assign-lhs s))

             (define-var! vals (cadr (get-assign-lhs res)) (get-assign-rhs res))
             )
         res
         )
       )
      
      ((var1)
       (define-var! syms (get-var1-id s) (get-var1-type s))
       (dbg "defining " (get-var1-id s) dnl)
       (if debug (dbg "visiting var1 " s dnl))
       (let ((res (visit-stmt s identity expr-visitor identity)))
         (dbg "returning var1 " res dnl)
         res
         )
       )

      ((loop-expression)
       (define-var! syms (get-loopex-dummy s) *default-int-type*)
       s
       )
      
      ((eval)
;;       (dis "VISITING eval" dnl)
       
       (list 'eval (expr (cadr s))))

      ((sequential-loop parallel-loop)
       (let ((loop-type  (car s))
             (loop-idx   (cadr s))
             (loop-range (caddr s))
             (loop-stmt  (cadddr s)))
         (dbg "loop " s dnl)
         (define-var! syms loop-idx *default-int-type*)
         `(,loop-type
           ,loop-idx
           ,(visit-range loop-range identity expr-visitor identity)
           ,loop-stmt)))

      ((XXXlocal-if)
       (let* ((guards     (map car (cdr s)))
              (stmts      (map cadr (cdr s)))
              (guardvals  (map expr guards)))

         (dis "local-if guards    " guards dnl)
         (dis "local-if stmts     " stmts  dnl)
         (dis "local-if guardvals " guardvals dnl)

         (let ((res
                `(local-if ,@(map list guardvals stmts))))
           (dis "local-if ...>      " res dnl)
           res
;;           (error)
           )
               

         
         )
       )

      (else s)
      
      )
    )

  (dis "fold-* visiting " stmt dnl)

  ;; why are we calling visit-stmt recursively here?  Is that
  ;; really necessary?
  
  (if (member (get-stmt-type stmt) *fold-stmts*)
      (let ((res (prepostvisit-stmt stmt
                                    stmt-visitor identity
                                    identity identity
                                    identity identity)))
        
;;        (dbg "fold-* res = " res dnl)
        res
        )
      stmt
      )
  )

(define (xx) (run-pass (list '* fold-constants-*) text2 *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl*))

(define (fold-constants prog)
  (run-pass (list '* fold-constants-*) prog *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl*))

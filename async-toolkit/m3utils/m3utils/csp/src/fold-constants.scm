;; handle-XXX-binop handles an operation that RETURNS an XXX
;; inputs can be polymorphic

(define (dbg . x)
  (apply dis x) ;; comment this out to make it quiet
  )

(define (handle-intrinsic name constant? constant-value arg-list)
  (case name
    ((log2)
     (let ((arg (car arg-list)))
       (if (constant? arg)
           (let* ((ca (constant-value 'integer arg))
                  (result (xnum-log2 ca)))

             (dis "performing intrinsic : (log2 " arg " <- " ca ") = " result dnl)
             result
             )
             
           `(call-intrinsic ,name ,arg))
       )
     )
    (else (error "handle-intrinsic of " name)))
  )

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

          (let ((ca (constant-value 'integer a))
                (cb (constant-value 'integer b)))
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

          (let ((ca (constant-value 'integer a)))
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

(define *boolean-binops*
  (list
   `(== integer ,BigInt.Equal) ;; must be first in the list
   `(== boolean ,eq?)
   `(!= integer ,(lambda(a b)(not (BigInt.Equal a b))))
   `(<  integer ,(lambda(a b)(<   (BigInt.Compare a b) 0)))
   `(>  integer ,(lambda(a b)(>   (BigInt.Compare a b) 0)))
   `(>= integer ,(lambda(a b)(>=  (BigInt.Compare a b) 0)))
   `(<= integer ,(lambda(a b)(<=  (BigInt.Compare a b) 0)))

   ;; * below here doesnt mean bitwise operations.
   ;; it means that the operands will be coerced to boolean
   `(^  *       ,xor)
   `(&  *       ,and)
   `(|  *       ,or) ;; |)
   `(&& *       ,and)
   `(|| *       ,or)))

(define *boolean-unops*
  (list
   `(not * ,not)))

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

          (let* ((ty (derive-type a syms func-tbl struct-tbl))
                 (type (car ty)))
                     
            (let loop ((p *boolean-binops*))
              (let* ((bool-op   (caar p))
                     (bool-type (cadar p))
                     (bool-impl (caddar p)))
                (cond ((null? p)      x ;; not found
                       )
                      ((and (eq? op bool-op)
                            (or (eq? type bool-type) (eq? bool-type '*)))

                       ;; operation and type match
                       
                       (let* (
                              ;; coerce the types
                              (ca (constant-value bool-type a))
                              (cb (constant-value bool-type b))

                              (res  (bool-impl ca cb))

                              )
                         (dis "boolean-binop (" op " " ca " " cb ") = " res dnl)
                         res))
                      
                      (else (loop (cdr p)))
                      );;dnoc
            ))));;*tel
        x
        );;fi
    );;tel
  );;enifed

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

          (let* ((ca (constant-value 'boolean a))
                 (ty (derive-type a syms func-tbl struct-tbl))
                 (type (car ty)))
                     
            ;; if literal, we apply the op, else we inline the value
            ;;
            (dbg "ca = " ca dnl)
            (dbg "type = " type dnl)

            (if (not (boolean? ca)) (error "not a boolean : ca = " ca))

            (let loop ((p *boolean-unops*))
              (let ((bool-op   (caar p))
                    (bool-type (cadar p))
                    (bool-impl (caddar p)))
              (cond ((null? p)      x ;; not found
                     )

                    ((and (eq? op bool-op)
                          (or (eq? type bool-type) (eq? bool-type '*)))

                     (let ((res  (bool-impl ca)))
                       (dis "boolean-unop (" op " " ca ") = " res dnl)
                       res))

                    (else (loop (cdr p))))))
            ))
        x
        )
    )
  )

(define string+ string-append) ;; this little trickery makes + work on strings

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

          (let ((ca (constant-value 'string a))
                (cb (constant-value 'string b)))
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
  
  (dbg "constant? " x dnl)
  (dbg "symbols : " (map (lambda(tbl)(tbl 'keys)) syms) dnl)

  ;; this has to return a list containing the value
  ;; #f is a constant
  (cond ((literal? x) (list x))
        ((ident? x)
         (let* ((defn (retrieve-defn (cadr x) syms))
                (is-const (constant-type? defn))
                )
             (dbg "constant? : x defn   : " defn dnl)
             (dbg "constant? : is-const : " is-const dnl)
           (if is-const (list defn) #f)
           )
         )
        (else #f)))

(define fold-stmt-visit #f)

(define (call-intrinsic? x)
  (and (pair? x) (eq? 'call-intrinsic (car x))))

(define (coerce-type type res)
  (define (*? _) #t) ;; a type request that matches all types

  (define integer? bigint?) ;; don't pollute the environment

  (define (do-error)
    (error "can't convert " (stringify res) " to " type))

  (let ((checker (eval (symbol-append type '?))))
    (if (checker res)
        res
        (case type  
          ((integer)           (do-error)) ;; no integer conversions
          
          ((boolean)
           (if (bigint? res)
               (if (eq? *big0* res) #f #t)
               (do-error))
           )
          
          ((string)
           (cond ((bigint? res)  (BigInt.Format res 10))
                 ((boolean? res)
                  (if res "-1" "0"))
                 (else (do-error)))
           )
          
          (else (do-error))
          
          );;esac
        );;fi
    )
  )

(define (coerce-type-if-literal type x)
  (if (literal? x) (coerce-type type x) x))

(define *xgs* #f)

(define (fold-constants-* stmt syms vals tg func-tbl struct-tbl)
  (dbg "fold-constants-* " (if (pair? stmt) (car stmt) stmt) dnl)

  (define (constant? x) (constant-simple? x syms))

  (define (constant-value type x)

    (let ((res 
           (if (pair? x)
               (let ((val (retrieve-defn (cadr x) vals)))
                 (dbg "constant-value replacing " x " -> " val dnl)
                 val
                 )
               x)))
      (dbg "constant-value x    : " x dnl)
      (dbg "constant-value type : " type dnl)
      (dbg "constant-value res  : " res dnl)

      (coerce-type type res)
      )
    )
  
  (define (expr requested-type)
    (lambda(the-expr)
      (coerce-type-if-literal
       requested-type
       (visit-expr the-expr identity expr-visitor identity)))
    )
  
  (define (expr-visitor x)
    (dbg "expr-visitor x = " x dnl)
    
    (cond ((not (pair? x)) x)
          
          ((ident? x)
           
           (dbg "expr-visitor x " x dnl)
           (dbg "expr-visitor constant? " (constant? x) dnl)
           
           (let ((res
                  (if (constant? x) (constant-value '* x) x)))
             
             (dbg "expr-visitor ident? returning " res dnl)
             res))
          
          ((call-intrinsic? x)
           (let* ((nam (cadr x)))

             (if (member nam '(log2))
                 (handle-intrinsic nam
                                   constant?
                                   constant-value
                                   (cddr x))

                 
                 (let((res
                       (cons 'call-intrinsic
                             (cons (cadr x) 
                                   (map
                                    (lambda(xx)(visit-expr xx
                                                           identity
                                                           expr-visitor
                                                           identity))
                                    (cddr x))))))
                   
                   (dbg "call-intrinsic x   " x dnl)
                   (dbg "call-intrinsic res " res dnl)
                   res)))
           )
          
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

          ((array-access? x)
           (let* ((base (get-designator-id x))
                  (ilist (get-index-list x))
                  (data  (*the-globals* 'retrieve base))
                  (value (if (or (not ilist)
                                 (eq? data '*hash-table-search-failed*))
                             '*hash-table-search-failed*
                             (data 'retrieve ilist))))
             (if (eq? value '*hash-table-search-failed*)
                 x
                 value))
           )
          
          (else x)))
  

  (define (stmt-visitor s)
    (dbg "fold-* stmt-visitor " s dnl)

    ;;
    ;; This is dumb: we are replicating parts of "stmt-check-enter"
    ;; from the main compiler loop here.
    ;;

    (set! fold-stmt-visit s)
    
    (case (get-stmt-type s)
      ((assign)


       (dbg "visiting assign " s dnl)
;;       (error)
       (let ((res `(assign
                    ,(get-assign-lhs s)
                    ,((expr '*) (get-assign-rhs s)))))
         (dbg "returning assign " res dnl)

         (if (procedure? (get-assign-rhs res))
             (error "garbage!"))
         
         (if (ident? (get-assign-lhs s))

             (define-var!
               vals
               (cadr (get-assign-lhs res))
               (get-assign-rhs res))
             )
         res
         )
       )
      
      ((var1)
       (define-var! syms (get-var1-id s) (get-var1-type s))
       (dbg "var1 : defining  " (get-var1-id s) dnl)
       (dbg "var1 : visiting  " s dnl)
       (let ((res (visit-stmt s identity expr-visitor identity)))
         (dbg "var1 : returning " res dnl)
         res
         )
       )

      ((loop-expression)
       (define-var! syms (get-loopex-dummy s) *default-int-type*)
       s
       )
      
      ((eval)
;;       (dbg "VISITING eval" dnl)
       
       (list 'eval ((expr '*) (cadr s))))

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

      ((local-if)
       (let* ((guards     (map car (cdr s)))
              (stmts      (map cadr (cdr s)))
              (guardvals  (map (expr 'boolean) guards))
              )

         (dbg "local-if guards    " guards dnl)
         (dbg "local-if stmts     " stmts  dnl)
         (dbg "local-if guardvals " guardvals dnl)

         (let* ((xgs  (map list guardvals stmts))
                (fxgs (filter car xgs))
                )

           (dbg "local-if xgs       " xgs dnl)
           (dbg "local-if fxgs      " fxgs dnl)

           (set! *xgs* xgs)

           (if (member #t (map car fxgs))
               (let ((the-result
                      (cadr (assoc #t fxgs))))   ;; there is a #t guard, use it
                 (dis "removing if : " s " -> " the-result dnl)
                 the-result)
               (cons 'local-if fxgs)    ;; no #t, keep the simplified gcs

               )
           ) 

         
         )
       )

      (else s)
      
      )
    )

  (dbg "fold-* visiting " stmt dnl)

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

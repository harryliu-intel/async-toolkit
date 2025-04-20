(define *has-ass* #f)

;; type of an expression

(define *boolean-ops*
  ;; these create booleans all on their own
  '(probe not == != < > >= <= && == || ) ;; | )
  )

(define *integer-ops*
  ;; these create integers all on their own
  '(- / % * << >> **))

(define *string-ops*
  ;; these create strings all on their own
  '())

(define *polymorphic-ops*
  ;; these create objects whose types depend on operands
  '(+ & ^ | ) ;; | )
  )

(define (op-zero-elem op type)
  ;; zero element for iterated ops
  (case op
    ((&& ==)    #t)
    ((||)       #f)
    ((-)        *big0*)
    ((* / **)   *big1*)
    ((+)
     (cond ((string-type? type)    "")
           ((integer-type? type)   *big0*)
           (else (error "???op-zero-elem of : " op " : for type : " type))))

    ((&)
     (cond ((boolean-type? type)   #t)
           ((integer-type? type)  *bigm1*)
           (else (error "???op-zero-elem of : " op " : for type : " type))))
    
    ((^ |) ;; |)
     (cond ((boolean-type? type)   #f)
           ((integer-type? type)   *big0*)
           (else (error "???op-zero-elem of : " op " : for type : " type))))
    (else (error "???op-zero-elem of : " op))))
    
(define (integer-type? t)
  (and (pair? t) (eq? 'integer (car t))))

(define (integer-expr? x syms func-tbl struct-tbl cell-info)
  (or (bigint? x)
      (if (pair? x)
          (cond
           ((eq? 'id (car x)) (integer-type?
                                   (car (retrieve-defn (cadr x) syms))))
           ((member (car x) *integer-ops*) #t)
           ((member (car x) *polymorphic-ops*)
            (integer-type? (derive-type (cadr x) syms func-tbl struct-tbl cell-info)))
           (else #f)
           )
          #f
          )))

(define (boolean-expr? x syms func-tbl struct-tbl cell-info)
  (or (boolean? x)
      (if (pair? x)
          (cond
           ((eq? 'id (car x)) (boolean-type? (retrieve-defn (cadr x) syms)))
           ((member (car x) *boolean-ops*) #t)
           ((member (car x) *polymorphic-ops*)
            (boolean-type? (derive-type (cadr x) syms func-tbl struct-tbl cell-info)))
           (else #f)
           )
          #f
          )))

(define (string-expr? x syms func-tbl struct-tbl cell-info)
  (or (string? x)
      (if (pair? x)
          (cond
           ((eq? 'id (car x)) (string-type? (retrieve-defn (cadr x) syms)))
           ((member (car x) *string-ops*) #t)
           ((member (car x) *polymorphic-ops*)
            (string-type? (derive-type (cadr x) syms func-tbl struct-tbl cell-info)))
           (else #f)
           )
          #f
          )))

(define (array-access? x)
  (and (pair? x)(eq? 'array-access (car x))))

(define (peel-array t)
  (if (or (not (pair? t))
          (not (eq? 'array (car t))))
      (error "not an array type " t)
      (caddr t)))

(define (array-accessee x)
  (if (not (array-access? x))
      (error "array-accessee : not an array access : " x)
      (cadr x)))

(define (member-access? x)
  (and (pair? x)(eq? 'member-access (car x))))

(define (member-accessee x)
  (if (not (member-access? x))
      (error "member-accessee : not a member access : " x)
      (cadr x)))

(define (member-accesser x)
  (if (not (member-access? x))
      (error "member-accesser : not a member access : " x)
      (caddr x)))

(define (simple-operand? x) (or (literal? x)(ident? x)))

(define (literal? x)
  (or (boolean? x)
      (bigint? x)
      (string? x)))




(define (make-binop op lst)
  ;; given a list of expressions, make binary op across them
  (cond ((null? lst) (error "make-binop of empty list"))
        ((not (list? lst)) (error "make-binop : not a list : " lst))
        ((= 1 (length lst)) (car lst))
        ((= 2 (length lst)) `(,op ,(car lst) ,(cadr lst)))
        (else `(,op ,(car lst) ,(make-binop op (cdr lst)))))
  )


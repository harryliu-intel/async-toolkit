                                        ; cspc.scm ;
                                        ;

(require-modules "basic-defs" "m3" "hashtable" "display")

(define (caddddr x) (car (cddddr x)))

(define (load-csp fn)

  (let* ((p (open-input-file fn))
         (res (read-big-int p)))
    (close-input-port p)
    res
    )
  )

(define data '())
(define funcs '())
(define structs '())
(define refparents '())
(define declparents '())
(define inits '())
(define text '())

(define (loaddata! nm)
  (begin
    (set! data (cadr (load-csp (string-append nm ".scm"))))

    (set! funcs (nth data 1))
    (set! structs (nth data 2))
    (set! refparents (nth data 3))      ;
    (set! declparents (nth data 4))
    (set! inits (nth data 5))
    (set! text (nth data 6))

    ))


(loaddata! "p1")


    

(define (pp lst) ;; pretty-print
  (dis "(" dnl)
  (map (lambda (c) (dis c dnl)) lst)
  (dis ")" dnl)
  #t)

(define (make-var name type)
  (cons name type))

(define (make-var-hash-table size)
  (make-hash-table size (lambda(var)(Atom.Hash (car var)))))

(define *bad-s* #f)
(define *bad-last* #f)

(define *s* #f)
(define *last* #f)
(define *var-s* #f)

(define (init-seq type)
  (let ((res (obj-method-wrap (new-modula-object type) type)))
    (obj-method-wrap (res 'init 10) type)))

(define *last-var* #f)

(define (convert-dir dir)
  (case dir
    ((none) 'None)
    ((in)   'In)
    ((out)  'Out)
    ((inout) 'InOut)
    (else (error "convert-dir : unknown direction : " dir))
    )
  )

(define (convert-range r)
  (if (or (not (pair? r))
          (not (eq? (car r) 'range)))
      (error "not a range : " r)
      `((min . ,(convert-expr (cadr r)))
        (max . ,(convert-expr (caddr r))))))

(define (convert-type type)
  (cond
   ((eq? type 'boolean) (CspAst.BooleanType))
   ((eq? type 'string)  (CspAst.StringType))
   ((pair? type)
    (case (car type)
      ((array)       (CspAst.ArrayType (convert-range (cadr type))
                                       (convert-type (caddr type))))
      ((channeltype) (CspAst.ChannelType (cadr type) (convert-dir (caddr type))))
      ((integer)     (let ((isConst (cadr type))
                           (isSigned (caddr type))
                           (dw       (cadddr type))
                           (interval (caddddr type)))
                       
                       (CspAst.IntegerType
                        isConst
                        isSigned
                        (not (null? dw))
                        (if (null? dw) 0 (BigInt.ToInteger dw))
                        (not (null? interval))
                        (if (null? interval)
                            '(() ())
                            (list (car interval) (cadr interval))))))
      
      
      ((node-array)  (CspAst.NodeType #t (caddr type) (convert-dir (cadr type))))
      ((node)        (CspAst.NodeType #f 1 (convert-dir (cadr type))))
      ((structure)   (CspAst.StructureType (cadr type)
                                           (symbol->string (caddr type))))
      (else (error "convert-type : unknown type (pair) : " type))
      )
    )
    
   (else (error "convert-type : unknown type : " type))
   )
  )
    

(define *last-decl* #f)

(define (convert-declarator decl)
  (set! *last-decl* decl)
  (dis "convert-declarator : " decl dnl)
  (let ((ident (cadr decl))
        (type  (caddr decl))
        (dir   (cadddr decl))
        (expr  (caddddr decl)))
    (CspAst.Declarator
     (if (or (not (pair? ident))
             (not (eq? (car ident) 'id)))
         (error "convert-declarator : unexpected identifier in declarator : " ident)
         (cadr ident))
     (convert-type type)
     (if (null? expr) '() (convert-expr expr))
     (convert-dir  dir))
    )
  )


(define (convert-var-stmt s)
  (set! *last-var* s)

  (let ((decls (cadr s))
        (decl  (caadr s))
        (stmt  (caddr s)))

    (if (not (= 1 (length decls)))
        (error "convert-var-stmt : need to convert single var decl : " s))
    
    (CspAst.VarStmt (convert-declarator decl) stmt)
    )
  )

(define (convert-var1-stmt s)
  (let ((decl  (cadr s))
        (stmt  (caddr s)))
    (CspAst.VarStmt (convert-declarator decl) stmt)
    )
  )

(define xxx #f)

(define (flatten-var-stmt s)
  ;; this desugars the var stmts to var1 stmts
  ;; (one decl per statement)
  (let ((the-sequence 
         (cons 'sequence
               (map
                (lambda(d)(list 'var (list d) (caddr s)))
                (cadr s)))))
    the-sequence
    )
  )

(define (convert-stmt s last)
  (set! *s* s)
  (set! *last* last)
  (dis "CONVERT : " s dnl)

  (if (eq? s 'skip)

      ;; special case for skip, only token allowed on its own
      (CspAst.SkipStmt)

      ;; not a skip -- handle it properly
      (begin
        (if (not (pair? s))
            (begin
              (set! *bad-s* s)
              (set! *bad-last* last)
              (error "Not a statement : " s dnl "last : " last)))
        
        (let ((kw (car s))
              (args (cdr s))
              )
;;          (dis "kw is " kw dnl)
          
          (case kw
            
            ((sequence parallel)
             (let ((seq (init-seq 'CspStatementSeq.T)))
               (map (lambda(ss)
                      (seq 'addhi (convert-stmt ss s)))
                    args
                    )
               ((if (eq? kw 'sequence)
                    CspAst.SequentialStmt
                    CspAst.ParallelStmt)
                (seq '*m3*))
               
               ))

            ((assign)
             (CspAst.AssignmentStmt (convert-expr (car args))
                                    (convert-expr (cadr args))
                                )
             )

            ;; the next three are just syntactic sugar 
            ((assign-operate)
             (let ((transformed (list 'assign
                                   (cadr args)
                                   (list
                                    (car args)
                                    (cadr args)
                                    (caddr args)))))
               (dis "transform " s " -> " transformed dnl)
               (convert-stmt transformed s)))

            ((increment)
             (convert-stmt (list 'assign-operate '+ (car args) (BigInt.New 1))))

            ((decrement)
             (convert-stmt (list 'assign-operate '- (car args) (BigInt.New 1))))

            ((var) (if (= 1 (length (cadr s)))
                       
                       (convert-var-stmt s)

                       (convert-stmt (flatten-var-stmt s) s))
             )

            ((var1) (convert-var1-stmt s))

            ((recv) (CspAst.RecvStmt (convert-expr (car args))
                                     (convert-expr (cadr args))))

            ((send) (CspAst.SendStmt (convert-expr (car args))
                                     (convert-expr (cadr args))))

            ((do if nondet-if nondet-do)
             (let ((seq (init-seq 'CspGuardedCommandSeq.T)))
               (map (lambda (gc)
                      (seq 'addhi
                           (CspAst.GuardedCommand
                            (convert-expr (car gc))
                            (convert-stmt (cadr gc) s))))
                    args)

;;               (dis "here! " kw dnl)

               (let ((maker
                      (case kw
                        ((do) CspAst.DetRepetitionStmt)
                        ((if) CspAst.DetSelectionStmt)
                        ((nondet-do) CspAst.NondetRepetitionStmt)
                        ((nondet-if) CspAst.NondetSelectionStmt)
                        )))
;;                 (dis "here2 " maker dnl)
                 (maker (seq '*m3*)))
               ))

            ((eval) (CspAst.ExpressionStmt (convert-expr (car args))))
            
            (else (set! *bad-s* s)
                  (set! *bad-last* last)
                  (error "convert-stmt : unknown statement " s))
            )
          )
        )
      ))

(define *last-x* #f)

(define *all-x* '())

(define *rest* #f)

(define *last-a* #f)

(define (convert-expr x)
  (set! *last-x* x)
  (set! *all-x* (cons x *all-x*))

  (dis "EXPR : " x dnl)

  (cond ((null? x) (error "convert-expr : x is null"))

        ((BigInt.IsT x)  (CspAst.IntegerExpr x))

        ((string? x) (CspAst.StringExpr x))

        ((eq? x 'else) (CspAst.BooleanExpr #t)) ;; bit of a hack

        ((eq? x #t) (CspAst.BooleanExpr #t))

        ((eq? x #f) (CspAst.BooleanExpr #f))

        ((pair? x)

         ;; a pair...
         (case (car x)
           ((probe) (CspAst.ProbeExpr (convert-expr (cadr x))))

           ((array-access) (CspAst.ArrayAccessExpr (convert-expr (cadr x))
                                                   (convert-expr (caddr x))))

           ((id) (CspAst.IdentifierExpr (cadr x)))

           ((member-access) (CspAst.MemberAccessExpr (convert-expr (cadr x))
                                                     (caddr x)))
           ((structure-access) (CspAst.StructureAccessExpr (convert-expr (cadr x))
                                                     (caddr x)))

           ((apply)
            (let ((fx     (convert-expr (cadr x)))
                  (rest   (cddr x))
                  (argseq (init-seq 'CspExpressionSeq.T)))

              (set! *rest* rest)
              (map
               (lambda(a)
                 (set! *last-a* a)
                 (argseq 'addhi (convert-expr a)))
               rest)
              (CspAst.FunctionCallExpr fx (argseq '*m3*))
              )
            )

           ((not) (CspAst.UnaExpr 'Not (convert-expr (cadr x))))

           ((-)
            ;; - is special

            (if (null? (cddr x))
                (CspAst.UnaExpr 'Neg (convert-expr (cadr x)))
                (CspAst.BinExpr 'Sub (convert-expr (cadr x))
                                     (convert-expr (caddr x)))))

           ((+ / % * == != < > >= <= & && | || ^ == << >> **)
            (CspAst.BinExpr
             (case (car x)
               ((+) 'Add)
               ((/) 'Div)
               ((%) 'Rem)
               ((*) 'Mul)
               ((==) 'EQ)
               ((!=) 'NE)
               ((<) 'LT)
               ((>) 'GT)
               ((>=) 'GE)
               ((<=) 'LE)
               ((&) 'And)
               ((&&) 'CondAnd)
               ((|) 'Or)
               ((||) 'CondOr)
               ((^) 'Xor)
               ((<<) 'SHL)
               ((>>) 'SHR)
               ((**) 'Pow)
               (else (error " BinExpr " (car x))))
             (convert-expr (cadr x))
             (convert-expr (caddr x))))
           
           (else (error "convert-expr : unknown keyword " (car x) " : " x ))
           )
         )
        
        (else (error "dunno that type " x) )
        )
  )

(define (convert-prog p)
  (if (not (and (pair? p) (eq? (car p) 'csp)))
      (error (string-append "Not a CSP program : " p)))
  (convert-stmt (nth p 6) '())
  )

(define (reload) (load "cspc.scm"))

(loaddata! "arrays_p1")


(define a (BigInt.New 12))

(define csp (obj-method-wrap (convert-prog data) 'CspSyntax.T))

(set-rt-error-mapping! #f)

(define lisp0 (nth data 6))
(define lisp1 (csp 'lisp))
(define lisp2 ((obj-method-wrap (convert-stmt lisp1 '()) 'CspSyntax.T) 'lisp))
(define lisp3 ((obj-method-wrap (convert-stmt lisp2 '()) 'CspSyntax.T) 'lisp))
(define lisp4 ((obj-method-wrap (convert-stmt lisp3 '()) 'CspSyntax.T) 'lisp))

(if (not (equal? lisp2 lisp3)) (error "lisp2 and lisp3 differ!"))


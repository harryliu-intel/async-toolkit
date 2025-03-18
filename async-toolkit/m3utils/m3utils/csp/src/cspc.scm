                                        ; cspc.scm ;
                                        ;

(require-modules "basic-defs" "m3" "hashtable" "display")

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
    (set! refparents (nth data 3))
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
            
            ((var) (dis "HI" dnl) (set! *var-s* s) '())

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
                  (error "Unknown statement " s))
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

           ((+ / % * == != < > >= <= & && | || ^ == << >>)
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
               (else (error " BinExpr " (car x))))
             (convert-expr (cadr x))
             (convert-expr (caddr x))))
           
           (else (error "unknown keyword " (car x) " : " x ))
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

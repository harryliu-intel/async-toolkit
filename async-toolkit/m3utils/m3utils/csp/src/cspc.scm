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
      (CspAst.SkipStmt)
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

(define (convert-expr x)
  (set! *last-x* x)
  (set! *all-x* (cons x *all-x*))

  
  '()
  )

(define (convert-prog p)
  (if (not (and (pair? p) (eq? (car p) 'csp)))
      (error (string-append "Not a CSP program : " p)))
  (convert-stmt (nth p 6) '())
  )

(define (reload) (load "cspc.scm"))

(loaddata! "arrays_p1")

(define a (BigInt.New 12))

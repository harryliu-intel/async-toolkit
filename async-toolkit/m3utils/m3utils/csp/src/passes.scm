(define (find-pass proc)

  (define (recurse p)
    (cond ((null? p) #f)
          ((equal? proc (cadar p)) (car p))
          (else (recurse (cdr p)))))

  (recurse (append 
            *the-passes-2*
            *the-passes-3*
            *the-passes-4*
            *the-passes-5*
;;            *the-passes-6*
            ))
  )

(define (manual-pass proc prog)

  ;; use this to run a single pass on the program
  ;; 
  ;; e.g., (manual-pass inline-evals <program>)
  
  (let* ((pass (find-pass proc))
         (res (run-pass
               pass prog *cellinfo* *the-inits* *the-func-tbl* *the-struct-tbl*)))

    (dis (if (equal? prog res)
             "============================  no change"
             *program-changed*
             )
         dnl)
    
    res
    )
  )

(define (make-simple-pass stmt-xform)
  (lambda(the-inits stmt func-tbl struct-tbl cell-info)
    (stmt-xform stmt)))

(define (define-simple-pass sym)
  (define-global-symbol
    (symbol-append sym '-pass) (make-simple-pass (eval sym))))

(define-simple-pass 'sequentialize-nonblocking-parallels)
(define-simple-pass 'constantify-constant-vars)
(define-simple-pass 'simplify-stmt)
(define-simple-pass 'unfold-loop-ranges)
(define-simple-pass 'convert-waiting-ifs)

(define (clear-texts!)
  (set! text1 #f)
  (set! text2 #f)
  (set! text3 #f)
  (set! text4 #f)
  (set! text5 #f)
  (set! text6 #f)
  (set! text7 #f)
  (set! text8 #f)
  (set! text9 #f)
  'ok
  )

(define (compile1!)
  ;; unquify the loops before doing ANYTHING else
  ;; and add the struct decls
  (set! text1
        
           (uniquify-loop-dummies *the-text*))
  'text1
  )


;;
;; there are two different types of compiler passes:
;; there are the statement-oriented passes, these are marked by statement
;; type.  * matches all statement types.  These are all called from the
;; syntax walker, which maintains an environment structure.
;;
;; the global passes have a different interface and are not called from
;; the syntax walker.
;;
(define *the-passes-2* (list
                        (list 'assign handle-access-assign)
                        (list 'recv   handle-access-recv)
                        (list 'send   handle-access-recv)
                        (list 'assign handle-assign-rhs)
                        (list 'send   handle-send-rhs)
                        (list 'eval   handle-eval)
                        (list 'global inline-evals)
                        (list 'global handle-assign-shortcircuit)
                        (list 'global global-simplify)
                        (list 'global remove-assign-operate)
                        (list 'global remove-do)
                        (list 'global remove-choose)
                        (list 'global simplify-if)
                        (list 'global convert-waiting-ifs-pass)
                        (list 'global unfold-loop-ranges-pass)
                        (list 'assign remove-loop-expression)))

(define (compile2!)
  (set! *stage* 'compile2!)
  (set! text2
        (uniquify-loop-dummies
         (run-compiler *the-passes-2*
                       text1
                       *cellinfo*
                       *the-inits*
                       *the-func-tbl*
                       *the-struct-tbl*)))
  'text2
  )

(define *the-passes-3*
  `(
    (send    ,convert-send-struct)
    (recv    ,convert-recv-struct)
    (*       ,fold-constants-*)
    )
  )

(define (compile3!)
  (set! *stage* 'compile3!)
  (set! *the-globals* (construct-globals-tbl *the-inits*))
  (set! text3
        (uniquify-loop-dummies
         (run-compiler  
          *the-passes-3*
          text2
          *cellinfo*
          *the-inits*
          *the-func-tbl*
          *the-struct-tbl*))
        )
  'text3
  )


(define *the-passes-4*
  ;; we need to insert assignments after initialization for
  ;; all var1s that aren't immediately followed by an initialization,
  ;; before we run constantification
  `((*       ,fold-constants-*)
    (global  ,constantify-constant-vars-pass)
    )
  )

(define (compile4!)
  (set! *stage* 'compile4!)

  (set! text4
        (uniquify-loop-dummies
         (run-compiler  
          *the-passes-4*

          ;; we patch the uninitialized uses:
          (patch-uninitialized-uses text3)
          
          *cellinfo*
          *the-inits*
          *the-func-tbl*
          *the-struct-tbl*))
        )
  'text4
  )

(define *the-passes-5*
  `((global          ,sequentialize-nonblocking-parallels-pass)
    (global          ,delete-unused-vars-pass)
    (global          ,simplify-stmt-pass)
    (parallel-loop   ,unroll-parallel-loops)
    (sequential-loop ,unblock-loops)
;;    (*       ,fold-constants-*)
;;    (global  ,constantify-constant-vars-pass)
    )
  )

  
(define (compile5!)
  (set! *stage* 'compile5!)
    (set! text5
          (uniquify-loop-dummies
           (run-compiler
            *the-passes-5*
            text4
            *cellinfo*
            *the-inits*
            *the-func-tbl*
            *the-struct-tbl*)))
    ;;  (display-success-2)

    (set! *the-struct-decls* (find-structdecls (list text5)))
    'text5
  )

(define fixpoint-simplify-stmt (make-fixpoint-func simplify-stmt))

(define (compile6!)
  (set! *stage* 'compile6!)
  (set! text5.1
        (simplify-stmt
         `(sequence
            ,@(get-global-stmts text5 *the-initvars*)
            ,text5
            )
         )
        )
  (set! text5.2
        (simplify-stmt (insert-pack-copies (insert-unpack-copies text5.1)))
        )
  (make-the-tables text5.2)
  (seed-integer-ranges!)
  (close-integer-ranges!)
  (propose-types!)
  (set! text6 text5.2)
  'text6
  )

(define (compile7!)
  (set! *stage* 'compile7!)
  (set! text7
        (fixpoint-simplify-stmt (insert-block-labels text6))
        (dis "=========  PROGRAM LABELS INSERTED" dnl)
        )
  'text7
  )

(define (compile8!)
  (set! *stage* 'compile8!)
  (set! text8
        (map fixpoint-simplify-stmt  (scan-stmt text7) ) )
  'text8
  )


(define (compile9!)
  (set! *stage* 'compile9!)
  (set! text9
        (remove-empty-blocks text8) )
        (dis "=========  PROGRAM BLOCKS GENERATED" dnl)
        (dis "---  BEGIN PROGRAM LISTING  ---" dnl)
        (map pp text9)
        (dis "---   END PROGRAM LISTING   ---" dnl)
  'text9
  )

(define (compile!)
  (compile1!)
  (compile2!)
  (compile3!)
  (compile4!)
  (compile5!)
  (compile6!)
  (compile7!)
  (compile8!)
  (compile9!)
  )

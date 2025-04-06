(define (symtabvisit-program the-inits the-text visitor)
  ;; the visitor should take stmt and syms
  (define syms '())

  (define (enter-frame!)
    (set! syms (cons (make-hash-table 100 atom-hash) syms))
;;    (dis "enter-frame! " (length syms) dnl)
    )
  
  (define (exit-frame!)
;;    (dis "exit-frame! " (length syms) " " (map (lambda(tbl)(tbl 'keys)) syms) dnl)
    (set! syms (cdr syms))
    )

  (define (stmt-pre0-v s)
;;    (dis "pre stmt  : " (stringify s) dnl)
    (if (member (get-stmt-kw s) frame-kws) (enter-frame!))

    (case (get-stmt-kw s)
      ((var1)
       (define-var! syms (get-var1-id s) (get-var1-type s))
       )

      ((parallel-loop sequential-loop)
       (define-var! syms (get-loop-dummy s) *default-int-type*))
         
      )
    (visitor s syms)
    )

  (define (stmt-post s)
;;    (dis "post stmt : " (get-stmt-kw s) dnl)
    (if (member (get-stmt-kw s) frame-kws) (exit-frame!))
    s
    )

  (enter-frame!)
  (prepostvisit-stmt the-inits
                     stmt-pre0-v stmt-post
                     identity identity
                     identity identity)

  (let ((res   (prepostvisit-stmt the-text
                                  stmt-pre0-v stmt-post
                                  identity identity
                                  identity identity)))
    (exit-frame!)
    res
    )
  )

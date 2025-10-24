(define (dead-code)

  
;;  (error "here")

  (define (enter-frame!)
    (set! syms (cons (make-hash-table 100 atom-hash) syms))
;;    (dis "enter-frame! " (length syms) dnl)
    )

  (define (exit-frame!)
    ;;    (dis "exit-frame! " (length syms) " " (map (lambda(tbl)(tbl 'keys)) syms) dnl)

;;    (dis "exit-frame! " (length syms) dnl)
    (set! syms (cdr syms))
    )

  (define (stmt-check-enter s)
    (if (member (get-stmt-kw s) frame-kws) (enter-frame!))
    (case (get-stmt-kw s)
      ((var1)
       (define-var! syms (get-var1-id s) (get-var1-type s))
       )

      ((loop-expression)
       (define-var! syms (get-loopex-dummy s) *default-int-type*)
       )

      ((parallel-loop sequential-loop)
       (define-var! syms (get-loop-dummy s) *default-int-type*))
         
      )
    )
  
  (define (stmt-pre0 s)
;;    (dis "pre stmt  : " (stringify s) dnl)
    (stmt-check-enter s)

    (case (get-stmt-kw s)
      ((assign) (handle-assign-rhs s syms tg func-tbl struct-tbl))

      ((eval) ;; this is a function call, we make it a fake assignment.
       (dis "eval!" dnl)
       
       (dis "===== stmt-pre0 start " (stringify s) dnl)
       
       (let* ((fake-var (tg 'next))
              (fake-assign (make-assign `(id ,fake-var) (cadr s)))
              (full-seq   (handle-assign-rhs fake-assign syms tg func-tbl struct-tbl))
              (res (remove-fake-assignment fake-var full-seq)))

         (dis "===== stmt-pre0 fake  " (stringify fake-assign) dnl)
         (dis "===== stmt-pre0 full  " (stringify full-seq) dnl)
         (dis "===== stmt-pre0 done  " (stringify res) dnl)
         res
       ))
         

      (else s)
      )
    )

  (define (stmt-post s)
;;    (dis "post stmt : " (get-stmt-kw s) dnl)
    (if (member (get-stmt-kw s) frame-kws) (exit-frame!))
    s
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; EXAMPLE OF CREATING SYMBOL TABLE during visitation
  ;;
  
  (dis dnl "creating global frame... " dnl)
  
  (enter-frame!) ;; global frame

  ;; first visit the inits
  (dis dnl "visit inits ... " dnl dnl)

  (set! inits1
        (prepostvisit-stmt
         the-inits
;;         (filter-unused the-inits *unused-globals*)
         stmt-pre0 stmt-post
         identity identity
         identity identity))

  ;; then visit the program itself
  (dis dnl "visit program text ... " dnl dnl)

;;  (error "here")
  
  (set! text1
    (prepostvisit-stmt (simplify-stmt lisp)
                       stmt-pre0 stmt-post
                       identity identity
                       identity identity))

;;  (error)
  (exit-frame!) ;; and leave the global frame

  )

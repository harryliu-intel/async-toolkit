


(define (stmt-may-block? stmt)

  (define result #f)

  (define (yes) (set! result #t) 'cut)
  
  (define (s-visitor s)
    ;; statements that on their own introduce blocking
;;7    (dis "s-visitor " (stringify s) dnl)
    (if result
        'cut
        (case (get-stmt-type s)
          ((recv send)               (yes))
          ((if nondet-if waiting-if) (yes))
          (else                          s)
          )
        )
    )

  (define (x-visitor x)
    ;; expressions that introduce blocking
;;    (dis "x-visitor " (stringify x) dnl)
    (if result
        'cut
        (case (get-expr-type x)
          ((apply)                    (yes)) ;; shouldnt happen in compiled

          ((call-intrinsic)
           (if (eq? (cadr x) 'wait)   (yes) x))

          ((recv-expression peek)     (yes))

          (else x))))

;;  (dis "smb? : " (stringify stmt) dnl)

  (visit-stmt stmt s-visitor x-visitor identity)

  result
  )

(define (sequentialize-nonblocking-parallels stmt)

  (define (s-visitor s)
    (if (not (stmt-may-block? s))
        (case (get-stmt-type s)
          ((parallel)
           (dis "sequentialize-nonblocking-parallels : sequentializing : "
                s dnl)
           (cons 'sequence (cdr s)) ;; convert parallel to sequence
           )
          
          ((parallel-loop)
           (dis "sequentialize-nonblocking-parallels : sequentializing : "
                s dnl)
           (cons 'sequential-loop (cdr s)) ;; convert parallel to sequence
           )

          (else s))
        s);;fi
    )
  
  (visit-stmt stmt s-visitor identity identity)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (insert-block-labels prog)
  (define tg (make-name-generator "L"))
  
  (define (s-visitor s)

    (define (mark side)
      (let* ((suffix (case side
                       ((both after)
                        `((label ,(tg 'next))))
                       ((before -before)
                        '())))

             (prefix (case side
                       ((-before) ;; - labels are optional
                        `((label ,(symbol-append '- (tg 'next)))))
                       ((before) ;; - labels are optional
                        `((label ,(tg 'next))))
                       ((both)
                        `((label ,(tg 'next))))
                       ((after)
                        '())))
             
             (res (append '(sequence) prefix (list s) suffix))
             )
        res
        )
      )

    
    (case (get-stmt-type s)
      ((recv send) (mark 'before))

      ((waiting-if) (mark 'before))

      ((if nondet-if do nondet-do) (error))

      ((parallel parallel-loop) (mark 'both))

      ((while)
       (if (stmt-may-block? s) (mark '-before) s))

      ((eval)
       (if (stmt-may-block? s) (mark 'before) s))
      
      ((assign)
       (let ((rhs (get-assign-rhs s)))
         (if (or (peek? rhs) (recv-expression? rhs))
             (mark 'before)
             s))
       )

      (else s)))


  ;; label the entry point also
  `(sequence (label START) ,(visit-stmt prog s-visitor identity identity))
  )

(define (unblock-loops stmt syms vals tg func-tbl struct-tbl cell-info)

  ;; sequential-loop with a blocking statement have to be desugared
  ;; to regular while loops
  
  (define tg (make-name-generator "unblock-loops"))

    (define (s-visitor s)
      (dis "s-visitor s : " s dnl)
      (if (and (eq? 'sequential-loop (get-stmt-type s))
               (stmt-may-block? s))
          
          (let* ((loopr    (get-loop-range s))
                 (lmin     (cadr loopr))
                 (lmax     (caddr loopr))
                 
                 (nam      (get-loop-dummy s))
                 ;; reuse the dummy
                 
                 (newtype  (derive-type lmax syms func-tbl struct-tbl cell-info))
                 ;; we can only count upward... so lmax is a safe type
                 
                 (newddecl (make-var1-decl nam newtype))
                 
                 (newdinit  (make-assign `(id ,nam) lmin))
                 
                 (incstmt   (make-assign `(id ,nam) `(+ (id, nam) ,*big1*)))
                 
                 (newbnam  (tg 'next))
                 
                 (newbdecl (make-var1-decl newbnam *default-boolean-type*))
                 
                 (bupdate  (make-assign `(id ,newbnam) `(<= (id ,newbnam ,lmax))))
                 
                 (the-loop `(while (id ,newbnam) (sequence ,(get-loop-stmt s)
                                                           ,incstmt
                                                           ,bupdate)))
                 )
            
            (define-var! syms nam newtype)
            (define-var! syms newbnam *default-boolean-type*)
            
            `(sequence ,newddecl ,newdinit ,newbdecl ,bupdate , the-loop))
          s
          )
      )

    (visit-stmt stmt s-visitor identity identity)
    )

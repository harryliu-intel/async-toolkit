


(define (stmt-may-block? stmt)

  (define result #f)

  (define (yes) (set! result #t) 'cut)
  
  (define (s-visitor s)
    ;; statements that on their own introduce blocking
;;    (dis "s-visitor " (stringify s) dnl)
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

(define (insert-block-labels stmt)
  (define tg (make-name-generator ""))
  
  (define (s-visitor s)

    (define (mark side)
      (let* ((tail (case side
                     ((both) `((label ,(tg 'next))))
                     ((before) '())))
             (res (append (list 'sequence
                                `(label ,(tg 'next))
                                s)
                          tail)))

        res
        )
      )

    
    (case (get-stmt-type s)
      ((recv send) (mark 'before))

      ((waiting-if) (mark 'before))

      ((if nondet-if) (error))

      ((parallel parallel-loop) (mark 'both))

      ((assign)
       (let ((rhs (get-assign-rhs s)))
         (if (or (peek? rhs) (recv-expression? rhs))
             (mark 'before)
             s))
       )

      (else s)))


  (visit-stmt stmt s-visitor identity identity)
  )

  

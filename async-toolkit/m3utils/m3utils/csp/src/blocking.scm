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
          ((apply)                    (yes))

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
    (if (and (eq? (get-stmt-type s) 'parallel)
             (not (stmt-may-block? s)))
        (begin
          (dis "sequentialize-nonblocking-parallels : sequentializing : " s dnl)
          (cons 'sequence (cdr s)) ;; convert parallel to sequence
          )
        s)
    )
  
  (visit-stmt stmt s-visitor identity identity)
  )

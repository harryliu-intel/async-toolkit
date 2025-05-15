(define (convert-send-struct s syms vals tg func-tbl struct-tbl cell-info)
  
  (let ((lhs (get-send-lhs s))
        (rhs (get-send-rhs s)))
    (if (simple-operand? rhs)
        (let* (
               (oldtype (derive-type rhs syms func-tbl struct-tbl cell-info)) 
               )
          (if (struct-type? oldtype)
              (let*
                  ((tempnam (tg 'next))
                   (newvar  (make-var1-decl tempnam *default-int-type*))
                   (newass  `(eval (call-intrinsic pack (id ,tempnam) ,rhs)))
                   (newsend (make-send lhs `(id ,tempnam)))
                   )
                   
                (define-var! syms tempnam *default-int-type*)
                (list 'sequence newvar newass newsend)
                )
              s
              )
          )
        s
        )
    )
  )
  
(define (convert-recv-struct s syms vals tg func-tbl struct-tbl cell-info)
  
  (let ((lhs (get-recv-lhs s))
        (rhs (get-recv-rhs s)))
    (if (simple-operand? rhs)
        (let* (
               (oldtype (derive-type rhs syms func-tbl struct-tbl cell-info)) 
               )
          (if (struct-type? oldtype)
              (let*
                  ((tempnam (tg 'next))
                   (newvar  (make-var1-decl tempnam *default-int-type*))
                   (newass  `(eval (call-intrinsic unpack ,rhs (id ,tempnam))))
                   (newrecv `(recv ,lhs (id ,tempnam)))
                   )
                   
                (define-var! syms tempnam *default-int-type*)
                (list 'sequence newvar newrecv newass)
                )
              s
              )
          )
        s
        )
    )
  )
  

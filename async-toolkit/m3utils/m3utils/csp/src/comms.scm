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
                   (newass  `(assign (id ,tempnam)
                                     (call-intrinsic pack ,rhs)))
                   (newsend (make-send lhs `(id ,tempnam)))
                   (res     (list 'sequence newvar newass newsend))
                   )
                   
                (define-var! syms tempnam *default-int-type*)
                (dis "convert-send-struct : " s " -> " res dnl)
                res
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
                   (newrecv `(recv ,lhs (id ,tempnam)))
                   (newass  `(eval (call-intrinsic unpack ,rhs (id ,tempnam))))
                   (res     (list 'sequence newvar newrecv newass))
                   )
                   
                (define-var! syms tempnam *default-int-type*)
                (dis "convert-recv-struct : " s " -> " res dnl)
                res
                )
              s
              )
          )
        s
        )
    )
  )
  

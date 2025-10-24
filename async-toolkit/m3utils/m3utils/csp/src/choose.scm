; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (remove-choose the-inits prog func-tbl struct-tbl cell-info)

  (define tg (make-name-generator "remove-choose"))
  
  (define (visitor s)
    (let ((kw (get-stmt-type s)))
      (cond ((and (eq? kw 'assign)
                  (call-intrinsic? (get-assign-rhs s))
                  (eq? 'choose (cadr (get-assign-rhs s))))
             (let* ((lhs  (get-assign-lhs s))
                    (rhs  (get-assign-rhs s))
                    (sel  (caddr rhs))
                    (tval (cadddr rhs))
                    (fval (caddddr rhs))

                    (tid (tg 'next))
                    (fid (tg 'next))
                    (t-ass `(assign (id ,tid) ,tval))
                    (f-ass `(assign (id ,fid) ,fval))
                    
                    (stmt `(sequence
                             ,t-ass
                             ,f-ass
                             (if (,sel (assign ,lhs (id ,tid)))
                                 (else (assign ,lhs (id ,fid))))))
                                 
                    )
               stmt))

            ((and (eq? kw 'eval)
                  (call-intrinsic? (cadr s))
                  (eq? 'choose (cadr (cadr s))))
             `(assign (id ,(tg 'next 'dummy-)) ,(cadr s))
             )
            
            (else s)
            )
      )
    )
  
  (visit-stmt prog visitor identity identity)
  )


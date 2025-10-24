; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (handle-assign-dbg . x)
;;  (apply dis x)
  )

(define (peek?            x)  (and (pair? x) (eq? 'peek (car x))))
(define (probe?           x)  (and (pair? x) (eq? 'probe (car x))))
(define (recv-expression? x)  (and (pair? x) (eq? 'recv-expression (car x))))

(define get-probe-port cadr)

(define (handle-access-assign ass syms vals tg func-tbl struct-tbl cell-info)

  (set! *has-ass* ass)
  
  (if (not (pair? ass))
      (error "handle-access-assign can't handle " ass))

  (let ((kw (car ass)))

    (if (not (eq? kw 'assign))
        (error "not an assignment : " ass))

    (define seq '())
    
    (define (make-simple x)

      (handle-assign-dbg "make-simple " x dnl)
      
      (set! sss (cons (cons x syms) sss))
      
      (if (simple-operand? x)
          x
          (let* ((nam     (tg 'next))
                 (newtype (derive-type x syms func-tbl struct-tbl cell-info))
                 (newvar  (make-var1-decl nam newtype))
                 (newass  `(,kw ,(make-ident nam) ,x))
                 )
            (define-var! syms nam newtype)
            (dis "make simple adding " newvar dnl)
            (dis "make simple adding " newass dnl)
            (set! seq  (cons newvar (cons newass seq)))
            `(id ,nam)
            )))
    
    (define (handle-access-expr a)
      
      ;;    (handle-assign-dbg "handle-access-expr " a dnl)
      
      (cond ((simple-operand? a) a)

            ((eq? 'member-access (car a))
             (list 'member-access (handle-access-expr (cadr a)) (caddr a)))

            ((eq? 'bits (car a))
             (list 'bits
                   (handle-access-expr (cadr a))
                   (make-simple (caddr a))
                   (make-simple (cadddr a))))
            
            ((eq? 'array-access (car a))
             
             (list 'array-access
                   (handle-access-expr (cadr a))
                   (make-simple (caddr a))))
            
            (else a)
            ))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (handle-assign-dbg   "handle-access-assign : called    : "  ass dnl)
    
    (let* ((lhs       (handle-access-expr (get-assign-lhs ass)))
           (rhs       (handle-access-expr (get-assign-rhs ass)))
           (this-ass `(,kw ,lhs ,rhs))
           (res       (if (null? seq)
                          this-ass
                          `(sequence ,@seq ,this-ass))))

      (if (not (null? seq)) ;; print if changing
          (dis "handle-access-assign : returning : " res dnl))
      res
      )
    )
  )
 

(define *har-ass* #f)

(define (handle-send-rhs s syms vals tg func-tbl struct-tbl cell-info)
  (let ((lhs (get-send-lhs s))
        (rhs (get-send-rhs s)))
    (handle-assign-dbg "handle-send-rhs :       " s dnl)
    (handle-assign-dbg "handle-send-rhs : lhs : " lhs dnl)
    (handle-assign-dbg "handle-send-rhs : rhs : " rhs dnl)
    (if (simple-operand? rhs)
        s

        (let* ((tempnam (tg 'next))
               (newtype (derive-type rhs syms func-tbl struct-tbl cell-info)) 
               (newvar  (make-var1-decl tempnam newtype))
               (newass  (make-assign `(id ,tempnam) rhs))
               (newsend (make-send lhs `(id ,tempnam)))
               )
          (define-var! syms tempnam newtype)
          (list 'sequence newvar newass newsend)
          )
        )
    )
  )

(define (handle-assign-shortcircuit
         the-inits prog func-tbl struct-tbl cell-info)

  (define (visitor a)
    (if (assign? a)
    
        (let ((lhs (get-assign-lhs a))
              (rhs (get-assign-rhs a)))
          
          (define (do-it ze)
            (let* ((c0 (cadr rhs))
                   (c1 (caddr rhs))
                   (res 
                    `(if ((== ,c0 ,ze) (assign ,lhs ,ze))
                         (else         (if ((== ,c1 ,ze) (assign ,lhs ,ze))
                                           (else         (assign ,lhs ,(not ze))))))
                    )
                   )
              (dis "handle-assign-shortcircuit : res : " (stringify res) dnl)
              res
              )
            )
          
          (cond ((and (= 3 (length rhs)) (eq? '|| (car rhs)))
                 (do-it #t))
                ((and (= 3 (length rhs)) (eq? '&& (car rhs)))
                 (do-it #f))
                (else a))
          );;tel
        
        a ;; not an assignment
        );;fi
    );;enifed

  (visit-stmt prog visitor identity identity)
  
  )


(define (handle-assign-rhs a syms vals tg func-tbl struct-tbl cell-info)

  (set! *har-ass* a)
  
  (define (recurse a) (handle-assign-rhs a syms vals tg func-tbl struct-tbl cell-info))
  
  (handle-assign-dbg "handle-assign-rhs   : " a dnl)

  (set! *a* (cons a *a*))
  (set! *syms* (cons syms *syms*))

  (let ((lhs (get-assign-lhs a))
        (rhs (get-assign-rhs a)))

    (set! *rhs* rhs)
    (set! *lhs* lhs)

    (handle-assign-dbg "handle-assign-rhs   lhs : " lhs dnl)
    (handle-assign-dbg "handle-assign-rhs   rhs : " rhs dnl)

    (cond

     ((or (apply? rhs)
          (call-intrinsic? rhs))
      (handle-assign-dbg "handle-assign-rhs : function application : " (stringify rhs) dnl)
      
      (let* ((call-type (car rhs))
             (fnam      (cadr rhs)))
        (let loop ((p   (cddr rhs))
                   (seq '())
                   (q   '()))

          (handle-assign-dbg "p = " p dnl)
          
          (cond ((null? p)
                 ;; done iterating
                 
                 (handle-assign-dbg "handle-assign-rhs base case seq : " (stringify seq) dnl)
                 (handle-assign-dbg "handle-assign-rhs base case q   : " (stringify q) dnl)
                 
                 (if (null? seq)
                     a
                     `(sequence
                        ,@seq
                        (assign ,lhs
                                (,call-type
                                 ,fnam
                                 ,@(map
                                    (lambda(x)
                                      ;; this is tricky:
                                      ;; list holds identifiers
                                      ;; and literals
                                      (if (symbol? x)  
                                          (list 'id x) ;; id
                                          x            ;; literal
                                          ))
                                    (reverse q)))))))
                
                
                ((simple-operand? (car p))
                 (loop (cdr p) seq (cons (car p) q)))

                (else
                 (let* ((tempnam (tg 'next))
                        (newtype (derive-type (car p) syms func-tbl struct-tbl cell-info)) 
                        (newvar (make-var1-decl tempnam newtype))
                        (newass (make-assign `(id ,tempnam) (car p)))
                        )
                   (define-var! syms tempnam newtype)
                   (loop (cdr p)
                         (cons newvar (cons newass seq))
                         (cons tempnam q))))))))
        
            
     
     ((binary-expr? rhs)

      (handle-assign-dbg "binary-expr : rhs : " rhs dnl)
      
      (let* ((op (car rhs))
             (l  (cadr rhs))
             (r  (caddr rhs))
             (complex-l (not (simple-operand? l)))
             (complex-r (not (simple-operand? r)))
             )
        
        (cond
         ((and complex-l complex-r)
          (let*
              ((ltempnam (tg 'next))
               (ltype    (derive-type l syms func-tbl struct-tbl cell-info))
               (rtempnam (tg 'next))
               (rtype    (derive-type r syms func-tbl struct-tbl cell-info))
               (seq
                `(sequence
                   ,(make-var1-decl ltempnam ltype)
                   ,(recurse `(assign (id ,ltempnam) ,l))
                   ,(make-var1-decl rtempnam rtype)
                   ,(recurse `(assign (id ,rtempnam) ,r))
                   ,(recurse `(assign ,lhs (,op (id ,ltempnam) (id ,rtempnam))))))
               (res (simplify-stmt seq)))
            res))
         
         
         (complex-l
          (let*
              ((tempnam (tg 'next))
               (type    (derive-type l syms func-tbl struct-tbl cell-info))
               (seq
                `(sequence
                   ,(make-var1-decl tempnam type)
                   ,(recurse `(assign (id ,tempnam) ,l))
                   ,(recurse `(assign ,lhs (,op (id ,tempnam) ,r)))))
               (res (simplify-stmt seq)))
            res))
         
         (complex-r
          (let*
              ((tempnam (tg 'next))
               (type    (derive-type r syms func-tbl struct-tbl cell-info))
               (seq
                `(sequence
                   ,(make-var1-decl tempnam type)
                   ,(recurse `(assign (id ,tempnam) ,r))
                   ,(recurse `(assign ,lhs (,op ,l (id ,tempnam))))))
               
               (res (simplify-stmt seq)))
            res))
         
         (else a))))
     
     ((unary-expr? rhs)
      
      (let ((op (car rhs))
            (x  (cadr rhs)))
        
        (cond ((not (simple-operand? x))
               (let*
                   ((tempnam (tg 'next))
                    (type    (derive-type x syms func-tbl struct-tbl cell-info))
                    (seq
                     `(sequence
                        ,(make-var1-decl tempnam type)
                        ,(recurse `(assign (id ,tempnam) ,x))
                        ,(recurse `(assign ,lhs (,op (id ,tempnam))))))
                    (res (simplify-stmt seq)))
                 res))
              
              (else a))))

     ((or (probe? rhs) (recv-expression? rhs) (peek? rhs))

      (define seq '())
      
      (define (make-simple x)
        
        (handle-assign-dbg "make-simple " x dnl)
        
        (set! sss (cons (cons x syms) sss))
        
        (if (simple-operand? x)
            x
            (let* ((nam     (tg 'next 'chanex-access-))
                   (newtype (derive-type x syms func-tbl struct-tbl cell-info))
                   (newvar  (make-var1-decl nam newtype))
                   (newass  `(assign ,(make-ident nam) ,x))
                   )
              (define-var! syms nam newtype)
              (dis "make simple adding " newvar dnl)
              (dis "make simple adding " newass dnl)
              (set! seq  (cons newvar (cons newass seq)))
              `(id ,nam)
              ))
        )

      (let* ((op (car rhs))
             (x  (cadr rhs))
             (result
              (cond ((eq? 'array-access (car x))
                     (let* (
                            (the-access (list 'array-access
                                              (cadr x)
                                              (make-simple (caddr x))))
                            (the-operation (list op the-access))
                            (the-ass (list 'assign lhs the-operation))
                           )

                       
                       (handle-assign-dbg "probe/recv-ex the-access    : " the-access dnl)
                       (handle-assign-dbg "probe/recv-ex the-operation : " the-operation dnl)
                       (handle-assign-dbg "probe/recv-ex the-ass       : " the-ass dnl)
                       (if (null? seq)
                           the-ass
                           (cons 'sequence (append seq (list the-ass))))))
                    
                    (else a))))
        (handle-assign-dbg "probe/recv-ex  : " op dnl)
        (handle-assign-dbg "probe/recv-ex op : " op dnl)
        
        (handle-assign-dbg "probe/recv-ex -> " (stringify result) dnl)
        result
        ))
              
     (else a))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; receives are similar to assignments -- sort of
;;

(define (get-recv-lhs rs)  (cadr rs))   ;; wait, isnt this backwards?

(define (get-recv-rhs rs)  (caddr rs))  ;; wait, isnt this backwards?

(define (handle-access-recv ass syms vals tg func-tbl struct-tbl cell-info)

  (set! *has-ass* ass)
  
  (if (not (pair? ass))
      (error "handle-access-recv can't handle " ass))

  (let ((kw (car ass)))

    (if (not (member kw '(recv send)))
        (error "not a recv : " ass))

    (define seq '())


    (define (make-simple x)

      ;;    (handle-assign-dbg "make-simple " x dnl)
      
      (set! sss (cons (cons x syms) sss))
      
      (if (simple-operand? x)
          x
          (let* ((nam     (tg 'next))
                 (newtype (derive-type x syms func-tbl struct-tbl cell-info))
                 (newvar  (make-var1-decl nam newtype))
                 (newass  `(assign ,(make-ident nam) ,x))
                 )
            (define-var! syms nam newtype)
            (dis "make simple adding " newvar dnl)
            (dis "make simple adding " newass dnl)
            (set! seq  (cons newvar (cons newass seq)))
            `(id ,nam)
            )))
    

    (define (handle-access-lhs a)
      
      ;;    (handle-assign-dbg "handle-access-lhs " a dnl)
      
      (cond ((eq? 'array-access (car a))
             (list 'array-access
                   (cadr a)
                   (make-simple (caddr a))))
            
            (else a)
            ))

    (define (handle-access-rhs a)
      
      ;;    (handle-assign-dbg "handle-access-expr " a dnl)
      
      (cond ((simple-operand? a) a)

            ((null? a) a)

            ((eq? 'member-access (car a))
             (list 'member-access (handle-access-rhs (cadr a)) (caddr a)))

            ((eq? 'bits (car a))
             (list 'bits
                   (handle-access-rhs (cadr a))
                   (make-simple (caddr a))
                   (make-simple (cadddr a))))
            
            ((eq? 'array-access (car a))
             
             (list 'array-access
                   (handle-access-rhs (cadr a))
                   (make-simple (caddr a))))
            
            (else a)
            ))
    
    ;;  (handle-assign-dbg   "handle-access-recv : called    : "  ass dnl)
    
    (let* ((lhs       (handle-access-lhs  (get-recv-lhs ass)))
           (rhs       (handle-access-rhs  (get-recv-rhs ass)))
           (this-ass `(,kw ,lhs ,rhs))
           (res       (if (null? seq)
                          this-ass
                          `(sequence ,@seq ,this-ass))))

      (if (not (null? seq)) ;; print if changing
          (dis "handle-access-recv : returning : " res dnl))
      res
      )
    )
  )


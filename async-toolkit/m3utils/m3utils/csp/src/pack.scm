;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pack.scm
;;
;; Insert temporaries to handle pack and unpack operations reasonably well.
;;

(define *unpack* #f)

(define (insert-unpack-copies prog)
  ;;
  ;; consider the following CSP code
  ;;
  ;; unpack(s, x)
  ;;
  ;; The compiler's implementation of unpack in the Modula-3 back end
  ;; (and probably other, future back-ends) uses VAR parameters for
  ;; the value being unpacked.  This means that unless we wish the
  ;; value to be destroyed in the process, we have to make a copy
  ;; first.  The value of x will likely be zero after the unpacking
  ;; operation.
  ;;
  ;; The generated code is thus:
  ;;
  ;; int temp;
  ;; temp = x;
  ;; unpack(s, temp); // temp is destroyed
  ;;
  
  (define tg (make-name-generator "unpack-copy"))
  
  (define (visitor s)
    (if (eq? 'eval (get-stmt-type s))
        (let* ((expr (cadr s))
               (expr-type (car expr)))
          (dis "visitor : eval : " s dnl)
          (if (not (eq? 'call-intrinsic expr-type))
              (error "make-assignments-tbl called on non-inlined code : " s))
          
          (let ((inam (cadr expr)))
            (if (eq? inam 'unpack)
                (let* ((newid (tg 'next))
                       (decl  (make-var1-decl newid *default-int-type*))
                       (ass   `(assign (id ,newid) ,(cadddadr s)))
                       (seq   `(sequence ,decl
                                         ,ass
                                         (eval (call-intrinsic
                                                unpack
                                                ,(caddadr s)
                                                (id ,newid)))))
                       )
                  (set! *unpack* s)
                  seq)
                s))
          );;*tel
        s);;fi
    )
  
  (visit-stmt prog visitor identity identity)
  )

(define *pack* #f)

(define (insert-pack-copies prog)
  ;;
  ;; consider the following CSP code
  ;;
  ;; x = pack(s)
  ;;
  ;; The compiler's implementation of pack, as its implementation of unpack,
  ;; uses VAR parameters and modifies x piecemeal for each field and member of
  ;; s.  The problem here is that if x is a narrower variable than pack needs
  ;; to represent the result, we may wind up with garbage (I am not sure about
  ;; this, actually, hmm.. but there would at least be a lot of type checking
  ;; and type conversion).  Instead, we pack into a temporary and perform
  ;; the type conversion on a separate assignment to x.
  ;;
  ;; The generated code is thus:
  ;;
  ;; int temp;
  ;; temp = pack(s);
  ;; x = temp; // may truncate
  ;;
  
  (define tg (make-name-generator "pack-copy"))
  
  (define (visitor s)
    (if (eq? 'assign (get-stmt-type s))
        (let* ((lhs (get-assign-lhs s))
               (rhs (get-assign-rhs s)))
          
          (dis "visitor : eval : " s dnl)
          (if (and (list? rhs)
                   (eq? 'call-intrinsic (car rhs))
                   (eq? 'pack (cadr rhs)))

              (let* ((src   (caddr rhs))
                     (newid (tg 'next))
                     (decl  (make-var1-decl newid *default-int-type*))
                     (ass   `(assign ,lhs (id ,newid)))
                     (seq   `(sequence ,decl
                                       (assign (id ,newid)
                                               (call-intrinsic pack ,src))
                                       ,ass)))

                (set! *pack* s)
                seq)

              s
              );;fi
          );;*tel
        s);;fi
    )
  
  (visit-stmt prog visitor identity identity)
  )


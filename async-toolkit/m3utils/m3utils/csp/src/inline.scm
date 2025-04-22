
(define assign-s    #f)
(define assign-fnam #f)
(define assign-fres #f)

(define *last-copyio* #f)
(define crap #f)

(define (inline-evals the-inits the-text func-tbl struct-tbl cell-info)

  (define initvars (find-referenced-vars the-inits))
  

  (define (visitor s syms)
    
    (define (handle-actual call-sfx actual formal)
      (define tg (make-name-generator "-inline-actual"))
      
      ;;
      ;; each actual is a designator
      ;; the designator references a variable, which has a type
      ;; but the designator can have type modifiers (array dereferences
      ;; or structure field references)
      ;;
      ;; So, we "clarify" the type of the designator so that we
      ;; know the precise type of the designator before generating
      ;; the function prolog.
      ;;
      ;; Note that often we don't really care so much about the type
      ;; of the designator.  This is because the type of the inlined
      ;; variable is the type of the formal, not the type of the actual.
      ;; There is one exception to this: array sizes are given by the
      ;; actual, not by the formal.  If the types don't match exactly,
      ;; an element-by-element type conversion will eventually need to
      ;; be performed.
      ;;

      ;; here:
      ;; 
      ;; actual is an expression
      ;; formal is a declarator
      
      (dis dnl dnl
           "handle-actual      actual : " actual dnl) 
      (dis "handle-actual      formal : " formal dnl)
      
      
      (dis "handle-actual syms        : " (get-symbols syms) dnl)

      (let* (
             ;; analyze the formal, based on its declaration
             (formal-type (get-decl1-type formal))
             (formal-id   (get-decl1-id formal))
             (formal-dir  (get-decl1-dir formal))
             
             ;; this is what the copyin variable will be called
             (copyin-id   (symbol-append formal-id call-sfx))
             (newass-in   (make-assign `(id ,copyin-id) actual))
             
             (literal-actual (literal? actual)))
        
        (dis "handle-actual formal      : " formal-id dnl)
        (dis "handle-actual copyin-id   : " copyin-id dnl)
        (dis "handle-actual formal type : " formal-type dnl)
        (dis "handle-actual formal dir  : " formal-dir  dnl)
        (dis "handle-actual copyin  code: " newass-in dnl)

        (if (not (simple-operand? actual))
            
            ;; not a simple type
            (let ((actual-type (derive-type actual syms func-tbl struct-tbl cell-info))
                  (newvar      (make-var1-decl copyin-id formal-type))
                  )
              (define-var! syms copyin-id formal-type)
              (dis "actual is not a simple operand : " (stringify actual) dnl)
              (dis "actual-type : " actual-type dnl)
                

              (if (not (eq? formal-dir 'in))
                  (error "not a writable designator : " actual))
              
              (cons
               (list newvar newass-in)
               '()
               )
              
              )
            
            ;; a simple type
            (let* ((actual-type
                    (if literal-actual
                        (literal-type actual)
                        (let ((id          (get-designator-id actual)))
                          (dis "handle-actual id          : " id dnl)
                          (retrieve-defn id syms))))
                   
                   (copyin-type (clarify-type formal-type
                                              actual-type
                                              actual))
                   
                   (newvar      (make-var1-decl copyin-id copyin-type))
                   (newass-out  (make-assign actual `(id ,copyin-id)))
                   )
              (define-var! syms copyin-id copyin-type)
              (dis "handle-actual copyin type : " copyin-type dnl)
              (dis "handle-actual copyout code: " newass-out dnl)
              
              (set! ha-a actual)
              (set! ha-at actual-type)
              (set! ha-ft formal-type)
              
              (dis dnl dnl)
              
              (cons (if (member formal-dir '(in inout))
                        (list newvar newass-in)
                        (list newvar))
                    
                    (if (member formal-dir '(inout out))
                        (list newass-out)
                        '())
                    )
              
              )
            )
        )
      )

    (define (handle-func fnam actuals lhs)
      (define tg (make-name-generator "-inline-func-body"))

      (let* ((fdef      (begin
                         (let ((res (func-tbl 'retrieve fnam)))
                           (dis "handle-func " fnam " -> " res dnl)
                          res))
                        )
             
             ;; if func-tbl doesn't contain a definition for the
             ;; function requested, we assume it's an intrinsic.
             
             (intrinsic    (eq? fdef '*hash-table-search-failed*))
             (sfx       (if intrinsic "" (tg 'next)))
             (fdef1text (if intrinsic
                            `(call-intrinsic ,fnam ,@actuals)
                            (uniquify-function-text fdef sfx cell-info initvars)))
             )
        (dis "pre-inline : " (stringify s) dnl)
        (dis "lhs        : " (stringify lhs) dnl)
        (dis "actuals    : " (stringify actuals) dnl)
        (dis "fdef       : " (stringify fdef) dnl)
        (dis "fdef1text  : " (stringify fdef1text) dnl)
        
        (if intrinsic
            ;; intrinsic:
            (if (null? lhs)

                (list 'eval fdef1text)

                `(assign ,lhs ,fdef1text))
            

            ;; not intrinsic:
            (let* ((formals    (get-function-formals fdef))
                   (ftype      (get-function-return fdef))
                   (f-inst-nam (symbol-append fnam sfx))
                   (fnamvarlst (if (null? ftype) '()
                                   (list (make-var1-decl f-inst-nam ftype))))
                   (copyinout  (map
                                (lambda(a f)(handle-actual sfx a f))
                                actuals formals)))

              (if (not null? ftype)
                  (define-var! syms f-inst-nam ftype))
              
              (dis "copyinout " (stringify copyinout) dnl)
              
              (set! *last-copyio* copyinout)
              (let*(
                    (copyin  (apply append (map car copyinout)))
                    (copyout (apply append (map cdr copyinout)))
                    
                    (assign-result
                     (if (null? lhs)
                         'skip
                         (make-assign lhs
                                            (make-ident f-inst-nam))))
                    
                    (seq
                     (cons 'sequence
                           (append copyin
                                   fnamvarlst
                                   (list fdef1text)
                                   (list assign-result)
                                   copyout)))
                    )

                (set! crap seq)
                (dis "formals    : " formals dnl)
                (dis "fdef1text  : " fdef1text dnl)
                (dis "copyin     : " copyin dnl)
                (dis "assign-res : " assign-result dnl)
                (dis "copyout    : " copyout dnl)
                (dis "seq        : " seq dnl)
                
                seq
                ;;              (error)
                )
              )
            )
        )
      )

    (case (get-stmt-type s)

      ((eval)
       (set! *eval-s*    (cons s *eval-s*))
       (set! *eval-syms* (cons syms *eval-syms*))
       (if (eq? (caadr s) 'apply)
           (let* ((fnam      (cadadadr s))
                  (fres      (handle-func fnam (cddadr s) '()))
                  )
             (dis "fres = " fres dnl)
             (if (eq? 'failed fres) s fres)
            ;; (error)
             )
           s))

      ((assign)
       (let ((lhs (get-assign-lhs s))
             (rhs (get-assign-rhs s)))
         (if (and (pair? rhs) (eq? (car rhs) 'apply))
             (let* ((fnam (cadadr rhs))
                    (fres (handle-func fnam (cddr rhs) lhs))
                    )
               (set! assign-s    s)
               (set! assign-fnam fnam)
               (set! assign-fres fres)
               fres
               ;;(error)
               )
             s)
         )
       )
      
      (else s)

      )
    )

  (symtabvisit-program the-inits the-text visitor)
  )


(define *visit-s* #f)


(define *stmt-previsit* #f)
(define *stmt-postvisit* #f)
(define *expr-previsit* #f)
(define *expr-postvisit* #f)
(define *type-previsit* #f)
(define *type-postvisit* #f)

(define s-history '())

(define (re-visit)
  (prepostvisit-stmt *visit-s*
                     *stmt-previsit* *stmt-postvisit*
                     *expr-previsit* *expr-postvisit*
                     *type-previsit* *type-postvisit*)
  )
   
(define (prepostvisit-stmt s
                           stmt-previsit stmt-postvisit
                           expr-previsit expr-postvisit
                           type-previsit type-postvisit)
  ;; first run stmt-previsit
  ;; if it returns #f, stop recursing
  ;; if it returns (cut . XXX) return XXX
  ;; else run (stmt-postvisit (prepostvisit-stmt ...))

  (define (stmt ss)(prepostvisit-stmt
                    ss
                    stmt-previsit stmt-postvisit
                    expr-previsit expr-postvisit
                    type-previsit type-postvisit
                    ))
    
  (define (expr x)(prepostvisit-expr
                   x
                   stmt-previsit stmt-postvisit
                   expr-previsit expr-postvisit
                   type-previsit type-postvisit
                   ))

  (define (range x)(prepostvisit-range x
                                       stmt-previsit stmt-postvisit
                                       expr-previsit expr-postvisit
                                       type-previsit type-postvisit))

  (set! *visit-s* s)
  (set! s-history (cons s s-history))
  (set! *stmt-previsit* stmt-previsit)
  (set! *stmt-postvisit* stmt-postvisit)
  (set! *expr-previsit* expr-previsit)
  (set! *expr-postvisit* expr-postvisit)
  (set! *type-previsit* type-previsit)
  (set! *type-postvisit* type-postvisit)
  
  (define (continue s)
    ;; this procedure does most of the work, it is called after stmt-previsit
    (if (eq? s 'skip)
        s
        (begin
          (if (not (pair? s))
              (begin
                (set! *bad-s* s)
                (set! *bad-last* last)
                (error "Not a statement : " s dnl "last : " last)))
          
          (let ((kw   (car s))
                (args (cdr s))
                )
            
            ;; we are really just re-assembling the statement here, from
            ;; its parts
            (cons kw
                  (case kw
                    ((sequence parallel)
                     ;; filter out anything that returns 'delete
                     (filter filter-delete (map stmt args)))
                    
                    ((assign)            (list
                                          (expr (car args)) (expr (cadr args))))
                    
                    ((loop increment decrement var)
                     (error "visit-stmt : need to desugar : " kw))
                    
                    ((var1)
                     (cdr (prepostvisit-var1-stmt
                           s

                           stmt-previsit stmt-postvisit
                           expr-previsit expr-postvisit
                           type-previsit type-postvisit
                           )))
                    
                    ((recv send)
                     (list (expr (car args)) (expr (cadr args))))
                    
                    ((do if nondet-if nondet-do)
                     (set! vs s)
                     ;; what happens if stmt returns 'delete?
                     (map (lambda(gc)(list (expr (car gc)) (stmt (cadr gc))))
                          args)
                     )
                    
                    ((eval)
;;                     (dis "visit eval stmt : " (stringify args) dnl)
                     (list (expr (car args))))

                    ((assign-operate)
                     (list (car args) (expr (cadr args)) (expr (caddr args))))

                    ((parallel-loop sequential-loop)
                     ;; what happens if stmt returns 'delete?
                     (let ((new-stmt (stmt (caddr args))))
                       (if (eq? new-stmt 'delete)
                           'delete
                           (list (car args)
                                 (range (cadr args))
                                 new-stmt)))
                     )
                    
                    (else (set! *bad-s* s)
                          (set! *bad-last* last)
                          (error "visit-stmt : unknown statement " s))
                    )))
          )
        )
    )
 

  ;; Basic order of visiting:
  ;;
  ;; if the pre-visitor returns #f, skip the statement (and keep it unchanged)
  ;; if the pre-visitor returns 'cut, accept the change but stop visiting
  ;; else, just call the post-visitor on the result of the pre-visitor
  ;;

  (let ((pre (stmt-previsit s)))

;;    (dis "pre returns " (stringify pre) dnl)
    
    (cond ((eq? pre #f) s)
          ((and (pair? pre) (eq? 'cut (car pre))) (cdr pre))
          ((eq? pre 'delete) 'delete) ;; will be caught in caller
          (else (stmt-postvisit (continue pre)))))
  )
  
(define (filter-delete stmt) ;; use to filter out 'delete
  (not (eq? 'delete stmt)))

(define (prepostvisit-range x
                            stmt-previsit stmt-postvisit
                            expr-previsit expr-postvisit
                            type-previsit type-postvisit)
  
  (define (expr x)(prepostvisit-expr x
                                     stmt-previsit stmt-postvisit
                                     expr-previsit expr-postvisit
                                     type-previsit type-postvisit))

  (if (or (not (pair? x)) (not (eq? 'range (car x))))
      (error "not a range : " x))
  (list 'range (expr (cadr x)) (expr (caddr x))))
  

(define *visit-x* #f)
  
(define (prepostvisit-expr x
                           stmt-previsit stmt-postvisit
                           expr-previsit expr-postvisit
                           type-previsit type-postvisit)
  
  (define (expr x)(prepostvisit-expr x
                                     stmt-previsit stmt-postvisit
                                     expr-previsit expr-postvisit
                                     type-previsit type-postvisit))

  (define (range x)(prepostvisit-range x
                                     stmt-previsit stmt-postvisit
                                     expr-previsit expr-postvisit
                                     type-previsit type-postvisit))

  (define (expr-or-null x) (if (null? x) '() (expr x)))

  (set! *visit-x* x)
  
  (define (continue)
    (if (pair? x)
        (let ((kw   (car x))
              (args (cdr x)))
          (cons kw
                (case kw
                  ((id) args)
                  
                  ;; unary/binary ops
                  ((probe array-access - not
                    + / % * == != < > >= <= & && | || ^ << >> ** ) ;; | )
                   (map expr args))
                  
                  ((apply call-intrinsic)
;;                   (dis "visit apply expr : " (map stringify args) dnl)
                   (cons (car args) (map expr (cdr args))))
                  
                  ((member-access structure-access)
                   (list (expr (car args)) (cadr args))
                   )

                  ((loop-expression)
                   (list 
                    (car args)
                    (range (cadr args))
                    (caddr args)
                    (expr (cadddr args))
                   ))

                  ((recv-expression peek)
                   (list (expr (car args))))

                  ((bits)
                   (list 
                    (expr-or-null (car args))
                    (expr-or-null (cadr args))
                    (expr-or-null (caddr args))))
                  
                  (else (error "visit-expr : unknown keyword " kw " : " x ))
                  ) ;; esac
                
                );; snoc
          
          );;tel
        x ;; not a pair
        );;fi
    );;enifed

  ;; Basic order of visiting:
  ;;
  ;; if the pre-visitor returns #f, skip the statement (and keep it unchanged)
  ;; if the pre-visitor returns 'cut, accept the change but stop visiting
  ;; else, just call the post-visitor on the result of the pre-visitor
  ;;

  (let ((pre (expr-previsit x)))
    (cond ((eq? pre #f) x)
          ((and (pair? pre) (eq? 'cut (car pre))) (cdr pre))
          (else (expr-postvisit (continue)))))

  );;enifed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; short forms for postvisit only
;;

(define (visit-stmt s stmt-visitor expr-visitor type-visitor)
  (prepostvisit-stmt s
                     identity stmt-visitor
                     identity expr-visitor
                     identity type-visitor))

(define (visit-expr s stmt-visitor expr-visitor type-visitor)
  (prepostvisit-expr s
                     identity stmt-visitor
                     identity expr-visitor
                     identity type-visitor))

(define (visit-type t stmt-visitor expr-visitor type-visitor)
  (prepostvisit-type t
                     identity stmt-visitor
                     identity expr-visitor
                     identity type-visitor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

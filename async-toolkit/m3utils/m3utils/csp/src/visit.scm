
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; visitors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; the type visitor doesn't actually get at structure types
;; because structure types are defined in a separate space to the
;; text of the CSP process.



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


  (define (visit-waiting-clause cl)
    (let ((dummy     (car cl))
          (sens      (cadr cl))
          (guardtext (caddr cl))
          (cmd       (cadddr cl)))
      (list dummy sens (stmt guardtext) (stmt cmd))
      )
    )
  
  
  (define (visit-waiting-if)
    (map visit-waiting-clause (cdr s)))
  
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
                    
                    ((assign)
                     (let ((res
                            (list
                             (expr (car args))
                             (expr (cadr args)))))
                       (if (not (equal? args res))
                           (dis "visit-assign : " s " -> " (cons kw res) dnl))
                       res))
                    
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
                     (map (lambda(gc)
                            (let ((guard   (car gc))
                                  (command (cadr gc)))
                            (list
                             (if (eq? guard 'else) 'else (expr guard))
                             (stmt command))))
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

                    ((waiting-if)
                     (visit-waiting-if)
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
  
(define (prepostvisit-expr xx
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

  (set! *visit-x* xx)
  
  (define (continue x)
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

  (let ((pre (expr-previsit xx)))
    (cond ((eq? pre #f) xx)
          ((and (pair? pre) (eq? 'cut (car pre))) (cdr pre))
          (else (expr-postvisit (continue pre)))))

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

(define (visit-range r stmt-visitor expr-visitor type-visitor)
  (prepostvisit-range r
                     identity stmt-visitor
                     identity expr-visitor
                     identity type-visitor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prepostvisit-range r
                            stmt-previsit stmt-postvisit
                            expr-previsit expr-postvisit
                            type-previsit type-postvisit)

  (if (or (not (pair? r)) (not eq? 'range (car r)))
      (error "not a range : " r))

  (list 'range
        ;; is this the right way to do it?
        (expr-postvisit (expr-previsit (cadr r)))
        (expr-postvisit (expr-previsit (caddr r)))) 
  )

(define (prepostvisit-type ttt
                           stmt-previsit stmt-postvisit
                           expr-previsit expr-postvisit
                           type-previsit type-postvisit)

  (define (type tt) (prepostvisit-type tt
                                       stmt-previsit stmt-postvisit
                                       expr-previsit expr-postvisit
                                       type-previsit type-postvisit))

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

  (define (continue t)
    (cond ((not (pair? t)) t)
          ((eq? 'array (car t))
           (list 'array (prepostvisit-range
                         (cadr t)
                         stmt-previsit stmt-postvisit
                         expr-previsit expr-postvisit
                         type-previsit type-postvisit)
                 
                 (type (caddr t))))
          ((eq? 'integer (car t))
           (let ((is-const  (cadr t))
                 (is-signed (caddr t))
                 (dw        (cadddr t))
                 (the-range (caddddr t)))
                           
             `(integer ,is-const
                       ,is-signed
                       ,(expr dw)
                       ,(if (null? the-range) '() (range the-range)))
           ))
           
          (else t))
        )

;;  (dis "visit type " ttt dnl)

  (let ((pre (type-previsit ttt)))
    (cond ((eq? pre #f) ttt)
          ((and (pair? pre) (eq? 'cut (car pre))) (cdr pre))
          (else (type-postvisit (continue pre)))))
  )
  
(define (prepostvisit-declarator d 
                         stmt-previsit stmt-postvisit
                         expr-previsit expr-postvisit
                         type-previsit type-postvisit)
;;  (dis "visit declarator " d dnl)
  
  (if (not (equal? 'decl1 (car d)))
      (error "prepostvisit-declarator : not a desugared declarator : " d))

   (let ((ident (cadr d))
         (type  (caddr d))
         (dir   (cadddr d)))
     (list (car d)
           ident
           (prepostvisit-type type 
                              stmt-previsit stmt-postvisit
                              expr-previsit expr-postvisit
                              type-previsit type-postvisit)
           dir)
     )
   )
  

(define (prepostvisit-var1-stmt s
                         stmt-previsit stmt-postvisit
                         expr-previsit expr-postvisit
                         type-previsit type-postvisit)
  (list 'var1 (prepostvisit-declarator (cadr s)
                                       stmt-previsit stmt-postvisit
                                       expr-previsit expr-postvisit
                                       type-previsit type-postvisit))
)


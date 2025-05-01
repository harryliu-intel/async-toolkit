;;
;; code generation
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; routines for initial cleanup
;;

(define (retrieve-var1 v1lst id)
  (car (filter (lambda(v1)(eq? id (get-var1-id v1) )) v1lst)))

(define (filter-out-var1s lisp)
  ;; remove all var1 statements from lisp
  (define (visitor s)
    (case (get-stmt-kw s)
      ((var1)   'skip)
      (else s))
    )
  (fixpoint-simplify-stmt (visit-stmt lisp visitor identity identity))
  )

(define (filter-in-var1s lisp)

  (define res '())
  
  (define (visitor s)
    (case (get-stmt-kw s)
      ((var1)
       (set! res (cons s res))
       s)
      (else     s))
    )
  
  
  (visit-stmt lisp visitor identity identity)
  res
  )

(define (sequence-length seq) (length (cdr seq)))

(define (gen-decls the-blocks prop-types)
  ;; generate all program declarations
  ;; prefer prop-types over the declared types
  (let* ((decl-var1s  (apply append (map (curry1 find-stmts 'var1) the-blocks)))

         (prop-syms  (prop-types 'keys))

         (prop-var1s
          (map (lambda(sym)(make-var1-decl sym (prop-types 'retrieve sym)))
               prop-syms))

         (decl-vars (map get-var1-id decl-var1s))

         (prop-vars (map get-var1-id prop-var1s))

         (all-vars (set-union decl-vars prop-vars))
         
         (chosen-decls
          (map
           (lambda(id) (if (member id prop-vars)
                           (retrieve-var1 prop-var1s id)
                           (retrieve-var1 decl-var1s id)))
           all-vars))
         
         )

    chosen-decls
    )
  )

(define (visit-blocks blk-list v)
  (map (lambda(blk)(visit-stmt blk v identity identity)) blk-list))

(define (get-label-dummies the-blocks)
  ;; get label dummies from dynamic fork/join statements
  (define res '())

  (define (visitor s)
    (case (get-stmt-kw s)
      ((label)
       (if (and (not (null? (cddr s)))
                (eq? 'fork (caddr s)))
           (set! res (cons (cadddr s) res))))))

  (visit-blocks the-blocks visitor)
  res
  )

(define (get-shared-variables the-blocks cell-info)
  (let* ((port-ids         (get-port-ids cell-info))
         (var-refs         (map find-referenced-vars the-blocks))
         (shared-ids       (multi (apply append var-refs)))
         (shared-var-ids   (set-diff shared-ids port-ids)))
    shared-var-ids
    )
  )

(define (get-block-variables the-blocks cell-info)
  (let* ((port-ids         (get-port-ids cell-info))
         (var-refs         (map find-referenced-vars the-blocks))
         (shared-ids       (uniq eq? (apply append var-refs)))
         (shared-var-ids   (set-diff shared-ids port-ids)))
    shared-var-ids
    )
  )

(define (get-block-label blk)
  (cond ((label? blk) blk)
        
        ((sequence? blk)
         (let ((first (cadr blk)))
           (if (not (label? first))
               (error "first statement in block is not a label : " first))
           first
           ))

        (else          (error "block is not a sequence : " blk))
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the following will need to be enhanced for structured ports
;;

(define (port-direction pdef) (caddr pdef)) ;; works for both channel and node


(define (input-port? pdef) (eq? 'in (port-direction pdef)))
(define (output-port? pdef) (eq? 'out (port-direction pdef)))

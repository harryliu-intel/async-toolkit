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
  ;;
  ;; generate all program declarations
  ;; prefer prop-types over the declared types
  ;; 
  ;; but apply the array declaration from declared types in any case
  ;; 
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
           (lambda(id) (let ((have-prop (member id prop-vars))
                             (have-decl (member id decl-vars)))
                         (cond
                          ((and have-prop have-decl)
                           (let
                               ((type
                                 (smush-type
                                  (get-var1-type (retrieve-var1 decl-var1s id))
                                  (get-var1-type (retrieve-var1 prop-var1s id))
                                  )))
                             (make-var1-decl id type)
                             ))
                             
                          (have-prop
                           (retrieve-var1 prop-var1s id))

                          (have-decl
                           (retrieve-var1 decl-var1s id))

                          (else (error "gen-decls : no type for : " id))
                          );;dnoc
                         );;tel
                  )
           all-vars))
         
         )

    chosen-decls
    )
  )

(define (smush-type decl prop)
  ;; if decl is an element type and prop is an array type,
  ;; return the type that is the array with the extent of prop
  ;; but with the base type decl
  (dis "smush-type : decl : " decl dnl)
  (dis "smush-type : prop : " prop dnl)
  (if (array-type? decl)
      (make-array-type (array-extent decl)
                       (smush-type (array-elemtype decl) prop))
      prop)
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

(define (array-dims type)
  (if (not (array-type? type))
      0
      (+ 1 (array-dims (peel-array type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the following will need to be enhanced for structured ports
;;

(define (port-direction pdef) (caddr pdef)) ;; works for both channel and node


(define (input-port? pdef) (eq? 'in (port-direction pdef)))
(define (output-port? pdef) (eq? 'out (port-direction pdef)))


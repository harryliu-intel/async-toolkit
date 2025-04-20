
(define (retrieve-var1 v1lst id)
  (car (filter (lambda(v1)(eq? id (get-var1-id v1) )) v1lst)))

(define (gen-decls the-text prop-types)
  ;; generate all program declarations
  (let* ((decl-var1s  (apply append (map (curry1 find-stmts 'var1) text8)))

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

(define (filter-out-var1s lisp)
  (define (visitor s)
    (case (get-stmt-kw s)
      ((var1)   'skip)
      (else s))
    )
  (fixpoint-simplify-stmt (visit-stmt lisp visitor identity identity))
  )

(define (sequence-length seq) (length (cdr seq)))

(define (empty-block? blk)
  (or (eq? blk 'skip)
      (and (sequence? blk)
           (= 2 (sequence-length blk))
           (label? (first (cdr blk)))
           (goto? (last blk)))))
  
                      
(define (find-empty-blocks blk-lst)
  (filter empty-block? blk-lst))

(define label-label cadr)
(define goto-label cadr)

(define (find-empty-label-remap blk-lst)
  (map (lambda(blk)
         (if (sequence? blk)
             (cons (label-label (cadr blk))
                   (goto-label  (last blk)))
             #f
             )
         )
       (filter identity (find-empty-blocks blk-lst))
       )
  )

(define (remap-label blk from to)
  (define (visitor s)
    (case (get-stmt-kw s)
      ((goto)    (if (eq? from (goto-label  s)) `(goto ,to) s))
      ((label)   (if (eq? from (label-label s)) `(label ,to) s))
      (else s))
    )
  (visit-stmt blk visitor identity identity)
  )

(define (remap-labels remap blk)
  (if (null? remap)
      blk
      (remap-labels (cdr remap)
                    (remap-label blk (caar remap) (cdar remap))))
  )

(define (remove-empty-blocks blk-lst)
  (let ((remap     (find-empty-label-remap blk-lst))
        (short-lst (filter (filter-not empty-block?) blk-lst))
        )
    (map (lambda(blk)(remap-labels remap blk)) short-lst)))

                    

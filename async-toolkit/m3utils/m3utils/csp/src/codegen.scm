;;
;; code generation
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; routines for initial cleanup
;;

(define (retrieve-var1 v1lst id)
  (car (filter (lambda(v1)(eq? id (get-var1-id v1) )) v1lst)))

(define (gen-decls the-text prop-types)
  ;; generate all program declarations
  ;; prefer prop-types over the declared types
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
  ;; remove all var1 statements from lisp
  (define (visitor s)
    (case (get-stmt-kw s)
      ((var1)   'skip)
      (else s))
    )
  (fixpoint-simplify-stmt (visit-stmt lisp visitor identity identity))
  )

(define (sequence-length seq) (length (cdr seq)))

(define (empty-block? blk)

  ;; an empty block is either
  ;; 1. skip
  ;; 2. label X ; goto Y
  
  (or (eq? blk 'skip)
      (and (sequence? blk)
           (= 2 (sequence-length blk))
           (or (simple-label? (first (cdr blk)))
               (fork-label? (first (cdr blk))))
           (simple-goto? (last blk)))))

(define (simple-label? x) (and (label? x) (null?   (cddr x))))

(define (fork-label? x) (and (label? x) (equal?  (caddr x) 'fork)))
                      
(define (simple-goto? x) (and (goto? x) (null? (cddr x))))
                      
(define (find-empty-blocks blk-lst) (filter empty-block? blk-lst))

(define label-label cdr)
(define goto-label cdr)

(define (find-empty-label-remap blk-lst)
  ;; find the remapping map to get rid of empty blocks
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
  ;; remap a single label from from to to
  (define (visitor s)
    (case (get-stmt-kw s)
      ((goto)    (if (equal? from (goto-label  s)) `(goto ,@to) s))
      ((label)   (if (equal? from (label-label s)) `(label ,@to) s))
      (else s))
    )
  (visit-stmt blk visitor identity identity)
  )

(define (remap-labels remap blk)
  ;; remap all the labels according to the remapping map remap;
  ;; remap is a list of conses (from . to)
  (if (null? remap)
      blk
      (remap-labels (cdr remap)
                    (remap-label blk (cdar remap) (caar remap))))
  )

(define (remove-empty-blocks blk-lst)

  ;; remove all the empty blocks from the blk-lst,
  ;; and patching the labels so control flow remains
  
  (let ((remap     (find-empty-label-remap blk-lst))
        (short-lst (filter (filter-not empty-block?) blk-lst))
        )
    (map (lambda(blk)(remap-labels remap blk)) short-lst)))

                    
(load "codegen-m3.scm")

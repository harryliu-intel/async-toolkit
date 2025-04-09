

(define (make-assignments-tbl prog cell-info the-inits func-tbl struct-tbl)

  (define tbl (make-hash-table 100 atom-hash))
  
  (define (visitor s syms vals tg func-tbl struct-tbl)

    (define (add-entry! designator)
      (let* ((id        (get-designator-id designator))
             (new-entry (list s syms id vals))
             )

        (if (not id) (error "no designator id in " designator dnl))
        
        (let ((q (tbl 'retrieve id)))
          (if (eq? q '*hash-table-search-failed*)
              (tbl 'add-entry!    id (list new-entry))
              (tbl 'update-entry! id (cons new-entry q))))))
      

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (case (get-stmt-type s)
      ((assign)
       
       (let* ((lhs       (get-assign-lhs s)))
         (add-entry! lhs))
       )

      ((recv)
       (let ((lhs (get-recv-lhs s))
             (rhs (get-recv-rhs s)))
;;         (dis "recv     : " s dnl)
;;         (dis "recv lhs : " lhs dnl)
;;         (dis "recv rhs : " rhs dnl)

         (if (not (null? rhs)) (add-entry! rhs)))
       )

      ((assign-operate)  ;; should be transformed out
       (error))


      ((eval)
       ;; should only be a call-intrinsic at this point
       (let* ((expr (cadr s))
              (expr-type (car expr)))
         (if (not (eq? 'call-intrinsic expr-type))
             (error "make-assignments-tbl called on non-inlined code : " s))

         (let ((inam (cadr expr)))
           (if (eq? inam 'unpack)
               (add-entry! (caddr expr))))))
      )
    s
    )

  ;;  (visit-stmt tgt visitor identity identity)


  (run-pass (list '* visitor)
            prog cell-info the-inits func-tbl struct-tbl)

  tbl
  )

(define (make-asses prog)
  (make-assignments-tbl prog *cellinfo* *the-inits* '() *the-struct-tbl*))
         
    
(define (constant-assignment? ass syms)
  (let ((rhs (get-assign-rhs ass)))
    (constant-simple? rhs syms)))

(define (ass-tgt-designator stmt)
  (case kw
    ((assign) (get-designator-id (get-assign-lhs stmt)))
    ((recv)   (let ((tgt (get-assign-rhs stmt)))
                (if (null? tgt) '() (get-designator-id (get-recv-rhs stmt)))))
    (else (error "not an assigning statement : " stmt))))

(define (find-constant-symbols tbl)
  (let* ((keys      (tbl 'keys))  ;; all the keys from the assignment table
         
         (ass-lsts  (map (lambda(k)(tbl 'retrieve k)) keys))
         ;; get the assignments for each key
         
         (ssa-lst   (map car (filter (lambda(l)(= (length l) 1)) ass-lsts)))
         ;; filter out the single assignment variables
         
         (con-lsts  (filter (lambda(e)(constant-assignment? (car e) (cadr e)))
                            ssa-lst))
         ;; and filter out the constant assignments from that

         )

    (map caddr con-lsts) ;; return just the symbols
    
    )
  )

(define (make-var1-constant v1)
  (make-var1-decl (get-var1-id v1) (make-constant-type (get-var1-type v1)))) 

(define (mark-decls-constant prog constant-ids)
  (define (visitor s)
    (if (eq? 'var1 (get-stmt-type s))
        (if (member (get-var1-id s) constant-ids)
            (let ((res (make-var1-constant s)))
              (dis "mark-decls-constant " (get-var1-id s) " " res dnl)
              res
              )
            s
            )
        s
        )
    )

  (visit-stmt prog visitor identity identity)
  )


(define (vx)
   (find-constant-symbols the-asses)

   (define the-asses (make-asses text4))

   (map car (the-asses 'retrieve 'run-pass-temp177))
)

(define (constantify-constant-vars prog)
  (let* ((the-asses         (make-asses prog))
         (the-constant-syms (find-constant-symbols the-asses))
         (the-new-prog      (mark-decls-constant prog the-constant-syms)))
    the-new-prog
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    


(define (make-uses prog)

  (define tbl (make-hash-table 100 atom-hash))

  (define cur-stmt #f) ;; the statement currently being processed
  
  (define (add-entry! id)
    (let* ((new-entry (list cur-stmt))
           )
;;      (dis "make-uses-tbl add-entry!  "  id " -> " cur-stmt dnl)
      (let ((q (tbl 'retrieve id)))
        (if (eq? q '*hash-table-search-failed*)
            (tbl 'add-entry!    id (list new-entry))
            (tbl 'update-entry! id (cons new-entry q))))))


  ;; the idea here is that we record every id in every expression,
  ;;
  ;; EXCEPT left-hand-sides, which appear in three places:
  ;; 1. assignments
  ;; 2. receives
  ;; 3. the first argument to unpack()
  ;;
  ;; we abort on encountering apply() and assign-operate
  
  (define (s-visitor s)

;;    (dis "s-visitor : s : " s dnl)
    (case (get-stmt-type s)

      ((apply assign-operate)
       (error "make-uses-tbl : being called too soon : encountered : " s))
      
      ((assign)
       
       (let* ((lhs         (get-assign-lhs s))
              (lhs-depends (get-designator-depend-ids lhs))
              (rhs         (get-assign-rhs s))
              (rhs-ids     (find-expr-ids rhs)))
         (map add-entry! lhs-depends)
         (map add-entry! rhs-ids))
       )

      ((recv)
       (let ((lhs (get-recv-lhs s))
             (rhs (get-recv-rhs s)))
;;         (dis "recv     : " s dnl)
;;         (dis "recv lhs : " lhs dnl)
;;         (dis "recv rhs : " rhs dnl)

         (if (not (null? rhs)) ;; we do NOT want the written variable
             (map add-entry! (get-designator-depend-ids rhs)))
         (if (not (null? lhs))
             (map add-entry! (find-expr-ids lhs))))
       )


      ((eval)
       (let* ((expr (cadr s))
              (expr-type (car expr)))
         (if (not (eq? 'call-intrinsic expr-type))
             (error "make-assignments-tbl called on non-inlined code : " s))

         ;; we add all the args as dependencies unless we're unpacking.
         (let* ((inam  (cadr expr))
                (iargs (cddr expr))
                (ids   (uniq eq?
                             (if (eq? inam 'unpack)
                                 (append
                                  (get-designator-depend-ids (car iargs))
                                  (apply append (map find-expr-ids (cdr iargs)))
                                  )
                                 
                                 (apply append (map find-expr-ids iargs)))))
                )
;;           (dis "s-visitor inam " inam dnl)
;;           (dis "s-visitor iargs " (stringify iargs) dnl)
;;           (dis "s-visitor ids " ids dnl)
           (map add-entry! ids)
           )
         )
       )
      
      );;esac
    s)

  (define (advance-callback s)
;;    (dis "advance-callback " s dnl)
    (set! cur-stmt s)
    s)
  
  (define (x-visitor x)

;;    (dis "x-visitor : x is         : " x dnl)

    (if (not (null? cur-stmt)) ;; we can also get called through type visiting

        (let ((stmt-type (get-stmt-type cur-stmt)))
;;          (dis "x-visitor : stmt-type is : " stmt-type " : ")

          (case stmt-type
            ((assign recv eval)
             ;; skip
;;             (dis "skipping" dnl)
             )

            ((parallel sequence) ;; skip
;;             (dis "skipping" dnl)
             )
             
            (else
             (let ((ids (find-expr-ids x)))
;;               (dis "found ids " ids dnl)
               (map add-entry! ids)))

            );;esac
          );;tel
        );;fi
    x
    )

;;  (dis "make-uses" dnl)
  (prepostvisit-stmt prog
                     identity  s-visitor
                     identity x-visitor 
                     identity  identity
                     advance-callback)
  
  tbl
  )


(define (delete-referencing-stmts prog ids)
  ;; delete declarations and assignments to given vars.

  (define (visitor s)
    (case (get-stmt-type s)
      ((assign)
       (if (member (get-designator-id (get-assign-lhs s)) ids)
           'skip
           s))

      ((var1)
       (if (member (get-var1-id s) ids)
           'skip
           s))

      ((recv)
       (let ((rhs (get-recv-rhs s)))
         (if (and (not (null? rhs))
                  (member (get-designator-id rhs) ids))
             `(recv (get-recv-lhs s) ())
             s)))

      (else s)
      ))

  (visit-stmt prog visitor identity identity)
  )
  

(define (delete-unused-vars-pass  the-inits prog func-tbl struct-tbl cell-info)
  (let* ((ass-tbl
          (make-assignments-tbl prog cell-info the-inits func-tbl struct-tbl))
         (use-tbl
          (make-uses prog))

         (ass-keys (ass-tbl 'keys))

         (use-keys (use-tbl 'keys))

         (unused-ids (set-diff ass-keys use-keys))

         (result
          (delete-referencing-stmts prog unused-ids)))

    result
    )
  )

  


  

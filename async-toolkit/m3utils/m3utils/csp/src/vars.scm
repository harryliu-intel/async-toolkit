

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
         (dis "recv     : " s dnl)
         (dis "recv lhs : " lhs dnl)
         (dis "recv rhs : " rhs dnl)

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

  (set! ass-tbl tbl)
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

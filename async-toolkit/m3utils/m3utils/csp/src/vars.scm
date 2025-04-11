

(define (make-assignments-tbl prog cell-info the-inits func-tbl struct-tbl)

  (define tbl (make-hash-table 100 atom-hash))
  
  (define (trace-ass-visitor s syms vals tg func-tbl struct-tbl)

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


  (run-pass (list '* trace-ass-visitor)
            prog cell-info the-inits func-tbl struct-tbl)

  tbl
  )

(define (make-asses prog)
  (make-assignments-tbl prog *cellinfo* *the-inits* '() *the-struct-tbl*))
         
    
(define (constant-assignment? ass syms)
  (let* ((rhs (get-assign-rhs ass))
         (is-const (constant-simple? rhs syms))) ;; #f or a list containing the value
    is-const))

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

  (define (dbg . x)
;;      (apply dis x)
    )

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
    (dbg "advance-callback " s dnl)
    (set! cur-stmt s)
    s)
  
  (define (x-visitor x)

    (dbg "x-visitor : x is         : " x dnl)

    (if (not (null? cur-stmt)) ;; we can also get called through type visiting

        (let ((stmt-type (get-stmt-type cur-stmt)))
          (dbg "x-visitor : stmt-type is : " stmt-type " : ")

          (case stmt-type
            ((assign recv eval)
             ;; skip
             (dbg "skipping" dnl)
             )

            ((parallel sequence) ;; skip
             (dbg "skipping" dnl)
             )
             
            (else
             (let ((ids (find-expr-ids x)))
               (dbg "found ids " ids dnl)
               (map add-entry! ids)))

            );;esac
          );;tel
        );;fi
    x
    )

  (dbg "make-uses" dnl)
  (prepostvisit-stmt prog
                     identity  s-visitor
                     identity x-visitor 
                     identity  identity
                     advance-callback)
  
  tbl
  )


(define (delete-referencing-stmts prog ids)
  ;; delete declarations and assignments to given vars.
  (if (not (null? ids)) (dis "delete-referencing-stmts : " ids dnl))
  
  (define (visitor s)
    (case (get-stmt-type s)
      ((assign)
       (if (member (get-designator-id (get-assign-lhs s)) ids)
           (begin
             (dis "delete-referencing-stmts : assign : " s dnl)
             'skip
             )
           s))

      ((var1)
       (if (member (get-var1-id s) ids)
           (begin
             (dis "delete-referencing-stmts : var1   : " s dnl)
             'skip
             )
           s))

      ((recv)
       (let ((rhs (get-recv-rhs s)))
         (if (and (not (null? rhs))
                  (member (get-designator-id rhs) ids))
             ;;
             ;; we cant delete recvs
             ;; but we can make them "bare" (remove the target var)
             ;;
             (begin
               (dis "delete-referencing-stmts : recv :   " s dnl)
               `(recv (get-recv-lhs s) ())
               )
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


          (if (not (null? unused-ids))
              (begin
                (dis "delete-unused-vars-pass  ass-keys   : " ass-keys dnl
                     "delete-unused-vars-pass  use-keys   : " use-keys dnl
                     "delete-unused-vars-pass  unused-ids : " unused-ids dnl)))
          
    result
    )
  )

(define (recv? x)(and (pair? x)(eq? 'recv (car x))))
(define (assign? x)(and (pair? x)(eq? 'assign (car x))))


(define (get-channel-type-bit-width channel-type)
  (if (or (not (eq? 'channel             (car channel-type)))
          (not (eq? 'standard.channel.bd (cadr channel-type))))
      (error "not a channel type I understand : " channel-type))
  (caaddr channel-type))


  
(define (assignment-range ass chan-tbl)
  ;; compute the range of an assignment
  ;; the "ass" should be in the format of the ass-tbl,
  ;; that is: (ass syms id vals)

  (define syms (cadr   ass))
  (define id   (caddr  ass))
  (define vals (cadddr ass))

  (dis "assignment-range syms " syms dnl)
  (dis "assignment-range id   " id   dnl)
  (dis "assignment-range vals " vals dnl)

  (define (channel-value-range channel-designator)
    (let* ((id        (get-designator-id channel-designator))
           (chan-decl (chan-tbl 'retrieve id))
           (width     (get-channel-type-bit-width (cadddr chan-decl))))

      (dis "cvr : id : " id dnl)
      (dis "cvr : cd : " chan-decl dnl)

      (make-uint-range width)
      
      )
    )
  
  (let* ((ass-stmt   (car ass))
         (is-recv    (recv? ass-stmt))
         (is-assn    (assign? ass-stmt))
         (simple-rhs (and is-assn (ident? (get-assign-rhs ass-stmt)))))

    (cond (is-recv
           (channel-value-range (get-recv-lhs ass-stmt)))

          (simple-rhs
           (declared-ident-range (get-assign-rhs ass-stmt) syms))

          (is-assn ;; an expression
           (expression-range (get-assign-rhs ass-stmt) syms))

          (else
           (error "dont understand assignment " (stringify ass)))))
  )

(define (declared-ident-range id syms)
  (let ((decl (retrieve-defn id syms)))
    (get-type-range decl))
  )

(define (expression-range expr syms)
  (bigint? expr)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-port-table cell-info)

  (define tbl (make-hash-table 100 atom-hash))
  
  (map (lambda(p)(tbl 'add-entry! (cadr p) p)) 
       (get-ports cell-info))

  tbl

)
  
(define (make-the-tables prog)
  (set! *the-ass-tbl* (make-assignments-tbl
                       prog *cellinfo* *the-inits* '() *the-struct-tbl*))
  (set! *the-use-tbl* (make-uses prog))
  (set! *the-dcl-tbl* (make-intdecls prog))
  (set! *the-prt-tbl* (make-port-table *cellinfo*))
  'ok
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-sint-range bits)
  ;; what is the integer range of a <bits>-bit sint?
  (xnum-eval
   `(make-range (- (pow 2 (- ,bits 1))  )
                (- (pow 2 (- ,bits 1)) 1) )))

(define (make-uint-range bits)
  ;; what is the integer range of a <bits>-bit uint?
  (xnum-eval
   `(make-range 0 (- (pow 2 ,bits) 1))))
          
(define (get-type-range declared-type)
  (if (not (integer-type? declared-type))
      (error "get-type-range : not an integer type : " declared-type))

  (let ((bitsiz (cadddr declared-type))
        (sint   (caddr  declared-type)))
    
    (cond ((null? bitsiz) *range-complete*)
          
          (sint        (make-sint-range bitsiz))
          (else        (make-uint-range bitsiz)))
    
    )
  )

;; we'll keep the maximum size stupidly small for now, just to exercise
;; the code without bogging down the machines with printing (which is
;; the slow part of the routine)

(define *maximum-size* 128) ;; largest finite size we'll tolerate

(define *maximum-sint-range* (make-sint-range *maximum-size*))
(define *maximum-uint-range* (make-uint-range *maximum-size*))

(define (make-intdecls prog)
  (define tbl (make-hash-table 100 atom-hash))
  
  (define (s-visitor s)
    (case (get-stmt-type s)
      ((var1)
       (let ((id (get-var1-id s))
             (ty (get-var1-type s)))

             (if (integer-type? ty)

                 (tbl 'add-entry! id (get-type-range ty)))))
      )
    s
    )
  (visit-stmt prog s-visitor identity identity)
  tbl
  )

(define (make-integer-type signed bits)
  `(integer #f ,signed ,(force-bigint bits) ()))

(define (get-smallest-type range)
  ;; we preferentially choose the uint (what CSP calls "int(.)")
  
  (cond ((range-contains? *maximum-uint-range* range)
         (make-uint-type range))
        
        ((range-contains? *maximum-sint-range* range)
         (make-sint-type range))

        (else *default-int-type*)))

        
                         
      
  

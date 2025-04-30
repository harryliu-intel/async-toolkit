(define (interval-dbg . x)
;;  (apply dis x)
  )

(define (make-binop op lst)
  ;; given a list of expressions, make binary op across them
  (cond ((null? lst) (error "make-binop of empty list"))
        ((not (list? lst)) (error "make-binop : not a list : " lst))
        ((= 1 (length lst)) (car lst))
        ((= 2 (length lst)) `(,op ,(car lst) ,(cadr lst)))
        (else `(,op ,(car lst) ,(make-binop op (cdr lst)))))
  )

(define (expr-range expr syms)
  ;; should remove globals here...
  (interval-dbg "expr-range : expr : " (stringify expr) dnl)
  
  (cond ((bigint? expr) (make-point-range expr))

        ((not (pair? expr))
         (error "expr-range : not an integer expression : " expr))

        ((ident? expr)
         (let* ((id  (cadr expr))
                (rng (search-range *the-rng-tbl* id))
                (dcl (search-range *the-dcl-tbl* id))
                (glb (search-range *the-global-ranges* id))
                (res (range-intersection rng dcl glb))
               )
           res)
         )

        ((binary-expr? expr)
         (let* ((op (car expr))
                (rop (eval (symbol-append 'range- op))))
           (rop (expr-range (cadr expr) syms)
                (expr-range (caddr expr) syms)))
         )

        ((unary-expr? expr)
         (let* ((op (car expr))
                (rop (eval (symbol-append 'range- op))))
           (rop (expr-range (cadr expr) syms)))
         )

        ((bits? expr)
         (let* ((bexpr          (get-bits-expr expr))
                (bmin           (expr-range (get-bits-min expr) syms))
                (bmax           (expr-range (get-bits-max expr) syms))

                (source-range   (expr-range bexpr syms)) ;; range of source

                (extract-mask-range ;; range of extraction
                 (range-extend
                  (range-- (range-<< *range1* bmax) *range1*)
                  *big0*)))

                 (range-& source-range extract-mask-range)))

        ((array-access? expr)
         (expr-range (cadr expr) syms))
        
        (else *range-complete*))
  )
(define (assignment-range ass port-tbl)
  ;; compute the range of an assignment
  ;; the "ass" should be in the format of the ass-tbl,
  ;; that is: (ass syms id vals)

  (set! *ar-ass* ass)

  (define syms (cadr   ass))
  (define id   (caddr  ass))
  (define vals (cadddr ass))

  (interval-dbg "assignment-range syms " syms dnl)
  (interval-dbg "assignment-range id   " id   dnl)
  (interval-dbg "assignment-range vals " vals dnl)

  (define (channel-value-range channel-designator)
    (let* ((id        (get-designator-id channel-designator))
           (chan-decl (port-tbl 'retrieve id))
           (width     (get-channel-type-bit-width (cadddr chan-decl))))

      (interval-dbg "cvr : id : " id dnl)
      (interval-dbg "cvr : cd : " chan-decl dnl)

      (make-uint-range width)
      
      )
    )

  (define (loop-range loop-stmt)
    (let* ((the-range (get-loop-range loop-stmt))
           (min-expr  (cadr the-range))
           (max-expr  (caddr the-range)))
      (interval-dbg "loop-range " loop-stmt dnl)
      (make-range (range-min (expr-range min-expr syms))
                  (range-max (expr-range max-expr syms)))))
      

  (define (get-bits-interval bs)

    (interval-dbg "get-bits-interval : bs : " bs dnl)
    (let* ((the-index-range (range-union (expr-range (get-bits-min bs) syms)
                                         (expr-range (get-bits-max bs) syms)))
           (the-range
            (range-extend  ;; this is a little bit pessimistic
             (range-- (range-<< *range1* the-index-range) *range1*)
             *big0*)))

      (interval-dbg "get-bits-interval : the-index-range : " the-index-range dnl)
      (interval-dbg "get-bits-interval : the-range       : " the-range dnl)

      the-range
;;      *range-complete*
      )
    )
  
  
  (let* ((ass-stmt   (car ass))
         (is-recv    (recv?   ass-stmt))
         (is-assn    (assign? ass-stmt))
         (is-loop    (loop?   ass-stmt))
         (is-bits-lhs(and is-assn (bits? (get-assign-lhs ass-stmt))))
         (simple-rhs (and is-assn (ident? (get-assign-rhs ass-stmt)))))

    (interval-dbg "assignment-range ass-stmt   " ass-stmt dnl)
    (interval-dbg "assignment-range is-recv    " is-recv dnl)
    (interval-dbg "assignment-range is-assn    " is-assn dnl)
    (interval-dbg "assignment-range is-loop    " is-loop dnl)
    (interval-dbg "assignment-range simple-rhs " simple-rhs dnl)
    
    (cond (is-recv
           (channel-value-range (get-recv-lhs ass-stmt)))

          (is-bits-lhs ;; we do this for now...
           (get-bits-interval (get-assign-lhs ass-stmt)))

;;          (simple-rhs
;;           (declared-ident-range (cadr (get-assign-rhs ass-stmt)) syms))

          (is-assn ;; an expression
           (expr-range (get-assign-rhs ass-stmt) syms))

          (is-loop
           (loop-range ass-stmt)
           )

          (else
           (error "dont understand assignment " (stringify ass)))))
  )

(define (var-ass-range id ass-tbl port-tbl)

  ;; I think we have a bug here.
  ;; if we have a bits() on the LHS, this won't be right, I don't think.
  
  (let* ((var-asses   (ass-tbl 'retrieve id))
         (var-ranges  (map (lambda(ass)(assignment-range ass port-tbl))
                           var-asses)))
    (interval-dbg "var-ass-range : var-asses  : " var-asses dnl)
    (interval-dbg "var-ass-range : var-ranges : " var-ranges dnl)
    var-ranges
    )
  )

(define (get-fundamental-type-range decl)
  (if (and (pair?  decl) (eq? 'array (car decl)))
      (get-fundamental-type-range (caddr decl))
      (get-type-range decl)))

(define (declared-ident-range id syms)
  (let ((decl (retrieve-defn id syms)))
    (get-fundamental-type-range decl))
  )

(define (search-range tbl id)
  (let ((tentative (tbl 'retrieve id)))
    (if (eq? tentative '*hash-table-search-failed*)
        *range-complete*
        tentative
        )
    )
  )


(define (get-loop-range-range stmt syms)
  ;; I think maybe we don't need this...
  (let* ((lr      (get-loop-range stmt))
         (lrminx  (cadr lr))
         (lrmaxx  (caddr lr))
         
         (lrminxr (expr-range lrminx syms))
         (lrmaxxr (expr-range lrmaxx syms))
         (res     (range-union lrminxr lrmaxxr))
        )

    (dis "get-loop-range-range stmt     : " stmt dnl)
    (dis "get-loop-range-range lr       : " lr dnl)

    (dis "get-loop-range-range lrminx   : " lrminx dnl)
    (dis "get-loop-range-range lrmaxx   : " lrmaxx dnl)

    (dis "get-loop-range-range lrminxr  : " lrminxr dnl)
    (dis "get-loop-range-range lrmaxxr  : " lrmaxxr dnl)

    (dis "get-loop-range-range res      : " res dnl)

    res
    )
  )

(define (seed-integer-ranges!)
  (map (lambda(id)(*the-rng-tbl* 'add-entry! id (*the-dcl-tbl* 'retrieve id)))
       (*the-dcl-tbl* 'keys))
  )

(define (get-id-range id)
  ;; based on the tables, what is the range for id?
  ;; if there is more than one assignment, we just have to return
  ;; its own declaration?
  (let*( (dcl-range (*the-dcl-tbl* 'retrieve id))
         (ass-range
          (apply range-union (var-ass-range id *the-ass-tbl* *the-prt-tbl*)))
         )
    (if (eq? dcl-range '*hash-table-search-failed*)
        ass-range
        (range-intersection ass-range dcl-range))
    )
  )

(define (update-id-range! id rng-tbl)
  (dis "update-id-range! " id dnl)
  
  (let*((old-range (rng-tbl 'retrieve id))

        (dcl-range (*the-dcl-tbl* 'retrieve id))

        (id-range  (get-id-range id))
        
        (new-range
         (cond ((eq? dcl-range '*hash-table-search-failed*)
                ;; no declaration, we have to rely on assignments
                id-range)

               ((range-contains? dcl-range id-range)

                ;; this is a bit tricky:
                ;; if the computed range actually overruns the
                ;; identifier range, we don't have a contracting range
                ;; anymore, so therefore we can't just take the intersection
                ;; -- in that case, we go to the else below.
                
                (range-intersection dcl-range id-range))
         
               (else
                dcl-range)
               )
         )
        )
        
    (if (range-empty? new-range)
        (error (string-append "update-id-range! : got empty range for : " id " : " new-range "( dcl-range = " dcl-range " ; old-range = " old-range " ; id-range = " id-range " )" )))
    
    (rng-tbl 'update-entry! id new-range)
    (not (equal? old-range new-range))
    )
  )

(define (update-id-ranges! rng-tbl)
  ;; on each iteration we have to re-evaluate the loop ranges, since
  ;; they can change.  Right?  (They literally *are* ranges.)
  (eval (apply or
               (map (lambda(id)(update-id-range! id rng-tbl))
                    (the-typed-ids))
               )
        )
  )

(define *max-range-iters* 10)

(define (close-integer-ranges!)
  (iterate-until-false
   (lambda()(update-id-ranges! *the-rng-tbl*))
   *max-range-iters*
   )
  (dis "=========  INTEGER RANGES COMPUTED :" dnl)
  (display-the-ranges)
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

(define gtr-dt #f)

(define (get-type-range declared-type)
  (set! gtr-dt declared-type)
  (if (not (integer-type? declared-type))
      (error "get-type-range : not an integer type : " declared-type))

  (let ((bitsiz (cadddr declared-type))
        (sint   (caddr  declared-type)))
    
    (cond ((null? bitsiz) *range-complete*)
          
          (sint        (make-sint-range bitsiz))
          (else        (make-uint-range bitsiz)))
    
    )
  )


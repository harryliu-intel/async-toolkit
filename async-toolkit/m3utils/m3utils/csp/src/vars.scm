;; (loaddata! "expressions_u")
;;(var-ass-range 'y *the-ass-tbl* *the-prt-tbl*)

(load "debug.scm")

(define (vars-dbg . x)
  (apply dis x)
  )

(define (make-assignments-tbl prog cell-info the-inits func-tbl struct-tbl)

  (define tbl (make-hash-table 100 atom-hash))
  
  (define (trace-ass-visitor s syms vals tg func-tbl struct-tbl _cell-info)

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
       
       (let* ((rhs       (get-assign-rhs s))
              (lhs       (get-assign-lhs s)))
         (if (not (equal? rhs lhs))
             ;; self-assignments don't count!
             ;; (Andrew adds these for some sort of synchronization)
             (add-entry! lhs)))
       )

      ((recv)
       (let ((lhs (get-recv-lhs s))
             (rhs (get-recv-rhs s)))
         (vars-dbg "recv     : " s dnl)
         (vars-dbg "recv lhs : " lhs dnl)
         (vars-dbg "recv rhs : " rhs dnl)

         (if (not (null? rhs)) (add-entry! rhs)))
       )

      ((assign-operate)  ;; should be transformed out
       (error))


      ((sequential-loop parallel-loop)
       (add-entry! `(id ,(get-loop-dummy s)))
       )
      
      
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
         (is-id (ident? (get-assign-lhs ass)))
         (is-const (constant-simple? rhs syms))) ;; #f or a list containing the value
    (and is-id is-const)))

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
  (vars-dbg "mark-decls-constant" dnl)
  (define (visitor s)
    (if (eq? 'var1 (get-stmt-type s))
        (if (member (get-var1-id s) constant-ids)
            (let ((res (make-var1-constant s)))
              (vars-dbg "mark-decls-constant " (get-var1-id s) " " res dnl)
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
  (dis "constantify-constant-vars" dnl)
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
;;      (vars-dbg "make-uses-tbl add-entry!  "  id " -> " cur-stmt dnl)
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

;;    (vars-dbg "s-visitor : s : " s dnl)
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
;;         (vars-dbg "recv     : " s dnl)
;;         (vars-dbg "recv lhs : " lhs dnl)
;;         (vars-dbg "recv rhs : " rhs dnl)

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
;;           (vars-dbg "s-visitor inam " inam dnl)
;;           (vars-dbg "s-visitor iargs " (stringify iargs) dnl)
;;           (vars-dbg "s-visitor ids " ids dnl)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (recv? x)(and (pair? x)(eq? 'recv (car x))))
(define (assign? x)(and (pair? x)(eq? 'assign (car x))))


(define (get-channel-type-bit-width channel-type)
  (if (or (not (eq? 'channel             (car channel-type)))
          (not (eq? 'standard.channel.bd (cadr channel-type))))
      (error "not a channel type I understand : " channel-type))
  (caaddr channel-type))

(define *ar-ass* #f)

(load "bits.scm")

(define (assignment-range ass port-tbl)
  ;; compute the range of an assignment
  ;; the "ass" should be in the format of the ass-tbl,
  ;; that is: (ass syms id vals)

  (set! *ar-ass* ass)

  (define syms (cadr   ass))
  (define id   (caddr  ass))
  (define vals (cadddr ass))

  (vars-dbg "assignment-range syms " syms dnl)
  (vars-dbg "assignment-range id   " id   dnl)
  (vars-dbg "assignment-range vals " vals dnl)

  (define (channel-value-range channel-designator)
    (let* ((id        (get-designator-id channel-designator))
           (chan-decl (port-tbl 'retrieve id))
           (width     (get-channel-type-bit-width (cadddr chan-decl))))

      (vars-dbg "cvr : id : " id dnl)
      (vars-dbg "cvr : cd : " chan-decl dnl)

      (make-uint-range width)
      
      )
    )

  (define (loop-range loop-stmt)
    (let* ((the-range (get-loop-range loop-stmt))
           (min-expr  (cadr the-range))
           (max-expr  (caddr the-range)))
      (vars-dbg "loop-range " loop-stmt dnl)
      (make-range (range-min (expr-range min-expr syms))
                  (range-max (expr-range max-expr syms)))))
      

  (define (get-bits-interval bs)

    (vars-dbg "get-bits-interval : bs : " bs dnl)
    (let* ((the-index-range (range-union (expr-range (get-bits-min bs) syms)
                                         (expr-range (get-bits-max bs) syms)))
           (the-range
            (range-extend  ;; this is a little bit pessimistic
             (range-- (range-<< *range1* the-index-range) *range1*)
             *big0*)))

      (vars-dbg "get-bits-interval : the-index-range : " the-index-range dnl)
      (vars-dbg "get-bits-interval : the-range       : " the-range dnl)

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

    (vars-dbg "assignment-range ass-stmt   " ass-stmt dnl)
    (vars-dbg "assignment-range is-recv    " is-recv dnl)
    (vars-dbg "assignment-range is-assn    " is-assn dnl)
    (vars-dbg "assignment-range is-loop    " is-loop dnl)
    (vars-dbg "assignment-range simple-rhs " simple-rhs dnl)
    
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
    (dis "var-ass-range : var-asses  : " var-asses dnl)
    (dis "var-ass-range : var-ranges : " var-ranges dnl)
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


(define (expr-range expr syms)
  ;; should remove globals here...
  (vars-dbg "expr-range : expr : " (stringify expr) dnl)
  
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
  (apply range-union (var-ass-range id *the-ass-tbl* *the-prt-tbl*))
  )

(define (update-id-range! id rng-tbl)
  (dis "update-id-range! " id dnl)
  
  (let*((old-range (rng-tbl 'retrieve id))

        (dcl-range (*the-dcl-tbl* 'retrieve id))

        (id-range  (get-id-range id))
        
        (new-range
         (if (eq? dcl-range '*hash-table-search-failed*)
             id-range
             (range-intersection dcl-range id-range))
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

(define (iterate-until-false f)
  (let ((res (f)))
    (if res (iterate-until-false f) res)))

(define (the-typed-ids)
   (uniq eq?
         (append (*the-dcl-tbl* 'keys)
                 (*the-rng-tbl* 'keys)
                 *the-loop-indices*)))

(define (display-the-ranges)
  (display-tbl *the-rng-tbl* (the-typed-ids)))

(define (display-tbl tbl keys)
  (map
   (lambda(id)
     (dis (pad 40 id) " : " (tbl 'retrieve id) dnl))
   keys)
  'ok
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-port-table cell-info)

  (define tbl (make-hash-table 100 atom-hash))
  
  (map (lambda(p)(tbl 'add-entry! (cadr p) p)) 
       (get-ports cell-info))

  tbl

)

(define (get-all-loop-indices prog)
  (map cadr
       (apply append
              (map (lambda(stype)(find-stmts stype prog))
                   '(sequential-loop parallel-loop)))))
  
(define (make-the-tables prog)
  (set! *the-ass-tbl* (make-assignments-tbl
                       prog *cellinfo* *the-inits* '() *the-struct-tbl*))
  (set! *the-use-tbl* (make-uses prog))
  (set! *the-dcl-tbl* (make-intdecls prog))            ;; declared ranges
  (set! *the-rng-tbl* (make-hash-table 100 atom-hash)) ;; derived ranges
  (set! *the-prt-tbl* (make-port-table *cellinfo*))
  (set! *the-loop-indices* (get-all-loop-indices prog))
  (set! *the-global-ranges* (make-global-range-tbl *the-globals*))
  'ok
  )

(define (close-integer-ranges!)
  (iterate-until-false (lambda()(update-id-ranges! *the-rng-tbl*)))
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

                 (tbl 'add-entry! id (get-type-range ty))))
       )

      ;; we need to handle the loops separately, since we know
      ;; more about their ranges than we do about declared variables
      ;; (which we only know widths for)
      
      );;esac
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

(define (make-uint-type range)
  (if (range-infinite? range)
      *default-int-type*
      (let ((bits (xnum-clog2 (xnum-+ (range-max range) *big1*))))
        (make-integer-type #f bits))))

(define (make-sint-type range)
  (if (range-infinite? range)
      *default-int-type*
      (let* ((max (range-max range))
             (min (range-min range))
             
             (max0
              (if (xnum-< min *big0*)
                  (xnum-- (xnum-abs min) *big1*) ;; need one less value for neg
                  (xnum-abs min)))
             
             (max1 (xnum-abs max))

             (mmax (xnum-max max0 max1))

             (bits (xnum-+ *big1* (xnum-clog2 (xnum-+ mmax *big1*)))))
        (make-integer-type #t bits))))


(define (make-proposed-type-tbl)
  (let ((tbl (make-hash-table 100 atom-hash)))
    (map
     (lambda(id)
       (tbl 'add-entry! id
            (get-smallest-type
             (*the-rng-tbl* 'retrieve id))))
     (*the-rng-tbl* 'keys))
    tbl
    )
)

(define (propose-types!)
  (set! *proposed-types* (make-proposed-type-tbl))
  (dis "=========  INTEGER TYPES DERIVED :" dnl)
  (display-tbl *proposed-types* (*proposed-types* 'keys))
 )

         
                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-uninitialized-uses seq)
  (let loop ((p               (cdr seq))
             (declared        '())
             (assigned        '())
             (used-early      '()))
;;    (dis "visit-sequence loop p : " p dnl)
    (cond ((null? p)
           (dis "end of sequence : " dnl
                "declared        : " declared dnl
                "assigned        : " assigned dnl
                "used-early      : " used-early dnl)

           ;; a variable is suspicious if it has been used before
           ;; being assigned to or it has been declared but not assigned
           ;; to by end of sequence
           
           (set-union used-early (set-diff declared assigned)))

          ((eq? 'var1 (get-stmt-type (car p)))
           (loop (cdr p)
                 (cons (get-var1-id (car p)) declared)
                 assigned
                 used-early))

          ((eq? 'assign (get-stmt-type (car p)))
           (loop (cdr p)
                 declared
                 (cons (get-designator-id (get-assign-lhs (car p))) assigned)
                 used-early))

          ((eq? 'recv (get-stmt-type (car p)))
           (loop (cdr p)
                 declared
                 (cons (get-designator-id (get-recv-rhs (car p))) assigned)
                 used-early))

          (else
           (let* ((cur-refs (find-referenced-vars (car p)))
                  (unassigned (set-diff cur-refs assigned)))
             (loop (cdr p)
                   declared
                   assigned
                   
                   (if (null? unassigned)
                       used-early
                       (begin
                         (dis "used early : " unassigned dnl)
;;                         (dis "used early in : " (car p) " : " unassigned dnl)
                         (append unassigned used-early))
                       );;fi
                   );;pool
             );;*tel
           );;esle
         );;dnoc
    );;tel
  );;enifed

(define (initialize-sequence seq vars)
  (let loop ((p       (cdr seq))
             (output '()))
    (cond ((null? p) (cons 'sequence (reverse output)))

          ((eq? 'var1 (get-stmt-type (car p)))
           (let ((tgt (get-var1-id (car p))))
             (if (member tgt vars)
                 (loop (cdr p)
                       (cons `(assign (id ,tgt) ,*big0*)
                             (cons (car p)
                                   output)))

                 (loop (cdr p)
                       (cons (car p) output)));;fi
             );;tel
           )

          (else (loop (cdr p) (cons (car p) output))))))
  
(define (patch-uninitialized-uses prog)

  (define (visit-s s)
    (if (eq? 'sequence (get-stmt-type s))
        (let ((uninit-uses (search-uninitialized-uses s)))
          (if (null? uninit-uses)
              s
              (initialize-sequence s uninit-uses)))
        s))

  (visit-stmt prog visit-s identity identity)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-global-range-tbl global-tbl)
  (define tbl (make-hash-table 100 atom-hash))

  (define (insert-one-range! id)
    (dis "making global range for : " id dnl)
    
    (let ((r
           (apply range-union
                  (map make-point-range ((global-tbl 'retrieve id) 'values)))))
      (tbl 'add-entry! id r)))

  (map insert-one-range! (*the-globals* 'keys))
  tbl
  )


    

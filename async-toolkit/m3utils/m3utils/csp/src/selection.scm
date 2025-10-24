;;(dis "loading selection.scm" dnl)

(define (selection-dbg . x)
;;  (apply dis x)
  )


(define msi-selection #f)

(define (make-selection-implementation selection func-tbl cell-info)

  (set! msi-selection selection)
  
  (let ((kw (get-stmt-type selection)))

    (if (not (member kw '(if nondet-if)))
        (error "Not a selection : " selection))

    (let* ((gcs    (get-guarded-gcs selection))

           (guards (map car gcs))

           (sensitivities (map (lambda(x)
                                 (expr-sensitivity x func-tbl cell-info))
                               guards))

           (last-guard-is-else (eq? 'else (last guards)))
           
           (local
            ;; an if is local if either 
            ;; (1) the last guard is else or
            ;; (2) none of the guards depend on any interface object
            (or last-guard-is-else 
                (null? (apply append sensitivities)))))

      (if #f (begin
      (selection-dbg "make-selection-implementation : simple        : "
           simple dnl)
      (selection-dbg "make-selection-implementation : guards        : "
           (stringify guards) dnl)
      (selection-dbg "make-selection-implementation : sensitivities : "
           (stringify sensitivities) dnl)
      (selection-dbg "make-selection-implementation : local         : "
           local dnl)
      ))

      (cond
       (local  (implement-local-if selection func-tbl cell-info))

       (else 
        (implement-waiting-if selection func-tbl cell-info)))
          
      )
    )
  )

(define (implement-local-if selection func-tbl cell-info)
  ;; this needs to just compute each of the guards sequentially,
  ;; then call the if on the newly generated booleans

  (let* ((gcs    (get-guarded-gcs selection))
         
         (guards (map car gcs))

         (commands (map cadr gcs))

         (tg     (make-name-generator "implement-local-if"))
         
         (vars   (map (lambda(g)(if (eq? g 'else) 'else (tg 'next))) guards))

         (decls  (map (lambda(nm)
                        (if (eq? nm 'else)
                            'skip
                            (make-var1-decl nm *default-boolean-type*))) vars))

         (assigns (map (lambda(nm guard)
                         (if (eq? guard 'else)
                             'skip
                             (make-assign `(id ,nm) guard)))
                       vars guards))

         (rem-if (cons 'local-if (map (lambda(nm cmd)
                                        (list
                                         (if (eq? nm 'else) 'else `(id ,nm))
                                         cmd))
                                      vars commands)))

         (the-result
          `(sequence ,@decls ,@assigns ,rem-if)))

    (selection-dbg "implement-local-if start     : " (stringify selection) dnl)
    (selection-dbg "implement-local-if guards    : " (stringify guards) dnl)
    (selection-dbg "implement-local-if commands  : " (stringify commands) dnl)
    (selection-dbg "implement-local-if assigns   : " (stringify assigns) dnl)
    (selection-dbg "implement-local-if rem-if    : " (stringify rem-if) dnl)
    (selection-dbg "implement-local-if result    : " (stringify the-result) dnl)
    
    the-result

    )
  )

(define (waiting-if? s)
  (and (pair? s) (eq? 'waiting-if (car s))))

(define *wif* #f)

(define (implement-waiting-if selection func-tbl cell-info)
  ;; here we will implement the selection as a waiting-if
  ;;
  ;; the format of a waiting-if is
  ;;
  ;; (waiting-if
  ;;    (<dummy_0>   <sensitivity_0>   <guardeval_0>   <command0>)
  ;;    (<dummy_1>   <sensitivity_1>   <guardeval_1>   <command1>)
  ;;    ...
  ;;    (<dummy_n-1> <sensitivity_n-1> <guardeval_n-1> <command_n-1>)
  ;; )
  ;;
  ;; where
  ;;   dummy_i       is a new identifier for the ith GC
  ;;   sensitivity_i is the sensitivity set for guard_i
  ;;   guardeval_i   is a statement that evaluates the guard and places 
  ;;                 the result in dummy_i
  ;;

  (selection-dbg "implement-waiting-if : " selection dnl)

  ;; because of the definition of a local if, there can be no "else"
  ;; clause in a waiting if.
  
  (let* ((gcs    (get-guarded-gcs selection))
        
         (guards (map car gcs))

         (commands (map cadr gcs))

         (tg     (make-name-generator "implement-waiting-if"))
         
         (vars   (map (lambda(g)(tg 'next)) guards))

         (sensitivities
          (map (lambda(g)(expr-sensitivity g func-tbl cell-info))
               guards))

         (assigns (map (lambda(nm guard) (make-assign `(id ,nm) guard))
                       vars guards))

         (table 
          (map (lambda(dummy sensitivity guardeval command)
                 (list dummy sensitivity guardeval command))
               vars sensitivities assigns commands))

         (res (cons 'waiting-if table))
         )

    
    (selection-dbg "implement-waiting-if guards        : " (stringify guards) dnl)
    (selection-dbg "implement-waiting-if commands      : " (stringify commands) dnl)
    (selection-dbg "implement-waiting-if vars          : " (stringify vars) dnl)
    (selection-dbg "implement-waiting-if sensitivities : " (stringify sensitivities) dnl)
    (selection-dbg "implement-waiting-if assigns       : " (stringify assigns) dnl)  
    (selection-dbg "implement-waiting-if table         : " (stringify table) dnl)
    (selection-dbg "implement-waiting-if res           : " (stringify  res) dnl)
    ;;(error)
    (set! *wif* res)
    res
    )
  )

(define (get-waiting-if-clauses wif) (cdr wif))

(define (get-waiting-if-clause-dummy wifc) (car wifc))

(define (get-waiting-if-clause-sensitivity wifc) (cadr wifc))

(define (get-waiting-if-clause-guardeval wifc) (caddr wifc))

(define (get-waiting-if-clause-command wifc) (cadddr wifc))

(define (get-waiting-if-dummies prog)
  ;; multiset
  (define ids '())
  
  (define (s-visit s)
    (if (waiting-if? s)
        (set! ids (append
                   (map get-waiting-if-clause-dummy (get-waiting-if-clauses s))
                   ids))
        )
    s
    )
  
  (visit-stmt prog s-visit identity identity)
  ids
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; convert-waiting-if:
;;
;;
;; convert waiting-if to if/loop/goto version
;; this is, well, an optional module, because we could leave
;; the implementation to the code generator, or implement it
;; differently.
;;
;; See the document:
;; A proposal for the semantics of selection ([...]) in Fulcrum-CSP
;;

(define (convert-waiting-if-nonarray wif)
  ;; V1 : use scalar booleans
  
  (let* ((tg           (make-name-generator "convert-waiting-if"))
         (wif-clauses  (get-waiting-if-clauses wif))
         (dummies      (map get-waiting-if-clause-dummy wif-clauses))

         (wif-clauses-decls
                       (map (lambda(d) (make-var1-decl d
                                                       *default-boolean-type*))
                            dummies))
         
         (sensit       (uniq eq?
                             (apply append
                                    (map get-waiting-if-clause-sensitivity
                                         wif-clauses))))
         (guardeval    (cons 'sequence
                             (map get-waiting-if-clause-guardeval
                                  wif-clauses)))

         (lif-clauses  (map (lambda(wif-cl)
                              `((id ,(get-waiting-if-clause-dummy wif-cl))
                                ,(get-waiting-if-clause-command wif-cl)))
                            wif-clauses))

         (wif-clauses-lif
                      `(local-if ,@lif-clauses))
                                        
         ;; need to make the or

         (g-list       (map make-ident dummies))
         (or-expr      (make-binop '| g-list)) ; |))
         (nor-expr    `(not ,or-expr))
         ;; the OR/NOR expression
         
         (ndone        (tg 'next 'not-done-))

         (ndone-id     (make-ident ndone))
         (ndone-decl   (make-var1-decl ndone *default-boolean-type*))
         ;; declare the "not done" variable
         
         (ndone-init   (make-assign ndone-id #t))
         (ndone-assign (make-assign ndone-id nor-expr))

         (lock         `(lock ,@sensit))
         (unlock       `(unlock ,@sensit))
         (exec-unlock  `(sequence ,unlock ,wif-clauses-lif))
         (waitfor      `(waitfor ,@sensit))
         (done-lif     `(local-if (,ndone-id ,waitfor)
                                  (else ,exec-unlock)))

         (the-loop     `(while ,ndone-id
                               (sequence
                                 ,@wif-clauses-decls
                                 ,lock
                                 ,guardeval
                                 ,ndone-assign
                                 ,done-lif)))
         (res          `(sequence ,ndone-decl
                                  ,ndone-init
                                  ,the-loop))
         )

    res

    )
  )
(define (convert-waiting-if-array wif last)
  ;; V2 : use array booleans to allow fair implementation
  
  (let* ((tg           (make-name-generator "conv-wif"))
         (wif-clauses  (get-waiting-if-clauses wif))
         (dummies      (map get-waiting-if-clause-dummy wif-clauses))

         (wif-clauses-decls
                       (map (lambda(d) (make-var1-decl d
                                                       *default-boolean-type*))
                            dummies))

         (n            (length wif-clauses))

         (selected     (tg 'next))
         (seldecl      (make-var1-decl selected *default-int-type*))
         (selinit      `(assign (id ,selected) ,(bn -1)))
         (arrnm        (tg 'next 'arr))
         (arr-rng      `(range ,*big0* ,(bn (- n 1))))
         (arrdecl      (make-var1-decl arrnm
                                       `(array ,arr-rng
                                               ,*default-boolean-type*)))
         (arridxs      (map bn (count-execute n identity)))
         (arr          (map (lambda(idx)`(array-access (id ,arrnm) ,idx))
                            arridxs))

         (arrasses     (map (lambda(a g)`(assign ,a (id ,g)))
                            arr
                            dummies))

         (loop-dummy   (tg 'next))
         (sel-ass-loop `(sequential-loop
                         ,loop-dummy
                         ,arr-rng
                         (if
                          ((array-access (id ,arrnm) (% (+ (id ,loop-dummy)
                                                           (id ,last))
                                                        ,(bn n)))
                           (sequence
                             (assign (id ,selected)
                                     (% (+ (id ,loop-dummy)
                                           (id ,last))
                                        ,(bn n)))
                             (assign (id ,last) (id ,selected))))))
                       )
                       
                         
         (sensit       (uniq eq?
                             (apply append
                                    (map get-waiting-if-clause-sensitivity
                                         wif-clauses))))
         (guardeval    (cons 'sequence
                             (map get-waiting-if-clause-guardeval
                                  wif-clauses)))

         (lif-clauses  (map (lambda(wif-cl arridx)
                              `((== (id ,selected) ,arridx)
                                ,(get-waiting-if-clause-command wif-cl)))
                            wif-clauses arridxs))

         (wif-clauses-lif
                      `(if ,@lif-clauses))
                                        
         ;; need to make the or

         (g-list       (map make-ident dummies))
         (or-expr      (make-binop '| g-list)) ; |))
         (nor-expr    `(not ,or-expr))
         ;; the OR/NOR expression
         
         (ndone        (tg 'next 'not-done-))

         (ndone-id     (make-ident ndone))
         (ndone-decl   (make-var1-decl ndone *default-boolean-type*))
         ;; declare the "not done" variable
         
         (ndone-init   (make-assign ndone-id #t))
         (ndone-assign (make-assign ndone-id nor-expr))

         (lock         `(lock ,@sensit))
         (unlock       `(unlock ,@sensit))
         (exec-unlock  `(sequence ,unlock ,wif-clauses-lif))
         (waitfor      `(waitfor ,@sensit))
         (done-lif     `(if ((== (id ,selected) ,*bigm1*) ,waitfor)
                            (else ,exec-unlock)))

         (the-loop     `(while ,ndone-id
                               (sequence
                                 ,@wif-clauses-decls
                                 ,seldecl
                                 ,selinit
                                 ,arrdecl
                                 ,lock
                                 
                                 ,guardeval
                                 ,@arrasses
                                 ,sel-ass-loop
                                 ,ndone-assign
                                 ,done-lif)))
         
         (res          `(sequence ,ndone-decl
                                  ,ndone-init
                                  ,the-loop))
         )

    (dis "dummies    : " dummies dnl)
    (dis "arrnm      : " arrnm dnl)
    (dis "arrasses   : " arrasses dnl)
    
    res

    )
  )

(define *cwif* #f)

(define (convert-waiting-if wif   ;; statement in S-format
                            last  ;; symbol name of last state storage
                            )
  (set! *cwif* wif)
  (let ((res (convert-waiting-if-array wif last)))
    (dis "convert-waiting-if : res : " dnl)
    (pp res)
;;    (error)
    res
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-waiting-ifs prog)
  (define tg (make-name-generator "wif-state"))

  (define (lasts '()))
    
  (define (newlast)
    (let ((lnm (tg 'next)))
      (set! lasts (cons lnm lasts))
      lnm
      )
    )
    
  (define (visitor stmt)
    (if (waiting-if? stmt)
        (convert-waiting-if stmt (newlast))
        stmt))

  (let ((body   (visit-stmt prog visitor identity identity))
        
        (lastdecls
         (map (lambda(id)(make-var1-decl id *default-int-type*))
              lasts))
        (lastasses
         (map (lambda(id)`(assign (id ,id) ,*big0*))
              lasts))
        )
    `(sequence ,@lastdecls ,@lastasses ,body))
  )

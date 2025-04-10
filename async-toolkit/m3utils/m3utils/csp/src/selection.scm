(dis "loading selection.scm" dnl)

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
      (dis "make-selection-implementation : simple        : "
           simple dnl)
      (dis "make-selection-implementation : guards        : "
           (stringify guards) dnl)
      (dis "make-selection-implementation : sensitivities : "
           (stringify sensitivities) dnl)
      (dis "make-selection-implementation : local         : "
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

    (dis "implement-local-if start     : " (stringify selection) dnl)
    (dis "implement-local-if guards    : " (stringify guards) dnl)
    (dis "implement-local-if commands  : " (stringify commands) dnl)
    (dis "implement-local-if assigns   : " (stringify assigns) dnl)
    (dis "implement-local-if rem-if    : " (stringify rem-if) dnl)
    (dis "implement-local-if result    : " (stringify the-result) dnl)
    
    the-result

    )
  )

(define (waiting-if? s)
  (and (pair? s) (eq? 'waiting-if (car s))))

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

  (dis "implement-waiting-if : " selection dnl)

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

    
    (dis "implement-waiting-if guards        : " (stringify guards) dnl)
    (dis "implement-waiting-if commands      : " (stringify commands) dnl)
    (dis "implement-waiting-if vars          : " (stringify vars) dnl)
    (dis "implement-waiting-if sensitivities : " (stringify sensitivities) dnl)
    (dis "implement-waiting-if assigns       : " (stringify assigns) dnl)  
    (dis "implement-waiting-if table         : " (stringify table) dnl)
    (dis "implement-waiting-if res           : " (stringify  res) dnl)
    


    res
    )
  )

(define (get-waiting-if-clauses wif) (cdr wif))

(define (get-waiting-if-clause-dummy wifc) (car wifc))

(define (get-waiting-if-clause-sensitivity wifc) (cadr wifc))

(define (get-waiting-if-clause-guardeval wifc) (caddr wifc))

(define (get-waiting-if-clause-command wifc) (cadddr wifc))

(define (get-waiting-if-dummies prog)

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
  (uniq eq? ids)
  )



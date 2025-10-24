(require-modules "struct" "set")


(define variable-state
  (make-struct-type
   'variable-state
   `((initial-expression      ()              )
     (done                    #f              )
     (current-expression      ()              )

     ;;;;;;;;;;   MINIMIZATION VARIABLES BELOW HERE   ;;;;;;;;;;
     (working-expression      ()              )
     ;; working expression for the minimization

     (dependents               ()              )
     ;; direct dependents

     (current-limits          ()              )
     ;; current limits, enforced by depended-on variables
     ;; during recalc, this is what is updated

     (current-value           ()              )
     ;; current value of optimization variable (if any)

     (compiled-formula        ()              )
     ;; formula to evaluate for current value of limits
     ;; note this is NOT the same as the value of the optimization
     ;; variable (which can be on a 0..1 range), but the value of
     ;; the constraints for the value of the system variable.

     (topological-index       ()              )
     ;; index in the topological sort.  mainly used for assertions
     ;; (about things like evaluation order)

     (compiled-derivatives    ()              )
     ;; association list containing compiled expressions for 
     ;; calculating the partial derivatives, keyed by the denominator
     ;; differential variables

     (current-partial-values  ()              )
     ;; association list containing current values of compiled-derivatives

     (total-derivative        ()              )
     ;; dTARGET / dSELF

     (fixed                   #f              )
     ;; some variables cannot be changed

     (evaluation-order        ()              )
     ;; tail of the evaluation-order starting from me

     )))

(define (apply-everywhere ops root . y-only)
  ;;
  ;; apply one of a list of operations (or none) at every node of
  ;; the expression tree
  ;;
  ;; the operations are specified in an association list thus:
  ;; `((tag . ,tag-op) ...)
  ;;
  (define (operator type)
    (cond ((assoc type ops) => cdr) (else (lambda(x) x))))

  (define (operate1 expr) ((operator (car expr)) expr))

  (define (recurse-impl arg)
    (let ((x (operate1 arg))) ;; operate at high level before low
      (operate1
       (case (car x)
         ((pair)
          (if (null? y-only)
              (list 'pair   (recurse (cadr x)) (recurse (caddr x)))
              (list 'pair            (cadr x)  (recurse (caddr x))) )
         )

         ((range) x)
         
         ((+ - *)
          (cons (car x) (map recurse (cdr x))))

         ((softmax d-softmax-dc d-softmax-dx d-softmax-dy)
          (list (car x) (recurse (cadr x))
                        (recurse (caddr x))
                        (recurse (cadddr x))))
         
         ((select)
          (list 'select   (map recurse (cadr x)) (map recurse (caddr x))))

         ((diff)
          (list 'diff (cadr x) (caddr x)))

         ((frompair)
          (list 'frompair (recurse (cadr x)) (caddr x)))
         
         ((func func-prime)
          (list (car x)     (cadr x) (recurse (caddr x))))
         
         ((constant literal) x)
         
         (else (error "Unknown R : " x))))))

  (define recurse  (eq?-memo recurse-impl))

  (recurse root))

(define (simplify-select-1 x)
  (if (= 0 (length (cadr x))) (error "empty select")
      (if (= 1 (length (cadr x))) (caaddr x) x)))

(define (list-members-equal? lst)
  (let ((first (car lst)))
    (not    
     (member? #f 
              (map (lambda(x)(equal? first x)) (cdr lst))))))

(define (simplify-select-2 x)
  (if (list-members-equal? (caddr x)) 
      (caaddr x)
      x))

(define (simplify-select-3 x)
  (let loop ((bp    (cadr x))
             (vp    (caddr x))
             (prevb '())
             (prevv '()))

  ;;  (dis "(loop " (stringify bp) " " (stringify vp) " " (stringify prevb) " " (stringify prevv) ")" dnl)

    (if (null? bp)
        (list 'select prevb prevv)
        
        (let* ((the-others   (append (cdr bp) prevb))
               (domination   (map (lambda(y)(dominates? max y (car bp)))
                                  the-others)))
          (if (and (not (member? #f domination)) (member? #t domination))
              (loop (cdr bp)(cdr vp) 
                    prevb prevv)
              (loop (cdr bp)(cdr vp) 
                    (cons (car bp) prevb) (cons (car vp) prevv)))))))

(define (simplify-select x) 
  (let loop ((res  x)
             (ops 
              (list simplify-select-3 simplify-select-2 simplify-select-1)))

    (cond ((null? ops)                   res)
          ((not (eq? 'select (car res))) res)
          (else (loop ((car ops) res) (cdr ops))))))
         

(define (simplify-frompair x)
  (let ((p (cadr x)))
    (if (eq? (car p) 'pair)
        (case (caddr x)  ((X) (cadr p))  ((Y) (caddr p))  (else (error)))
        x)))

(define (insert-element elem lst less-than?)
  (cond ((null? lst) (list elem))
        ((less-than? (car lst) elem)
         (cons (car lst) 
               (insert-element elem (cdr lst) less-than?)))
        (else (cons elem lst))))

(define (stablesort lst less-than?)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((rest (stablesort (cdr lst) less-than?)))
        (insert-element (car lst) rest less-than?))))

(define (sort-+-cdr clst)
  ;; stable sort needed so we dont spuriously change rep
  ;; because mergesort reads left to right it's unstable unless <=
  ;; is used.
  (stablesort clst 
              (lambda(a b)(string<? (symbol->string (car a)) 
                                    (symbol->string (car b))))))

(define (flatten-op op x)
  ;; just flatten
  (cons op
        (sort-+-cdr
         (apply append
                (map (lambda(y) 
                       (if (eq? op (car y)) 
                           (cdr y)
                           (list y)))
                     (cdr x))))))

(define (remove-identities identity x)
  (cons (car x) (filter (lambda(t)(not (equal? identity t))) (cdr x))))

(define (check-short op identity x)
  ;; handle expressions with 0 or 1 terms
  (cond ((not (equal? (car x) op)) x)
        ((null? (cdr x) ) identity)
        ((null? (cddr x)) (cadr x))
        (else             x)))

(define (simplify-+ x) 
  ;;(dis "(simplify-+ "(stringify x)")" dnl)
  (check-short '+ '(constant 0)
               (simplify-++ x)))


(define (remove---identities x)
  (cond ((not (eq? (car x) '-))  (error "not -"))
        ((< 3 (length x))        x              )
        (else
         (append (head 2 x)
                 (filter (lambda(t)(not (equal? '(constant 0) t)))
                         (cddr x))))))

(define (simplify-- x)
  (check-short '- '(constant 0) (remove---identities x)))

(define (simplify-- x)
  (if (null? (cdr x)) 
      '(constant 0)
      `(+ ,(cadr x) ,@(map (lambda(t)`(* (constant -1) ,t)) (cddr x)))))

(define (simplify-++ x) 
  ;; this one leaves in canonical + notation (used by dominates?)
  (flatten-op    '+ 
                 (remove-identities '(constant 0) x)))


(define (simplify-* x)
  (check-short '* '(constant 1)
   (cond ((member? '(constant 0) (cdr x)) '(constant 0))
         (else (remove-identities '(constant 1) x)))))

(define (simplify-diff x)
  (if (eq? (cadr x) (caddr x)) '(constant 1) '(constant 0)))

(define simplify-step-x '())

(define (simplify-step x)
  (set! simplify-step-x x)
  ;;(dis "simplify-step..." dnl)
  (apply-everywhere 
   `((select   . ,simplify-select) 
     (+        . ,simplify-+)
     (-        . ,simplify--)
     (*        . ,simplify-*)
     (diff     . ,simplify-diff)
     (frompair . ,simplify-frompair))
   x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (combine-ranges . x)
  (let ((v (apply append (map cdr x))))
    (list 'range (apply min v) (apply max v))))

(define (range-surely>? r1 r2)
  (> (apply min (cdr r1)) (apply max (cdr r2))))

(define (opposite-extremum op)
  (cond ((eq? op max) min)
        ((eq? op min) max)
        (else (error "not an extremum fxn " op))))

(define (constant->range x) (list 'range (cadr x) (cadr x)))

(define (bag-intersection k l)
  (let kloop ((p    k)
              (q    l)
              (res '()))
    (if (null? p) 
        res
        (let lloop ((r    q)
                    (skip '()))
          
          (cond ((null? r) (kloop (cdr p) q res))
                ((equal? (car p) (car r))
                 (kloop (cdr p) (append skip (cdr r)) (cons (car p) res)))
                (else (lloop (cdr r) (cons (car r) skip))))))))
              
(define (bag-symmetric-diffs k l)
  (let kloop ((p    k)
              (q    l)
              (res '()))
    (if (null? p) 
        (list res q)
        (let lloop ((r    q)
                    (skip '()))
          
          (cond ((null? r) (kloop (cdr p) q (cons (car p) res)))
                ((equal? (car p) (car r))
                 (kloop (cdr p) (append skip (cdr r))  res))
                (else (lloop (cdr r) (cons (car r) skip))))))))
              
(define (dominates++? op x y)
  (if (not (eq? (car x) '+)) (error "x not +"))
  (if (not (eq? (car y) '+)) (error "y not +"))
  
  (let* ((diffs (bag-symmetric-diffs (extremum (opposite-extremum op)  x) 
                                     (extremum op y)))
         (xx (sort-+-cdr (car diffs)))
         (yy (sort-+-cdr (cadr diffs)))
         (xx-constant (assoc 'constant xx))
         (yy-constant (assoc 'constant yy)))

    ;;(dis "xx " (stringify xx) dnl)
    ;;(dis "yy " (stringify yy) dnl)

    (or 
     (and (eq? op max)
          (or (null? yy)
              (and
               (equal? yy `((constant ,(cadar yy))))
               xx-constant
               (> (cadr xx-constant) (cadar yy)))))
     
     (and (eq? op min)
          (or (null? xx)
              (and
               (equal? xx `((constant ,(cadar xx))))
               yy-constant
               (> (cadr yy-constant) (cadar xx)))))
     ) ;; ro
))
     

(define debug-op '())
(define debug-x '())
(define debug-y '())


(define (dominates? op x y)
  (case (car x)
    ((+)     (if (eq? (car y) '+) 
                 (dominates++? op x y) 
                 (dominates? op x (list '+ y))))

    
    ((pair)  (if (eq? (car y) 'pair)
                 (and (dominates? op (cadr x) (cadr y))
                      (dominates? op (caddr x) (caddr y)))
                 #f))

    ((range frompair func constant literal)  ;; scalars in general
     (dominates? op (simplify-++ (list '+ x)) (simplify-++ (list '+ y))))

    ((select)
     (let ((xx  (extremum (opposite-extremum op) x)))
       (if (eq? (car xx) 'select)
           #f ;; looping
           (dominates? op  xx (extremum op y)) ;; xx is not select...
           
           )))

    (else (error "unknown type " (car x)))))


     
     
;;(define single-extremum-args '())

(define (single-extremum op x recurse)
  ;;(set! single-extremum-args (list op x recurse))
  ;;(dis "single-extremum " (stringify single-extremum-args) dnl)
  (case (car x)

      ((range) (list 'constant (apply op (cdr x))))

      ((+ pair) (cons (car x) (map recurse (cdr x))))

      ((frompair) (list 'frompair (recurse (cadr x)) (caddr x)))

      ((select) (list 'select (cadr x) (map recurse (caddr x))))

      (else x)))

(define (extremum op x)
  (let ((recurse (lambda(z) (extremum op z))))
    (single-extremum op x recurse)
))


(define (make-func-evaluator op rf)
  (let* ((fo   (modula-type-op 'ArithRep.RFunc 'get-field rf 'f))

         (ev   (lambda(x)
                 (let ((xr ((obj-method-wrap fo 'ArithR.F) op x)))
                   (ArithRep.ToScheme xr)))
               ))
    ev
))


(define (eval-func-op op lst)
  ;; (func <RFunc obj> <arg>)
  (if (not (eq? (car lst) 'func)) (error))
  (let ((ev (make-func-evaluator op (cadr lst)))
        (arg  (caddr lst)))

    (case (car arg) 
      ((constant) (ev (cadr arg)))
      ((range)
        (combine-ranges (ev (cadr arg)) (ev (caddr arg))))
      (else lst))))

(define (eval-func lst) (eval-func-op 'eval lst))

(define (eval-+ lst)
  ;; (+ <arg> ...)
  (let loop ((range      (list 0 0))
             (p          (cdr lst))
             (other      '()))
    (if (null? p)
        ;; return result
        (cond ((and (= 0 (car range)) (= 0 (cadr range)))
               (cons '+ (sort-+-cdr (reverse other))))
              ((= (car range) (cadr range))
               (cons '+ (sort-+-cdr (cons (list 'constant (car range))
                                          (reverse other)))))
              (else
               (cons '+ (sort-+-cdr (cons (cons 'range range)
                                          (reverse other))))))

        ;; more to go
        (case (caar p)
          ((constant)
           (let ((v (cadar p)))
             (loop (list (+ v (car range))
                         (+ v (cadr range)))
                   (cdr p)
                   other)))
          ((range)
           (let ((r (mergesort (cdar p) <)))
             (loop (map + r range)
                   (cdr p)
                   other)))
          (else
           (loop range (cdr p) (cons (car p) other)))))))

(define (eval-* lst)
  (let loop ((p                (cdr lst))
             (constant-factor          1)
             (rest                   '()))
    (cond ((null? p) `(* (constant ,constant-factor) ,@rest))
          ((eq? 'constant (caar p))
           (loop (cdr p) (* constant-factor (cadar p)) rest))
          (else (loop (cdr p) constant-factor (cons (car p) rest))))))

                 

(define (eval-select lst)
  ;; (select (<by0> ..) (<val0> ..) )
  ;; get rid of Reset (TheEpoch) unless its the only thing
    (let loop ((by  (cadr lst))
               (val (caddr lst))
               
               (newby '())
               (newval '()))
      (cond ((null? by)
             `(select ,(reverse newby) ,(reverse newval)))
            ((and (is-epoch? (car by))
                  (or (not (null? newby)) (not (null? (cdr by)))))
             (loop (cdr by) (cdr val) newby newval))
            (else 
             (loop (cdr by) (cdr val) 
                   (cons (car by) newby) (cons (car val) newval))))))

(define the-epoch (cadr (ArithRep.ToScheme (ArithR.TheEpoch))))

(define (is-epoch? expr)
  (if (not (eq? (car expr) '+)) #f
      (member? the-epoch 
               (map cadr 
                    (filter (lambda(x)(eq? (car x) 'literal)) (cdr expr))
                    ))))

        
                   
(define (eval-step x)
  ;;(dis "eval-step..." dnl)
  (apply-everywhere `((+ . ,eval-+)
                      (* . ,eval-*)
                      (select . ,eval-select)
                      (func . ,eval-func)) x))

(define (fixpoint op x)
  (let loop ((prv    x)
             (cur    (op x)))
    (if (equal? prv cur) 
        cur
        (loop cur (op cur)))))

(define (simplify-arith x) (fixpoint simplify-step x))

(define (eval-arith x) (fixpoint eval-step x))

(define (simplify-eval-step x) (eval-step (simplify-step x)))

(define (simplify-eval-arith x) (fixpoint simplify-eval-step x))
    

(define (expand-step tm x)
  (let ((done #f))

    (define (expand-literal l)
      (if done 
          ;; if we have a result already, we return it if its a match
          ;; else we just pass the buck
          (if (eq? (cadr l) (car done)) (cdr done) l)

          ;; no result yet, look up the literal, return if a match
          ;; and update done
          (let ((lookup-result 
                 (modula-type-op 'Circuit.CktTimingModel
                                 'call-method
                                 tm
                                 'getVar (list (cadr l)))))
            (if (not (null? lookup-result))
                (let ((retval (ArithRep.ToScheme lookup-result)))
                  (dis "expanded " (cadr l) dnl)
                  (set! done (cons (cadr l) retval))
                  retval)

                ;; lookup failed, pass the buck (this is a true unknown)
                l))))

    (apply-everywhere `((literal . ,expand-literal)) x #t)))

(define (expand-specific-literal tm x named)
  (let* ((search-literal `(literal ,named))
         (literal-value (lookup-literal search-literal)))
    (define (expand-literal ll)
      (if (equal? search-literal ll) literal-value ll))
    
    (apply-everywhere `((literal .,expand-literal)) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (expand-simplify-eval-step tm x)
  (eval-step (simplify-step (expand-step tm x))))

(define (expand-simplify-eval-arith tm x)
  (fixpoint (lambda(q)(expand-simplify-eval-step tm q)) x))

;; new approach, try a table

(define (make-symbol-hash-table size) (make-hash-table size Atom.Hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (symhash-memo f)
  (let ((ans (make-symbol-hash-table 100)))
    (lambda(x)
      (let ((old (ans 'retrieve x)))
        (if (eq? old '*hash-table-search-failed*)
            (let ((new (f x))) (ans 'update-entry! x new) new)
            old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define expressions '())

(define (setup-algebra-system!)
  (set! expressions (make-symbol-hash-table 1000)))

(setup-algebra-system!)

(define (define-expression named x)
  (let ((new (variable-state 'new)))
    (new 'set! 'initial-expression x)
    (new 'set! 'current-expression x)
    (expressions 'update-entry! named new)))

(define (update-expression named x)
  ((expressions 'retrieve named) 'set! 'current-expression x))

;;(define-expression 'zz zz)

(define (lookup-literal l)
  (let ((lookup-result 
         (modula-type-op 'Circuit.CktTimingModel
                         'call-method
                         tm
                         'getVar (list (cadr l)))))
    (if (not (null? lookup-result))
        (ArithRep.ToScheme lookup-result)
        l)))

(define (system-step start)
  (let ((cur-entry (expressions 'retrieve start)))
    (if (eq? cur-entry '*hash-table-search-failed*)
           ;; no record, find it from environment and insert
           (let ((initial-lookup (lookup-literal `(literal ,start))))
             (define-expression start initial-lookup)
             initial-lookup)
           (cur-entry 'get 'current-expression))))

(define debug-l '())

(define (initialize-literals x)
  ;; grab the literals from x and add them to the system
  (define (initialize-literal l) 
    (set! debug-l l)
    (system-step (cadr l)))
  (apply-everywhere `((literal . ,initialize-literal)) x)
  #t
)
  
(define (contained-literals x)
  ;; result may include itself
  (let ((res (make-symbol-hash-table 100)))
    (define (visitor l) (res 'update-entry! (cadr l) '()) l)
    
    (apply-everywhere `((literal . ,visitor)) x)
    (res 'keys)))


(define (unexpanded-literals x)
  (filter (lambda (k)(not (system-expr-done? k))) (contained-literals x)))

(define (get-system-expression k)
  (let ((cur-entry (expressions 'retrieve k)))
    (cond ((eq? cur-entry '*hash-table-search-failed*) #f)
          (else (cur-entry 'get 'current-expression)))))

(define (system-expressions-without-literals)
  (let ((keys (expressions 'keys)))
    (filter
     (lambda(k)(not 
                (expression-contains-literals?
                 (get-system-expression k))))
     keys)))
 
(define (system-apply f k)
  ;; apply f at the slot named k
  (let ((old (get-system-expression k)))
    (update-expression k (f old))))

(define (system-mark-as-done k)
  (let ((old (get-system-expression k)))
    ((expressions 'retrieve k) 'set! 'done #t)))

(define (system-simplify-eval-arith k)
  (begin
    (system-apply simplify-eval-arith k)
    (system-mark-as-done k)
    )
)

(define (system-expr-done? k) ((expressions 'retrieve k) 'get 'done))

(define (depends-on? k1 k2)
  (and (not (eq? k1 k2))
       (member? k2 (contained-literals (get-system-expression k1)))))

(define (system-exprs-depending on)
  (let ((keys (expressions 'keys)))
    (filter (lambda(k)(depends-on? k on)) keys)))

(define (reset-expressions!)
  (map (lambda(k)
         (let ((x (expressions 'retrieve k)))
           (x 'set! 'current-expression (x 'get 'initial-expression))))
       (expressions 'keys))
  #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (system-expand-specific-literal x named)
  (define (expand-literal ll) (get-system-expression (cadr ll)))
  (apply-everywhere `((literal .,expand-literal)) x))


(define (expand-expr-in-expr to-expand within)
  (let ((old (get-system-expression within)))
    (let ((new (system-expand-specific-literal old to-expand)))
      (update-expression within new))))



(define (recursively-expand key)
  (dis "expanding " key dnl)
  (let ((deps (contained-literals (get-system-expression key))))
    (cond ((system-expr-done? key) #t)

          ((or (null? deps) 
             (equal? deps (list key)))
           (system-simplify-eval-arith key))
          
          (else

           ;; first expand any unexpanded literals
           (map
            (lambda (k) (if (not (eq? k key)) (recursively-expand k)))
            (unexpanded-literals (get-system-expression key)))

           ;; then replace all literals
           (map (lambda(k)(expand-expr-in-expr k key))
                (contained-literals (get-system-expression key)))

           ;; final cleanup
           (system-simplify-eval-arith key))
          )
    #t
))

(define (build-transition-time
         node-name
         n)
  (let ((x (ArithRep.ToScheme  (get-transition-time (nth-transition prs
                                                                    node-name
                                                                    n)))))
    (if (not (eq? (car x) 'pair)) 
        (error)
        (begin (let ((symbol (cadadr x)))
                 (dis "symbol is " symbol dnl)
                 (initialize-literals `(literal ,symbol))
                 symbol)))))

                    

(define (build-and-simplify-transition-time
         node-name
         n)
  (let ((x (ArithRep.ToScheme  (get-transition-time (nth-transition prs
                                                                    node-name
                                                                    n)))))
    (if (not (eq? (car x) 'pair)) 
        (error)
        (begin (let ((symbol (cadadr x)))
                 (dis "symbol is " symbol dnl)
                 (initialize-literals `(literal ,symbol))
                 (recursively-expand symbol)
                 (get-system-expression symbol))))))
                    

(define (find-dominator op lst)
  (let loop ((p       lst)
             (skipped '()))
    (if (null? p) 
        #f
        (let ((domination (map (lambda(x) (dominates? op (car p) x))
                                 (append skipped (cdr p)))))
          (if (not (member? #f domination))
              (car p)
              (loop (cdr p) (cons (car p) skipped)))))))
               
        
(define (allpairs? lst)
  (equal? (uniq eq? (map car lst)) '(pair)))

(define (allconstants? lst)
  (equal? (uniq eq? (map car lst)) '(constant)))

(define (conservative-pairs-dominator op pair-list)
  (if (not (allpairs? pair-list)) 
      #f
      (let ((x-dom (find-dominator op (map cadr pair-list)))
            (y-dom (find-dominator op (map caddr pair-list))))
        (if (and x-dom y-dom)
            (list 'pair x-dom y-dom)
            #f
            ))))

(define (conservative-select-component-extremum op sel which)
  (let ((pair-list (caddr sel)))
    (if (not (allpairs? pair-list))
        #f
        (let ((clauses
               (case which
                 ((X) (map cadr  pair-list))
                 ((Y) (map caddr pair-list))
                 (else (error "unknown component " which)))))
          (let ((dom
                 (find-dominator op 
                                 (map (lambda(c)(conservative-extremum-arith 
                                                 op 
                                                 c))
                                      clauses))))
            dom)))))
     

(define debug-op '())
(define debug-x  '())

(define (func-conservative-extremum op x)
  (set! debug-op op)
  (set! debug-x x)
  (let ((args
         (map (lambda(o)
                (conservative-extremum-arith o (caddr x)))
              (list op (opposite-extremum op)))))
    (if (allconstants? args)
        (list 'constant
              (apply op
                     (apply append 
                            (map cdr (map (lambda(a)(eval-func
                                                     (list 'func (cadr x) a)))
                                          args)))))
        x)))



(define (single-conservative-extremum op x)
  (cond ((eq? (car x) 'select)
         (let ((vals (caddr x)))
           (cond ((find-dominator op vals)               => (lambda(x) x))
                 
                 ((conservative-pairs-dominator op vals) => (lambda(x) x))
                 
                 (else x))))


        ((eq? (car x) 'func)
         (func-conservative-extremum op x))

        ((eq? (car x) 'literal)
         (get-system-expression (cadr x)))
        

        ((and (eq? (car x) 'frompair)
              (eq? (caadr x) 'select))
         (let ((xx (conservative-select-component-extremum op 
                                                           (cadr x) 
                                                           (caddr x))))
           (if xx xx x)))
                   
        (else x)))

              
          
(define (conservative-extremum op x)
  (let ((recurse (lambda (z) (conservative-extremum op z))))
    (let ((q  (single-conservative-extremum op x)))
      ;;(dis "single-conservative-extremum " x " -> " q dnl)
      (single-extremum op q recurse))))

(define (conservative-extremum-arith op x)
  (define (f x)
    (simplify-eval-step (conservative-extremum op x)))
  (fixpoint f x))

(define (min-max symbol) 
  (list (conservative-extremum-arith min `(literal ,symbol)) 
        (conservative-extremum-arith max `(literal ,symbol))))


(define (expand-literals literals x)
  (define (once x)
    (define (literal-expander x)
      (if (and (eq? (car x) 'literal) 
               (member? (cadr x) literals))
          (lookup-literal x)
          x
          )
      )
    (apply-everywhere `((literal . ,literal-expander)) x))

  (fixpoint once x))


(load "functions.scm")

(define (substitute x from to)
  (define (recurse x)
    (cond ((equal? x from)   to)
          ((list? x)         (map recurse x))
          (else              x)))

  (recurse x))

;; idea: delays and slews are chosen from ranges
;; times are real numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (literal? x) (equal? x `(literal ,(cadr x))))

(define (remove-select-step x)
  (apply-everywhere `((select . ,select-to-softmax)) x))

(define (setup-working-expressions!)

  ;; set up initial working expressions
  (map (lambda(k)
         (let ((x (expressions 'retrieve k)))
           (x 'set! 'working-expression
              (simplify-arith 
               (remove-select-step 
                (x 'get 'initial-expression))))))
       (expressions 'keys))
  #t)


(define (maxify-pairs c p0 p1)
  (let ((t0 (cadr  p0))
        (s0 (caddr p0))
        (t1 (cadr  p1))
        (s1 (caddr p1)))
    `(pair (softmax ,c ,t0 ,t1)
           (- (softmax ,c (+ ,t0 ,s0) (+ ,t1 ,s1))
              (softmax ,c ,t0 ,t1)))))

(define (maxify-pairlist c lst)
  (if (null? (cdr lst)) 
      (car lst)
      (maxify-pairs c (car lst) (maxify-pairlist c (cdr lst)))))

(define (select-to-softmax sel)
  ;; convert select to a softmax expr
  (if (not (eq? (car sel) 'select)) (error "not select"))
  
  (let* ((vals    (caddr sel))
         (numvals (length vals))
         ;; we could introduce a new variable here....
         (c       `(* (constant ,(/ numvals)) (+ ,@(map caddr vals)))))
    (maxify-pairlist c vals)
    ))

(define is-free?       '())
(define is-parametric? '())

(define (memoize-variable-types!)

  (set! is-free?
        (eq?-memo
         (lambda(tag)
           (let ((x (expressions 'retrieve tag)))
             (and (not (x 'get 'fixed))
                  (equal? 
                   (x 'get 'working-expression)
                   `(literal ,tag)))))))

  (set! is-parametric?
        (eq?-memo
         (lambda(tag)
           (eq? 
            (car ((expressions 'retrieve tag) 'get 'working-expression))
            'func))))
  )

(memoize-variable-types!)

(define (is-minimization-var? tag) (or (is-free? tag) (is-parametric? tag)))

(define depends-directly-on
  (eq?-memo
   (lambda(tag)
     (filter (lambda(k)(not (eq? k tag)))
             (contained-literals
              ((expressions 'retrieve tag) 'get 'working-expression))))))
     

(define (union . x)
  (let ((set (make-symbol-set 100)))
    (let loop ((p x))
      (if (null? p) 
          (set 'keys)
          (let loop2 ((q (car p)))
            (if (null? q) 
                (loop (cdr p))
                (begin (set 'insert! (car q))
                       (loop2 (cdr q)))))))))
        

(define (make-set-from-list lst)
  (let ((set (make-symbol-set 100)))
    (map (lambda(k)(set 'insert! k)) lst)
    set))

(define (all-deps tag)
  (let ((set (make-symbol-set 100)))

    (define (recurse x)
      (if (not (set 'member? x))
          (begin (set 'insert! x)
                 (map recurse (depends-directly-on x)))))

    (recurse tag)
    (set 'keys)))

(define all-deps2
  (eq?-memo
   (lambda (tag)
     (let ((set    (make-symbol-set 100))
           (direct (depends-directly-on tag)))
       
       (map (lambda(k)(set 'insert! k)) 
            (append direct (apply append (map all-deps2 direct))))

       (set 'keys)))))
   
(define minimization-target-expr '())

(define (set-minimization-target! x) (set! minimization-target-expr x) #t)

(define (get-nth-transition-expr node-text n)
   (ArithRep.ToScheme 
    (get-transition-time 
     (nth-transition prs (canonical-name node-text) n))))

(define (get-slew t) `(frompair ,t Y))

(define (get-time t) `(frompair ,t X))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-guardians tag)
  ;; initialize dependencies of "guardians" of node

  (define (add-tag-to-dependents guardian)
    ;;(dis "(add-tag-to-dependents " guardian ")" dnl)
    (let* ((x (expressions 'retrieve guardian))
           (old (x 'get 'dependents)))
      (if (and (not (member? tag old) (not (equal? tag guardian))))
          (x 'set! 'dependents (cons tag old)))))

  (let ((guardians (contained-literals ((expressions 'retrieve tag) 
                                        'get 'working-expression))))
    (map add-tag-to-dependents guardians)))

(define (initialize-dependents)
  (map initialize-guardians (expressions 'keys)))


(load "xcompiler.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  

(define (update-dependents! tag)
  (let* ((x      (expressions 'retrieve tag))
         (deps   (x 'get dependents)))
    (map (lambda(d)(map update-dependents! (recalc! d)))
         deps)))


(define (evaluation-order '()))

(define (fix-variable! tag at)
  (let ((x (expressions 'retrieve tag)))
    (x 'set! 'current-limits at)
    (x 'set! 'fixed #t))
  (memoize-variable-types!)
  )

(define (setup-minimization!)

  (dis "setting up symbolic algebra system..." dnl)
  (setup-algebra-system!)

  (dis "retrieving target transition data..." dnl)
  (set-minimization-target! (*get-target-expression*))

  (dis "setting up target expression and initializing formulas..." dnl)
  (define-expression 'TARGET minimization-target-expr)

  (initialize-literals '(literal TARGET))

  (fix-variable! 't0       *major-step-value*)
  (fix-variable! 'TheEpoch *the-epoch-value*)

  ;; cannot modify return values of is-free? or is-parametric? below
  ;; here as they are memoized.

  (setup-working-expressions!)

  (dis "calculating expression dependencies..." dnl)
  (let ((all (all-deps 'TARGET)))

    (dis "setting up initial values..." dnl)
    (map (lambda(t) ((expressions 'retrieve t) 'set! 'current-value 
                     *initial-parametric-value*))
         (filter is-parametric? all))
    (map (lambda(t) ((expressions 'retrieve t) 'set! 'current-limits 
                     *initial-free-value*))
         (filter is-free? all))
    
    all)

  (dis "calculating dependents..." dnl)
  (initialize-dependents)

  (dis "compiling minimization expressions..." dnl)
  (compile-minimization-expressions)

  (dis "calculating evaluation order by topological sort..." dnl)
  (set! evaluation-order (topological-sort))
  ;; set it for every member as well
  (let loop ((p evaluation-order))
    (if (null? p) 
        #t
        (begin
          ((expressions 'retrieve (car p)) 'set! 'evaluation-order p)
          (loop (cdr p)))))

  (dis "setting topological indices..." dnl)
  (let loop ((p evaluation-order)(n 0))
    (if (not (null? p)) 
        (begin
          ((expressions 'retrieve (car p)) 'set! 'topological-index n)
          (loop (cdr p) (+ n 1)))))

  (dis "initializing variables..." dnl)
  (initialize-system)

  (dis "compiling partial derivatives..." dnl)
  (compile-all-partial-derivatives!)

  (dis "calculating initial values of partial derivatives..." dnl)
  (initialize-all-numerical-derivatives)

  (dis "calculating initial values of total derivatives..." dnl)
  (calc-all-total-derivatives!)

)
         


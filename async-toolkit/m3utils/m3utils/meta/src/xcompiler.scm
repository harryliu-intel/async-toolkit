(load "calculus.scm")

(define (is-nan? x) (and (not (>= x 0)) (not (<= x 0))))

(define *inf* (/ 1 0))
(define *m-inf* (- *inf*))

(define (is-special? x)
  (or (= *inf* x) (= *m-inf* x) (is-nan? x)))

(define (compile-min-expression w-exp . tag)
  ;; tag is optional, needed for the partial derivatives only
  (if (not (list? w-exp)) 
      w-exp
      (case (car w-exp)
        ((func) `((make-func-evaluator 'eval
                                       ,(cadr w-exp)) 
                  ,(compile-min-expression (caddr w-exp))))

        ((func-prime) 

         `(let ((rho ((expressions 'retrieve ',(car tag))
                      'get 'current-value)))
            (+ 

             (* (- 1 rho) 
                (cadr ((make-func-evaluator 'evalDmin
                                            ,(cadr w-exp))
                       ,(compile-min-expression (caddr w-exp)))))

             (* rho
                (cadr ((make-func-evaluator 'evalDmax 
                                            ,(cadr w-exp))
                       ,(compile-min-expression (caddr w-exp)))))

             )
            )
         )

        ((constant) (cadr w-exp))

        ((literal) `(compute-actual-value ',(cadr w-exp)))

        (else (map compile-min-expression w-exp)))))

(define (compile-minimization-expressions)
  (map 
   (lambda(k)
     (let ((x (expressions 'retrieve k)))
       (x 'set! 
          'compiled-formula 
          (compile-min-expression (x 'get 'working-expression) k))))
   (all-deps 'TARGET)))

(define (interpolate-limits l v)
  (cond ((not (pair? l)) v)
        (else 
         (+ (* v (apply max (cdr l))) (* (- 1 v) (apply min (cdr l)))))))


(define (compute-actual-value tag)
  (let* ((x (expressions 'retrieve tag))
         (v (x 'get 'current-value))
         (l (x 'get 'current-limits)))
    (if (null? l) (error "not initialized: " tag))
    (if (null? v) l (interpolate-limits l v))))

(define (recalc! tag . force)
  ;;(dis "(recalc! " tag ")" dnl)
  ;; returns nil if no recalc, returns dependents if recalc
  (let* ((x          (expressions 'retrieve tag))
         (old-value  (x 'get 'current-limits))
         (new-value  (eval (x 'get 'compiled-formula))))
    (if (and (equal? new-value old-value) (null? force))
        '()
        (begin 
          (if (eq? tag 'TARGET) (dis "setting " tag " to " new-value dnl))
          (x 'set! 'current-limits new-value)
          (filter 
           (lambda(q)
             (if (< ((expressions 'retrieve q) 'get 'topological-index)
                    (x 'get 'topological-index))
                 (error "topo index decreasing " tag " -> " q dnl)
                 #t))
           
           (filter (lambda(q)(not (eq? tag q)))(x 'get 'dependents))

           )))))

(define (force-recalc! tag) (recalc! tag 'force))

(define (initialized? tag)
  (not (null? ((expressions 'retrieve tag) 'get 'current-limits))))

(define (ready-to-calc? tag)
  (not 
   (member? 
    #f
    (map initialized? 
         (filter (lambda(k)(not (eq? k tag)))
                 (contained-literals 
                  ((expressions 'retrieve tag) 'get 'working-expression)))))))

(define (initialize-system)
  (let ((done (make-symbol-set 100)))

    (define (do-one k)
      (if (and (not (done 'member? k))
               (ready-to-calc? k))
          (begin ;;(dis "initializing " k dnl)
            (done 'insert! k)
            (map do-one (force-recalc! k)))))
    
    (let ((initials (filter ready-to-calc? (expressions 'keys))))
      (map do-one initials)
      ))
  #t)

(define (actual-dependents tag)
  (filter (lambda(q)(not (eq? tag q)))((expressions 'retrieve tag) 'get 'dependents)))

(define actual-parents
  (eq?-memo
   (lambda(tag)
     (filter (lambda(q)(not (eq? tag q))) 
             (contained-literals 
              ((expressions 'retrieve tag) 
               'get 'working-expression))))))

(define (no-parents? tag) (null? (actual-parents tag)))

(define (recalc-dependents! tag)
  ;; this recalcs the dependents of a given tag, note it does not
  ;; update the partials!
  (let ((set      (make-symbol-set 100))
        (touched  (make-symbol-set 100)))
    (map (lambda(k)(set 'insert! k)) (actual-dependents tag))
    (let loop ((p ((expressions 'retrieve tag) 'get 'evaluation-order)))
      (cond ((= 0 (set 'size)) #t)
            ((null? p) (error "stopped with work to do :" (set 'keys)))
            ((set 'member? (car p))
             (set 'delete! (car p))
             (map (lambda(k)(set 'insert! k)) (recalc! (car p)))
             (touched 'insert! (car p))
             (loop (cdr p)))
            (else 
             (loop (cdr p)))))
    (touched 'keys)))

(define (contained? lst in)
  (or (null? lst)
      (and (member? (car lst) in) (contained? (cdr lst) in))))

(define (topological-sort)
  (let loop ((l          '())
             (s          (filter no-parents? (all-deps 'TARGET))))
    (if (null? s) 
        (reverse l)
        (let* ((n           (car s))
               (ll          (cons n l))
               (ms          (actual-dependents n)))
          (loop ll
                (append (filter (lambda(m)
                                  (let ((mp (actual-parents m)))
                                    (contained? mp ll)))
                                ms)
                        (cdr s)))))))


(define (calc-partial-derivative of wrt)
  (simplify-eval-arith 
   (differentiate ((expressions 'retrieve of) 'get 'working-expression) 
                  wrt)))


(define (compile-partial-derivatives of)
  (let ((wrt (actual-parents of)))
    (map cons wrt (map (lambda(k)(compile-min-expression
                                  (calc-partial-derivative of k) of))
                       wrt))))


(define (compile-all-partial-derivatives!)
  (map (lambda(k)((expressions 'retrieve k) 'set! 'compiled-derivatives
                  (compile-partial-derivatives k)))
       (all-deps 'TARGET))
  #t)

(define u-n-d-v-tag '())

(define (update-numerical-derivative-values! tag)
  (set! u-n-d-v-tag tag)
  (let* ((x          (expressions 'retrieve tag))
         (compiled-d (x 'get 'compiled-derivatives)))
    (x 'set! 'current-partial-values
       (map cons 
            (map car compiled-d) 
            (map eval (map cdr compiled-d))))))


(define (initialize-all-numerical-derivatives)
  ;; only called on initialization, should be able to do it incrementally
  ;; otherwise

  (map update-numerical-derivative-values! (all-deps 'TARGET))
  #t)

(define (get-current-total-derivative tag)
  (let ((x ((expressions 'retrieve tag) 'get 'total-derivative)))
    (if (null? x) 0 x)))

(define g-c-p-d-args '())

(define (get-current-partial-derivative of wrt)
  (set! g-c-p-d-args (cons of wrt))
  ;; we have to check if we have a partial derivative here.
  ;; its possible weve been asked for a partial derivative to a dependent
  ;; *beyond* the TARGET -- clearly we dont need that so we return 0 
  (cond ((assoc wrt ((expressions 'retrieve of) 'get 'current-partial-values))
         => cdr)
        (else 0)))

(define debug-wrt '())

(define (calc-total-derivative! of wrt)
  (set! debug-wrt wrt)
  ;; assumes total-derivative of all dependents of 'wrt' have already
  ;; been calculated and are present in the total-derivative field 
  ;; of the variable-state
  (let ((deps (actual-dependents wrt)))
    (let ((res
           (apply +
                  (map *
                       (map get-current-total-derivative                     
                            deps)
                       (map (lambda(d)(get-current-partial-derivative d wrt)) 
                            deps)
                       ))))
      (if (is-special? res)
          (error "special total derivative at key " wrt " : " res))

      ((expressions 'retrieve wrt) 'set! 'total-derivative res)
      res)))

(define (calc-all-total-derivatives!)
  ((expressions 'retrieve 'TARGET) 'set! 'total-derivative 1)
  (let loop ((p (cdr (reverse evaluation-order)))) ;; skip TARGET
    (if (null? p) #t
        (begin
          (calc-total-derivative! 'TARGET (car p))
          (loop (cdr p))
          ))
    )
  )

(define (calc-all-total-derivatives!)
  ((expressions 'retrieve 'TARGET) 'set! 'total-derivative 1)
  (let loop ((seen-target #f)
             (p (reverse evaluation-order))) 
    ;; skip past TARGET so we dont zero its total
    (cond ((null? p) #t)

          ((eq? (car p) 'TARGET) 
           ((expressions 'retrieve (car p)) 'set! 'total-derivative 1)
           (loop #t (cdr p)))

          (seen-target
           (calc-total-derivative! 'TARGET (car p))
           (loop #t (cdr p))
           )

          (else 
           ((expressions 'retrieve (car p)) 'set! 'total-derivative 0)
           (loop #f (cdr p)))
          )
    ))

;; sketch of what remains:
;; use partials to choose variable to look at
;; dont pick variable that cant be modified or is at limit
;; pick from remaining
;; use Brent's method to find minimum
;; update partials
;; repeat until ?

(define (change-and-recalc! tag new-value)
  ;; returns all touched variables
  ((expressions 'retrieve tag) 'set! 
   
   (if (is-parametric? tag) 'current-value 'current-limits)
   ;; "free" variables have their values stored in current-limits

   new-value)
  (cons tag (recalc-dependents! tag)))

(define (search-one! tag)
  (cond ((is-parametric? tag) (search-one-parametric! tag))
        ((is-free?       tag) (search-one-free!       tag))
        (else (error "cant search non-parametric non-free " tag))))

;; the following routines map any real to the 01 segment and its inverse:
(define (map-real-01 x) (* 0.5 (+ 1 (tanh x))))
(define (map-01-real y)  (atanh (- (* y 2) 1)))

;; 
(define (get-current-opt-value tag)
  ((expressions 'retrieve tag) 
   'get
   (cond ((is-free? tag)  'current-limits)
         ((is-parametric? tag) 'current-value)
         (else (error "not free or parametric " tag)))))


;; debugging for the minimizer
(define debug-s1m-mapper  '())
(define debug-s1m-imapper  '())
(define debug-s1m-tag     '())
(define debug-s1m-step1   '())
(define debug-s1m-init-cv '())
(define debug-s1m-init-cl '())

(define (search-one-mapped! real-mapper inv-mapper tag)
  ;; simple version that just calls the bracketing code directly

  (set! debug-s1m-mapper real-mapper)
  (set! debug-s1m-imapper inv-mapper)
  (set! debug-s1m-tag    tag)
  (set! debug-s1m-init-cv ((expressions 'retrieve tag) 'get 'current-value))
  (set! debug-s1m-init-cl ((expressions 'retrieve tag) 'get 'current-limits))
  (set! debug-s1m-step1 '())

  (let ( (t      (expressions 'retrieve 'TARGET))
         (touched (make-symbol-set 100))
         (last-arg #f) )

    (define (change! *unused* to)
      (dis "search-one-mapped!.change! " tag " " to dnl)
      (if (is-special? to) ;; catch problems early
          (error "attempting to change to special value: " to))
      (let ((arg (real-mapper to)))
        (if (or (not last-arg) (not (= arg last-arg)))
            (begin
              (set! last-arg arg)
              (map (lambda(k)(touched 'insert! k)) 
                   (change-and-recalc! tag arg)))))

      (let ((res (t 'get 'current-limits))) ;; return value of TARGET
        (if (is-special? res) (error "special TARGET: " res))
        res
        )
      )

    (let* ((min-obj    (new-modula-object 'LRFunction.T `(eval . ,change!)))
           (initval (get-current-opt-value tag))
           (init-brack (make-init-brack inv-mapper initval))
           (step1      (Bracket.SchemeInitial init-brack min-obj)))
      (dis "initial value of " tag " " initval dnl)
      (dis "after Bracket.SchemeInitial, step1: " step1 dnl)
      (set! debug-s1m-step1 step1)

      (Bracket.SchemeBrent (cdr (assoc 'x step1)) min-obj  *epsmach*)

      ;; update partials
      (dis "re-establishing system invariants:" dnl)
      (dis "recalcing numerical derivatives..." dnl)
      (map update-numerical-derivative-values! (touched 'keys))
      (dis "recalculating total derivatives..." dnl)
      (calc-all-total-derivatives!)

      (dis "TARGET is " (t 'get 'current-limits) dnl)

      ;;                        (if (= (get-current-opt-value tag) initval)
      ;;                                        (error "no change in " tag ", why?"))
      #t
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (finitize x limit)
  (cond ((is-nan? x) 0)
        ((= *inf* x) limit)
        ((= *m-inf* x) (- limit))
        (else x)))

(define (make-init-brack inv-mapper value)
  (map cons
       '(a b c)
       (mergesort
        (let ((old (finitize (inv-mapper value)
                             (inv-mapper (- 1 (sqrt *epsmach*)))
                             ))) ;; convert to bracketing space
          (if (> (abs old) *standard-bracketing-value*)
              (list (* old 2) old 0)
              (list (- *standard-bracketing-value*) 
                    old 
                    *standard-bracketing-value*)))
        <)))

(define identity (lambda(x) x))

(define (square x) (* x x))

(define (search-one-parametric! tag)(search-one-mapped! map-real-01 map-01-real tag))

(define (search-one-free! tag) (search-one-mapped! exp log tag))
;; free variables should not be negative

;; now what about changing the "fixed" variables t0 and TheEpoch?

(define debug-tag '())
(define debug-ic-pair '())   

(define (is-candidate? pair)
  (set! debug-ic-pair pair)
  ;; pair is (<tag> . <total deriv>)
  (let ((tag  (car pair))
        (dVdt (cdr pair)))

    (cond ((is-free? tag) (> (get-current-opt-value tag) *epsmach*))
          ;; our free vars are limited below by zero

          ((not (is-parametric? tag)) #f)
          (else 
           (set! debug-tag tag)
           (let ((v (get-current-opt-value tag)))

             (and (not (null? v)) ;; this tests for vars past TARGET
                  (or (< dVdt 0) (> v (sqrt *epsmach*)))
                  (or (> dVdt 0) (< v (- 1 (sqrt *epsmach*))))))))))

(define (candidate-less-than? c0 c1) (> (abs (cdr c0)) (abs (cdr c1))))

(define (find-next-candidates)

  (define (get-total k) ((expressions 'retrieve k) 'get 'total-derivative))

  (let* ((keys  (expressions 'keys))
         (keyed-derivs (map cons keys (map get-total keys))))
    
    (mergesort (filter is-candidate?  keyed-derivs) candidate-less-than?)
    ))

(define search-sequence '())

(define (search-loop)

  (define (repeat)
    (let ((next (car (find-next-candidates))))
      (if (and (> (abs (cdr next)) *quit-derivative*)
               #t)

          ;;    (or (< (length search-sequence) 2)
          ;;        (not (= (cdar search-sequence)
          ;;        (cdadr search-sequence))))
          (begin
            (dis "====================  SEARCHING " next "  ===================="dnl)
            (set! search-sequence
                  (cons 
                   (cons (car next) 
                         ((expressions 'retrieve 'TARGET) 'get 'current-limits))
                   search-sequence)
                  )

            (search-one! (car next))
            (repeat)
            )
          (dis "***** SEARCHING DONE: " next " doesnt meet search criterion." dnl))))

  (set! search-sequence '()) ;; debugging

  (repeat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (calc-numerical-derivative of wrt) ;; for debugging, not used by code
  (let* ((x0    (get-current-opt-value wrt))
         (y0    (compute-actual-value of))

         (step  (if (> x0 0.5) -1e-6 1e-6))

         (x1    (+ x0 step))
         (*unused*    (change-and-recalc! wrt x1))
         (y1    (compute-actual-value of))

         (slope (/ (- y1 y0) (- x1 x0))))
    (change-and-recalc! wrt x0) ;; reset it
    slope
    ))

(define (range-times-total tag)
  (let* ((x (expressions 'retrieve tag))
         (r (x 'get 'current-limits))
         (d (x 'get 'total-derivative)))
    (* d (- (caddr r) (cadr r)))))

(define (symbol-has-prefix? symbol prefix)
  (let* ((pstr (symbol->string prefix))
         (sstr (symbol->string symbol))
         (n    (string-length pstr)))
    (and 
     (>= (string-length sstr) n)
     (equal? (head n (string->list sstr)) (string->list pstr)))))

(define (is-time-symbol? sym) (symbol-has-prefix? sym 'time))

(define (get-all-node-transition-times nodetxt)
  (map cadadr 
       (map ArithRep.ToScheme 
            (map get-transition-time 
                 (get-transitions prs nodetxt)))))

(define (get-all-transition-times)
  (let* ((node-tbl 
          (modula-type-op 'PRS.T 'get-field prs 'nodes))
         (nodes    
          (map Name.Format (enumerate-tbl-keys 'NameRefTbl node-tbl))))
    (apply append
           (map (lambda(n)(map (lambda(t)(cons t n))
                               (get-all-node-transition-times n))) nodes))))

(define (get-time-predecessor t)
  ;; get the last time that this time depends on
  ;; (last fanin if several)
  (let ((tt (get-all-transition-times)))
    (let loop ((preds     (filter is-time-symbol? (actual-parents t)))
               (res       '())
               (last-time -1))
      (cond ((null? preds) (if (null? res) 
                               #f
                               (list res 
                                     (cond ((assoc res tt) => cdr) (else #f))
                                     (- (compute-actual-value t)
                                        (compute-actual-value res)))))
            ((> (compute-actual-value (car preds)) last-time)
             (loop (cdr preds) (car preds) (compute-actual-value (car preds))))
            (else (loop (cdr preds) res last-time))))))


(define (get-n-predecessors t n)
  (if (= n 0) 
      '()
      (let ((p (get-time-predecessor t)))
        (cons p (get-n-predecessors (car p) (- n 1))))))

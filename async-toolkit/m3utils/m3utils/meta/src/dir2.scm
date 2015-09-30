;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; timing margin simulation using contents of directives.0 file
;;
;;
;; Copyright 2011 Fulcrum Microsystems.  All rights reserved.
;; Author: Mika Nystrom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "meta.scm")
(load "system-parameters.scm")
(load "scenario.scm")
(Debug.SetLevel 10)

(define (list-fields-and-supertype-fields tc)
  (if (or (> tc (rttype-maxtypecode))
          (not (member? 'list-fields  (list-modula-type-ops tc))))
      '() 
      (let ((parent (rttype-supertype tc)))
        (append         
         (map (lambda(f)(cons tc f)) (modula-type-op tc 'list-fields '()))
         
         (list-fields-and-supertype-fields parent)))))

(define debug-x '())

(define (convert-all-reachable-fields x . formatter)

  (define (recurse x)
    (dis "(recurse " x ")" dnl)
    (set! debug-x x)
    (let* ( (tc     (rttype-typecode x))
            (ops    (list-modula-type-ops tc))
            (fields (list-fields-and-supertype-fields tc)) )
      (cond ((member? 'deref ops)
             (map recurse (modula-type-op tc 'deref x)))

            ((null? fields)
             (if (null? formatter) x (formatter x)))

            (else (map (lambda(f)(cons (cdr f)
                                       (recurse
                                        (modula-type-op (car f) 
                                                        'get-field 
                                                        x 
                                                        (cdr f)) )))
           fields)))))


  (recurse x))

(define (get-transitions prs nodetxt)
  (let* ((nodes (modula-type-op 'PRS.T 'get-field prs 'nodes))
         (node  (table-get 'NameRefTbl.T nodes (Name.ParseText nodetxt)))
         (tlist (modula-type-op 'PRS.Node 'get-field node 'transitions)))
    (convert-m3-list-to-list 'TransitionList.T tlist)))

(define (nth-transition prs nodetxt n)
  (let ((lst (get-transitions prs nodetxt)))
    (if (null? lst) #f (nth lst (- (- (length lst) 1) n )))))

(define (lookup-final-transition prs nodetxt)
  (let ((lst (get-transitions prs nodetxt)))
    (if (null? lst) #f  (car lst))))

(define (lookup-initial-transition prs nodetxt)
  (let ((lst (get-transitions prs nodetxt)))
    (if (null? lst) #f  (car (reverse lst)))))

(define (get-transition-time trans)
   (modula-type-op 'PRS.XTransition 'get-field trans 'at))


(load "algebra.scm")


(define (nth-trans-time prs nodetxt n)
  (ArithRep.ToScheme
   (get-transition-time 
    (nth (reverse (get-transitions prs nodetxt)) n)
    )))
                                 
(define (spaces n)
  (if (= n 0) "" (string-append " " (spaces (- n 1)))))

(define (pp lst) ;; pretty-print
  (dis "(" dnl)
  (map (lambda (c) (dis c dnl)) lst)
  (dis ")" dnl)
  #t)


(define (replace-char str c d)
  (list->string
   (map (lambda(x)(if (eq? x c) d x))
        (string->list str))))

(define modula-global-set
  (make-modula-set 'NameSetDef.T (map Name.ParseText globals)) )

(define modula-ignore-set
  (make-modula-set 'NameSetDef.T (map Name.ParseText '("ERROR"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; allocate directives-tbl
(define directives-tbl (modula-type-op 
                        'Directives.Table
                        'call-method
                        (new-modula-object 'Directives.Table)
                        'init
                        '(10)))

(define tm  (new-modula-object 'Circuit.CktTimingModel))

(define (canonicalize-impl x)
  (let ((canon (canonical-name (Name.Format x))))
    (Name.ParseText canon)))
    
(define canonicalize (eq?-memo canonicalize-impl))


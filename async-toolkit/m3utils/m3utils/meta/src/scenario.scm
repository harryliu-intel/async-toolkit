(define (make-binary-tree tag lst)
  (if (= 2 (length lst))
      (list tag (car lst) (cadr lst))
      (list tag (car lst) (make-binary-tree tag (cdr lst)))))

(define (binarify x)
  (case (car x)
    ((or and) (make-binary-tree (car x) (cdr x)))
    (else x)))

(define (make-scenario instance-name s next)

  (define (make-name txt)
    (dis "(make-name " (stringify txt) ")" dnl)
    (Name.ParseText (canonical-name (string-append 
                                     (Name.Format instance-name) "." txt))))

  (define (make-await x)

    ;; e.g. (make-await 
    ;;         '( and (or ("x1"  . #t) ("x2"  . #t)) 
    ;;                (or ("_y1" . #f) ("_y2" . #f)) )
    ;;       )

    (case (car x)
      ((or)  (new-modula-object 'Scenario.Or 
                                `(x0 . ,(make-await (cadr x)))
                                `(x1 . ,(make-await (caddr x)))))
      ((and) (new-modula-object 'Scenario.And
                                `(x0 . ,(make-await (cadr x)))
                                `(x1 . ,(make-await (caddr x)))))
      (else  (new-modula-object 'Scenario.V 
                                `(node . ,(make-name (car x)))
                                `(to . ,(cdr x))))))

  (define (make-block x)
    ;; e.g. (make-block '(("a" . #t)("b" . #f)) )
    (if (null? x) 
        '()
        (new-modula-object 'Scenario.B
                           `(node . ,(make-name (caar x)))
                           `(to   . ,(cdar x))
                           `(next . ,(make-block (cdr x))))))



  (let ((await (make-await (binarify (cdr (assoc 'await s)))))
        (block (make-block (cdr (assoc 'block s)))))
    (new-modula-object 'Scenario.T 
                       `(expr . ,await) 
                       `(mutex . ,block)
                       `(next . ,next)
                       )))

(define (make-scenario-sequence instance-name s)
  (if (null? s) 
      '()
      (make-scenario instance-name
                     (car s)
                     (make-scenario-sequence instance-name (cdr s)))))

(define (get-node n)
  (let ((trans (PRSimulate.Get prs (Name.ParseText (canonical-name n)))))
    (if (null? trans) 
        'X
        (modula-type-op 'PRS.XTransition 'get-field trans 'newValue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helper functions to name rails in a channel and make conditions
;;

(define (make-rails channel width)

	(define (format-numbers n)
		(if (= n 0) 
				'()
				(cons (number->string (- n 1)) (format-numbers (- n 1)))))

	(map (lambda(s)(string-append channel "." s)) (format-numbers width)))

(define (make-valid-cond channel width)
  `(or ,@(map (lambda(x)(cons x #t)) (make-rails channel width))))

(define (make-neutral-cond channel width)
  `(and ,@(map (lambda(x)(cons x #f)) (make-rails channel width))))


                                                                    ;;
                                                                    ;;
                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
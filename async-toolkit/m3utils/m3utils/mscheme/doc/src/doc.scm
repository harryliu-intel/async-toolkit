;;
;; $Id$
;;

(require-modules "basic-defs" "display" "mergesort" "hashtable")

(define (symbol-append . x)
  (string->symbol
   (eval 
    (cons 'string-append
          (map (lambda (s) 
                 (cond ((symbol? s) (symbol->string s))
                       ((string? s) s)
                       (else (error (string-append 
                                     "not a string or symbol : " s)))))
               x)))))

(define (process-environments port texport)
	(begin
		(dis "(" dnl port)
		(let loop ((to-go        environment-lst)
							 (prims-sofar '()))
			(if (null? to-go) #t
					(begin
						(dis "ENV \"" (car to-go) "\"" dnl '())
						(dis dnl
								 "  ;;; ENV \"" (car to-go) "\"" dnl 
								 dnl
								 port)
						(let ((done-this-step (do-one (car to-go) prims-sofar port texport)))
							(loop (cdr to-go)
										(append done-this-step prims-sofar))))))
		(dis dnl 
				 ")" dnl      
				 port)))

(define (get-named-env nam)
  (eval (symbol-append nam "-environment")))

(define (order-by-symbol e1 e2)
  (string<? (symbol->string (car e1))
            (symbol->string (car e2))))

(define (do-one env-name prims-sofar port texport)
  (let ((these-prims 
         (mergesort 
          (list-primitives (get-named-env env-name))
          order-by-symbol)))
    (let ((todo
           (filter (lambda (p) (not (memq (car p) prims-sofar))) these-prims)))
      (map 
			 (lambda (p) (begin (dis-screen p '())
													(dis-file p port)
													(dis-tex p texport)
													)					 
							 )
			 todo)
      (map car todo))))

(define (dis-screen p port)
	 (dis "prim: " (car p) dnl port) )

(define (dis-file p port)
	(dis "(" (car p) 
			 " \"" (get-cat (car p)) 
			 "\" \""
			 (get-desc (car p)) "\""
			 ")" dnl port))

(define (dis-tex p port)
	(dis "" port))


(define (get-cat prim) 
	(let ((v (cats 'retrieve (symbol->string prim))))
		(if (string? v) v "UNDEF")))

(define (get-desc prim) 
	(let ((v (descs 'retrieve (symbol->string prim))))
		(if (string? v) v "UNDEF")))

(define cats (make-string-hash-table 100))

(define descs (make-string-hash-table 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-it)
	(let ((memory-port (open-output-file "memory.dat"))
				(tex-port (open-output-file "prims.tex")))
		(process-environments memory-port tex-port)
		(close-output-port memory-port)
		(close-output-port tex-port)
		))

(define (load-old)
	(let ((memory-port (open-input-file "memory.dat")))
		(let ((prims (read memory-port)))
			(close-input-port memory-port)

			(cats 'clear!) 
			(descs 'clear!)
			
			(map (lambda (p) 
						 (let ((prim (symbol->string (car p))))
							 (cats 'add-entry! prim (cadr p))
							 (descs 'add-entry! prim (car (cddr p)))))

					 prims
					 )
			)))

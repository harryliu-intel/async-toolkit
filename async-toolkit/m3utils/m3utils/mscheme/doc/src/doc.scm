;;
;; $Id$
;;

(require-modules "basic-defs" "display" "mergesort")

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

(define (process-environments)
	(let loop ((to-go        environment-lst)
						 (prims-sofar '()))
		(if (null? to-go) #t
				(begin
					(dis "ENV \"" (car to-go) "\"" dnl '())
					(let ((done-this-step (do-one (car to-go) prims-sofar)))
						(loop (cdr to-go)
									(append done-this-step prims-sofar)))))))

(define (get-named-env nam)
	(eval (symbol-append nam "-environment")))

(define (order-by-symbol e1 e2)
	(string<? (symbol->string (car e1))
						(symbol->string (car e2))))

(define (do-one env-name prims-sofar)
	(let ((these-prims 
				 (mergesort 
					(list-primitives (get-named-env env-name))
					order-by-symbol)))
		(let ((todo
					 (filter (lambda (p) (not (memq (car p) prims-sofar))) these-prims)))
			(map (lambda (p) (dis "prim: " (car p) dnl '())) todo)
			(map car todo))))
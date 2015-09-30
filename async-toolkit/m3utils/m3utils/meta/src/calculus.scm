(define (differentiate x wrt)
	;;(dis "(differentiate " (stringify x) " " (stringify wrt) ")" dnl)

	(define (recurse z)(differentiate z wrt))

	(case (car x)
		((+ -)
		 (cons (car x) (map recurse (cdr x))))

		((func)
		 `(* (func-prime ,(cadr x) ,(caddr x)) ,(recurse (caddr x))))
		
		((*)
		 (cons '+ 
					 (let loop ((p      (cdr x))
											(past   '())
											(res    '()))
						 (if (null? p) 
								 res
								 (loop (cdr p) 
											 (cons (car p) past)
											 (cons (cons '*
																	 (cons
																		(recurse (car p)) 
																		(append past (cdr p))))
														 res))))))
								 
		((constant)
		 '(constant 0))

		((literal) `(diff ,(cadr x) ,wrt))

		((softmax)
		 (let ((c (cadr x)) (x (caddr x)) (y (cadddr x)))
			 `(+
				 (* (d-softmax-dc ,c ,x ,y) ,(recurse c))
				 (* (d-softmax-dx ,c ,x ,y) ,(recurse x))
				 (* (d-softmax-dy ,c ,x ,y) ,(recurse y)))))
					 
		(else (error (string-append "cant differentiate " x)))))


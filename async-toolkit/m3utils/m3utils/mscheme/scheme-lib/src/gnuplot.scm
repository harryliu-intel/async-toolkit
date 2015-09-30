;; 
;; $Id: gnuplot.scm,v 1.2 2008/11/08 20:09:44 mika Exp $
;;
;; basic Scheme definitions for GNUPLOT
;;

(require-modules "basic-defs" "display")

(define (make-plot lst nam)
	(define (iter lst port)
		(if (null? lst) #t
				(begin (dis (car lst) dnl port)
							 (iter (cdr lst) port))))

	
	(let ((p (open-output-file nam)))
		(iter lst p)
		(close-output-port p)))

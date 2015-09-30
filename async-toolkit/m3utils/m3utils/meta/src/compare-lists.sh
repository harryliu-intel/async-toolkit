#!/bin/sh

cat > /tmp/in$$.scm << __SCM__

(require-modules "display")

(define a (open-input-file "$1"))
(define b (open-input-file "$2"))

(define aa (read a))
(define bb (read b))

(define data (map list (map cdr aa) (map cdr bb)))

(define (add-deltas lst)
	(let* ((vals   (map cadr lst))
				 (base   (car vals))
				 (deltas (map (lambda (x)(- x base)) vals)))
		(map (lambda(z t)(append z (list t))) lst deltas)))
				 
					 
(define (dis-t x)(dis x dnl))

(define (dis-p x)(map dis-t (add-deltas (map cleanup x))) (dis dnl))

(define (cleanup lst)
	(let loop ((p lst)
						 (name ""))
		(cond ((number? (car p))(cons name p))
					(else (loop (cdr p) (string-append name (stringify (car p))))))))
		 
			

(map dis-p data)

__SCM__

~/meta/mscheme/interactive_r/LINUXLIBC6/mscheme /tmp/in$$.scm exit

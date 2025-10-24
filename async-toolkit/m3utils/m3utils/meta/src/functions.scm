;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Functions for calculating a "soft max".  
;; max(c)(x,y) returns the max of x and y with scale parameter c
;;
;; Copyright 2011 Fulcrum Microsystems.  All rights reserved.
;; Author: Mika Nystrom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; avoid math overflow by doing ln cosh x specially:
(define (logcosh x)
	(cond ((< x 0) (logcosh (- x)))
				((> x 100) (- x (log 2)))
				(else (log (cosh x)))))

;; soft-max function itself
(define (softmax c x y)
	;; c is the scale parameter
	(* 0.5 
		 (+
			(* c (logcosh (/ (- x y) c)))
			(+ x y)
			(* c (log 2)))))


;; and the partial derivatives of softmax w.r.t. the second, third, and first
;; slots

;; the second and third slots are by construction of the function
;; (see my old tech report about soft max).
(define (d-softmax-dx c x y)
	(* 0.5 (+ 1	(tanh (/ (- x y) c)))))

(define (d-softmax-dy c x y)
	(* 0.5 (- 1 (tanh (/ (- x y) c)))))

;; the derivative w.r.t. c is a bit different
;; I used Wolfram Alpha to calculate it
(define (d-softmax-dc c x y)
	(let ((diff (- x y)))

		(/ 
		 (+ 
			(* (- diff) (tanh (/ diff c)))
			(* c (+ (logcosh (/ diff c)) (log 2))))

		 (* 2 c))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a better softmax function would be simply

(define (better-softmax c . lst)
	;; not numerically stable. should calc relative to actual max of lst
	(* c (log (apply + (map (lambda(xx)(exp (/ xx c))) lst)))))

;; (enhancements left for later)

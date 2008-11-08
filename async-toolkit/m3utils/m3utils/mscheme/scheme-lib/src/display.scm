;;
;; $Id$
;;

(define dnl #\newline)

(define (dis . x)
	(display (accumulate string-append "" (all-except-last x)) (last x)))


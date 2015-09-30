;;
;; $Id: display.scm,v 1.1 2008/10/23 23:37:45 mika Exp $
;;

(define dnl #\newline)

(define (dis . x)
	(display (accumulate string-append "" (all-except-last x)) (last x)))


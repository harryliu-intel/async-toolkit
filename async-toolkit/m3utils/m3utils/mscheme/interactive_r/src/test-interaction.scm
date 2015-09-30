;;
;; $Id: test-interaction.scm,v 1.2 2009/11/28 12:11:34 mika Exp $
;;
;; test "interaction"
;;

;; it would be nice to be able to scan the dynamic scope too, 
;; rather than just the lexical!

(require-modules "display")

(define (go . x) `(**return-hook** @x))

(define break 100)

(define (fact n)
  (run-interaction (< n break) )
  (if (= 0 n) 1 (* n (fact (- n 1)))))

(dis (fact 69) dnl)

(exit)

;; example transcript:
;;
;; M-Scheme Experimental
;; LITHP ITH LITHENING.
;; return **return-hook** to quit.
;; > (go)
;; M-Scheme Experimental
;; LITHP ITH LITHENING.
;; return **return-hook** to quit.
;; > n
;; 68
;; > (go)
;; M-Scheme Experimental
;; LITHP ITH LITHENING.
;; return **return-hook** to quit.
;; > (go)
;; M-Scheme Experimental
;; LITHP ITH LITHENING.
;; return **return-hook** to quit.
;; > (set! n 0)
;; 0
;; > (go)
;; 314364

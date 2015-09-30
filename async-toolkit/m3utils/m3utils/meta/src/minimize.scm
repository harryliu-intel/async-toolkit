(define (minimize-01 change!   ;; proc of 1 param that changes indep var
										 get-var   ;; proc of 0 params that returns indep var
										 get-val   ;; proc of 0 params that returns dep var
										 derivs!   ;; proc of 0 params that prepares derivative
										 get-deriv ;; proc of 0 params that returns derivative
										 done!     ;; done
										 )

)

;; testing of Brent's method and initial bracketing code

;; (define (sin-method obj arg) (sin arg))
;; (define fo (new-modula-object 'LRFunction.T `(eval . ,sin-method) ))
;; ((obj-method-wrap fo 'LRFunction.T) 'eval 1)
;; (define brack (cdr (assoc 'x 
;;                           (Bracket.SchemeInitial 
;;                               '((a . 0.1) (b . 0.2) (c . 0.3)) fo)))
;; (Bracket.SchemeBrent brack fo 0.00000001) 
(define *debug-env-chain* #f) ;; in global env

(define (debug!)
  (dis "Use (exit-debugger!) to go back to main REPL." dnl)
  (change-global-environment! *last-error-environment*))

(define (exit-debugger!)
  (change-global-environment! *the-global-environment*))

;;*last-error-object*

;; we should have some way to navigate the call stack, also.
;; it can't be that hard, just leave a breadcrumb in apply somewhere.*

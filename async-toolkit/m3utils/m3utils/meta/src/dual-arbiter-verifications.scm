(require-modules "scenario.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define x0a2-verification-0             ;; x.0+;A.2+;...;x.0+;...
  `(
    (previous-cycle-scenario . 
       ((await . ,(make-valid-cond "A" 3)) 
        (block . (("_x.1" . #f)))))

    (early-transition "L[1].0" 3)
    (late-transition  "x.0" 3)

    (current-cycle-block-list ("_x.1" . #f))
    )
)

(define x0a2-verification-1             ;; x.0+;A.2+;...;x.1+;...
  `(
    (previous-cycle-scenario . 
       ((await . ,(make-valid-cond "A" 3)) 
        (block . (("_x.1" . #f)))))

    (early-transition "L[0].0" 3)
    (late-transition  "x.1" 1)

    (current-cycle-block-list ("_x.0" . #f))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x0a0-verification-0             ;; x.0+;A.0+;...;x.1+;...
  ;; in this scenario we want to check we dont get strict alternation!
  `(
    (previous-cycle-scenario . 
        ((await . ,(make-valid-cond "A" 3)) 
         (block . (("_x.1" . #f) ("_y[1].1" . #f)))))

    (early-transition "L[0].0" 3)
    (late-transition  "x.1" 1)

    (current-cycle-block-list ("_x.0" . #f))
    )
)

(define x1a1-verification-0             ;; x.1+;A.1+; ...; x.0+;...
  ;; I think this verification crashes a.t.m. but is probably not necessary
  `(
    (previous-cycle-scenario .
        ((await . ,(make-valid-cond "A" 3))
         (block . (("_x.0" . #f) ("_y[0].1" . #f)))))

    (early-transition "L[1].0" 3)
    (late-transition  "x.0" 1)

    (current-cycle-block-list ("_x.1" . #f))
    )

)

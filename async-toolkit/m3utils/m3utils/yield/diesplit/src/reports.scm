(load "reports-code.scm")

(define *n5-n* 32)

(define params '((0.075 1) ;; B-E
                 (0.05 1)
                 (0.10 1)

                 (0.10 0.05) ;; Stapper
                 (0.10 0.02)
                 (0.10 0.01)
                 (0.075 0.05)
                 (0.075 0.02)
                 
                 (0.10 10) ;; Poisson
                 (0.05 10) 
                 ))

(define basic-params '((0.05 1) (0.075 0.05) (0.05 10)))

(report-yields-for-params
 (tfc-model)

 basic-params  ;; short list of techs
;params        ;; long list of techs
 
 (list (eohalf-25t-model) (lrhalf-25t-model))
 )

(report-yields-for-params
 (tfc-twodie-model)

 basic-params  ;; short list of techs
;params        ;; long list of techs
 
 ()
 )

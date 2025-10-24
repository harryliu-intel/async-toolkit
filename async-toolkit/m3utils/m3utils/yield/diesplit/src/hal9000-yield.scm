; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(define (hal9000-model)
  '(hal9000
    (hal9000-misc 50)
    (* hal1000-modules 9 9
       (hal1000
        (hal1000-misc 5)
        (* hal1000-slices 10 8
           (hal1000-slice 10)))))
  )

(define hal9000-params '((0.05 1000)
                         (0.05 1)
                         (0.10 10)
                         (0.10 1)
                         (0.10 0.05)
                         (0.10 0.02)
                         (0.10 0.01))
  )

(define (hal8000-downbin)
  (make-downbin hal9000-model
                `(hal8000
                  (* hal1000-modules 9 8 (hal1000 hal1000))
                  hal9000-misc)))

(define (hal7000-downbin)
  (make-downbin hal9000-model
                `(hal7000
                  (* hal1000-modules 9 7 (hal1000 hal1000))
                  hal9000-misc)))

(define hal9000-downbins (list (hal8000-downbin) (hal7000-downbin)))
  

(define (do-9000-report)
  (report-yields-for-params
   (hal9000-model)
   hal9000-params
   hal9000-downbins)
  )
(dis "run (do-9000-report) for report" dnl)

(define (all-9000-recs)
  (mergesort
   (compute-yield (hal9000-model) build-yield)
   (lambda(r0 r1)(< (length (car r0)) (length (car r1))))))

(define (best-9000-Pi) (make-Pi (simplify (cadar (tail 1 (all-9000-recs))))))
(define (base-9000-Pi) (make-Pi (simplify (cadar (all-9000-recs)))))

(define *tech-D0*      0.1 )
(define *tech-n*      30   )
(define *tech-alpha*   0.02)
(define *bl-area*    500   )

(dis "run (do-9000-yield-improvement-data) for data" dnl)

(define (do-9000-yield-improvement-data)
  (dump-to-file-steps
   (lambda(lalpha)(compute-yield-improvement
                   (base-9000-Pi)
                   (best-9000-Pi)
                   *tech-D0*
                   (exp lalpha)
                   *tech-n*))
   (log 0.01) (log 10)
   "9000-yield-over-alpha-fixed-D0.dat" 100)
  
  (dump-to-file-steps
   (lambda(lalpha)(compute-yield-improvement-at-fixed-area-yield
                   (base-9000-Pi)
                   (best-9000-Pi)
                   0.07
                   500
                   *tech-n*
                   (exp lalpha)
                   ))
   (log 0.01) (log 10)
   "9000-yield-over-alpha-fixed-bigdie.dat" 100)
  )


(define (make-9000-model D0 alpha) (list D0 alpha *tech-n*))

(define (make-f ym)
  (let* ((D0             (car ym))
         (alpha          (cadr ym))
         (n              (caddr ym))
         
         (D0p            (* n D0))
         (alphap         (* n alpha))
         (beta           (/ D0 alpha)))
    (lambda(D)(YieldModel.GammaDistPdf alphap beta D))
    )
  )

(define (make-F ym)
  (let* ((D0             (car ym))
         (alpha          (cadr ym))
         (n              (caddr ym))
         
         (D0p            (* n D0))
         (alphap         (* n alpha))
         (beta           (/ D0 alpha)))
    (lambda(D)(YieldModel.GammaDistCdf alphap beta D))
    )
  )


(define *9000-slop* .0001)

(define (dump-f-to-file params)
  (let* ((ym (make-9000-model (car params) (cadr params)))
         (F  (make-F ym))
         (f  (make-f ym))
         (a             (solve (make-target (exponentiate-arg F) *9000-slop*)
                               -400
                               +400))
         (b             (solve (make-target (exponentiate-arg F) (- 1 *9000-slop*))
                               -400
                               +400))
         (best-Pi       (best-9000-Pi))
         (base-Pi       (base-9000-Pi))
         (best-int      (lambda(D)(* (best-Pi D) (f D))))
         (base-int      (lambda(D)(* (base-Pi D) (f D))))
         )
    (dump-to-file (lambda (x) (f (exp x)))
                  a b
                  (string-append "xf_" (car params) "_" (cadr params) ".dat"))

    ;; dump the integrands
    (dump-to-file (lambda(x)(* (exp x) (best-int (exp x))))
                  a b 
                  (string-append "best_xi_" (car params) "_" (cadr params) ".dat"))
    (dump-to-file (lambda(x)(* (exp x) (base-int (exp x))))
                  a b 
                  (string-append "base_xi_" (car params) "_" (cadr params) ".dat"))))
                  

(dis "run (make-9000-f-data) for data" dnl)
(define (make-9000-f-data)
  (map dump-f-to-file hal9000-params))

(define (dump-Pi-to-file Pi fn)
  (dump-to-file (lambda(x)(Pi (exp x))) -3 +3
                fn)
  (dump-to-file (lambda(x)(* (exp x) (Pi (exp x)))) -3 +3
                (string-append "x" fn))

  )

(dis "run (dump-9000-Pi-data) for data" dnl)
(define (dump-9000-Pi-data)
  (dump-Pi-to-file (best-9000-Pi) "best9000_xpi.dat")
  (dump-Pi-to-file (base-9000-Pi) "base9000_xpi.dat"))


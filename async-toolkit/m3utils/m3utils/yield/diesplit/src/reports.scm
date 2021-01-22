(define (report-yield model yield-model)
  (let* ((yield-list (compute-yield model build-yield))
         (all-tags   (accumulate union
                                 '()
                                 (map car yield-list)))
         (results    (mergesort
                      (map
                       (lambda(alt)(decorate-yield alt model yield-model))
                       yield-list)
                      (lambda(a b) (< (cadr a) (cadr b)))))
         (worst      (car results))
         (best       (nth results (- (length results) 1)))
         )
    (cons worst best)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-report-yield
         name model lev min-scale max-scale all-tags yield-model)
  (let ((data (report-yield model yield-model)))
    ;;    (dis  "--- do-report-yield data " name " " lev " " (stringify data) dnl)
    (dis (Fmt.FN "%-30s : %6s% %8s %8s %6s% %8s %8s %7s% %7s% %6s %8s"
                 (let* ((uy (cadar data))
                        (ua (caar data))
                        (ur (/ ua uy)) ;; reqd fab area
                        
                        (iy (caddr data))
                        (ia (cadr data))
                        (ir (/ ia iy))
                        
                        (yi (/ (- iy uy) uy)) ;; yield imp.
                        (ri (/ (- ir ur) ur)) ;; fabarea inc.
                              )
                   (list (string-append (spaces lev) (stringify name))
                         
                         (fmt% uy) (fmtA ua) (fmtA ur)
                         (fmt% iy) (fmtA ia) (fmtA ir)
                         (fmt% yi)
                         (fmt% ri)
                         (fmtC max-scale)
                         (fmtA (* ia max-scale))
                         )

                       )
                 )
         dnl)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define header (string-append
             "         BLOCK                    UNIMP    UNIMP   UNIMP     IMP       IMP    UNIMP   YIELD    FAB/GDIE  MULT     TOT " dnl
             "         NAME                     YIELD    AREA   FAB/GDIE  YIELD     AREA  FAB/GDIE  IMPROV    DELTA             AREA" dnl 
             "----------------------------------------------------------------------------------------------------------------------"))

(define (spaces n)
  (let loop ((i n)
             (res ""))
    (if (= i 0) res (loop (- i 1) (string-append res "  ")))))

(define (fmt% x) ;; format %age
  (Fmt.LongReal (* 100 x) 'Fix 2 #f))

(define (fmtA x) ;; format area
  (Fmt.LongReal x 'Fix 2 #f))

(define (fmtC x) ;; format count
  (Fmt.LongReal x 'Fix 0 #f))

(define (report-all-yields model yield-model title)

  (dis dnl dnl title dnl header dnl)
  
  (define all-tags
    (accumulate union '() (map car (compute-yield model build-yield))))
  
  (define (recurse model lev min-scale max-scale)
    (if (null? model) ;; not sure we need this check
        #f 
        (let ((key (car model)))
          (cond ((eq? '* key)
                 (let ((nam (cadr model))
                       (N (caddr model))
                       (M (cadddr model))
                       (sub (caddddr model))
                       )
                   (recurse sub lev (* min-scale M) (* max-scale N))))
                
                ((eq? 'scale key)
                 (let ((mult (cadr model)))
                   (recurse (caddr model)
                            lev
                            (* min-scale mult)
                            (* max-scale mult))))
                
                ((and (symbol? key) (number? (cadr model)))

                 ;;(dis "area yield : " key dnl)

                 (do-report-yield
                  key model lev min-scale max-scale all-tags yield-model)
                 
                 (let ((area (cadr model)))
                   (if (member 'repair model)
                       (let* ((rc (nth model (+ 1 (find 'repair-cost model))))
                              (min-area-1 area)
                              (max-area-1 (* rc area))
                              (min-area (* min-scale area))
                              (max-area (* max-scale rc area)))
                         )
                       (let* ((min-area (* min-scale area))
                              (max-area (* max-scale area)))
                         )
                       )))
                         
                ((and (symbol? key) (list? (cadr model)))

                 ;;(dis "cells yield: " key dnl)

                 (do-report-yield
                  key model lev min-scale max-scale all-tags yield-model)

                 (map (lambda(s)(recurse s (+ lev 1) min-scale max-scale))
                      (cdr model))


                 )



                (else (error "unknown spec " (stringify model))))
          ))
   )

  (recurse model 0 1 1)
  'ok
  )

(define *n5-n* 32)

(define (ym D0 alpha)
  (lambda(A) (stapper A D0 *n5-n* alpha)))

(define params '((0.075 1) ;; B-E
                 (0.05 1)
                 (0.10 1)

                 (0.10 0.05) ;; Stapper
                 (0.10 0.02)
                 (0.10 0.01)
                 (0.075 0.05)
                 (0.075 0.02)
                 
                 (0.10 10000) ;; Poisson
                 (0.05 10000) 
                 ))

(define (report-yields-for-params model params)
  (let loop ((p params))
    (if (null? p)
        'ok
        (begin
          (report-all-yields model
                             (apply ym (car p))
                             (string-append "==================================================   D:" (number->string (caar p)) " alpha:" (number->string (cadar p)) "  ================================================="))
          (loop (cdr p))
          ))))

(report-yields-for-params (tfc-model) params)

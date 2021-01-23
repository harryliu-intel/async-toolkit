(define (compute-yield-results model yield-model)
  (let* ((yield-list (compute-yield model build-yield))
         (all-tags   (accumulate union
                                 '()
                                 (map car yield-list)))
         (results    (mergesort
                      (map
                       (lambda(alt)(decorate-yield alt model yield-model))
                       yield-list)
                      (lambda(a b) (< (cadr a) (cadr b))))))
    results
    ))
         

(define (report-yield model yield-model)
  (let* ((results    (compute-yield-results model yield-model))
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
    data
    )
  )

(define (do-report-single-red name all-reds this-red)
  (let* ((ay (cadr all-reds))
         (aa (car all-reds))
         (ar (/ aa ay))  ;; fab area req'd
         (ty (cadr this-red))
         (ta (car this-red))
         (tr (/ ta ty))
         (yi (/ (- ay ty) ty))
         (rd (/ (- tr ar) tr))
         )
    (dis (Fmt.FN "%-30s : %6s% %8s %7s% %8s %7s%"
                 (list name
                       (fmt% ty)
                       (fmtA ta)
                       (fmt% yi)
                       (fmtA tr)
                       (fmt% rd)
                       )
                 )
         dnl)
    )

  )

(define (do-report-single-downbin name top-bin down-bin)
  (let* ((tyu (cadar top-bin))
         (tyi (caddr top-bin))
         (dyu (cadar down-bin))
         (dyi (caddr down-bin))
         (deltayu (- dyu tyu))
         (deltayi (- dyi tyi))
         )
    (dis (Fmt.FN "%-30s : %6s% %6s% %6s% %6s%"
                 (list name
                       (fmt% dyu)
                       (fmt% deltayu)
                       (fmt% dyi)
                       (fmt% deltayi)
                       )
                 )
         dnl)
        
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define line "----------------------------------------------------------------------------------------------------------------------")

(define header (string-append
             "         BLOCK                    UNIMP    UNIMP    UNIMP    IMP       IMP     IMP     YIELD   MM^2/GD   MULT     TOT " dnl
             "         NAME                     YIELD    MM^2    MM^2/GD  YIELD     MM^2   MM^2/GD   IMPROV   DELTA             MM^2" dnl 
             line))

(define header2 (string-append
             "         SINGLE                    IMP      IMP     YIELD    IMP       IMP " dnl
             "       REDUNDANCY                 YIELD    MM^2    IMPROV   MM^2/GD   MM^2/GD " dnl 
             line))

(define header3 (string-append
             "        DOWNBIN                   UNMP     UNIMP    IMP      IMP     " dnl
             "                                  YIELD    DELTA   YIELD    DELTA  " dnl 
             line))

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

(define (report-all-yields model yield-model title downbin-list)

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
  (dis line dnl)

  ;; now do differential over base

  (let* ((yield-results (compute-yield-results model yield-model))
         (all-red-result (car (filter (lambda(r)(set-eq? (nth r 3) all-tags))
                                     yield-results)))
         )
    (dis line dnl
         header2 dnl)
    (do-report-single-red "**all-repairs**" all-red-result all-red-result)
                   
    (let loop ((p all-tags))
      (cond ((null? p) 'ok)
            (else
             (let* ((tags (set-diff all-tags (list (car p))))
                    (result (car (filter (lambda(r)(set-eq? (nth r 3) tags))
                                         yield-results))))

               (do-report-single-red (string-append "-"(stringify (car p)))
                                     all-red-result
                                     result)
               )
             (loop (cdr p)))))
    )
  (dis line dnl)

  ;; now do downbins
  (let ((top-results (report-yield model yield-model)))
    (dis line dnl header3 dnl )
         
    (do-report-single-downbin (stringify (car model))
                              top-results
                              top-results)
    (let loop ((p downbin-list))
      (if (null? p)
          'ok
          (begin
            (do-report-single-downbin (stringify (caar p))
                                      top-results
                                      (report-yield (car p) yield-model))
            (loop (cdr p)))
          )
      )
    )
  (dis line dnl)
  
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

(define (report-yields-for-params model params downbin-list)
  (let loop ((p params))
    (if (null? p)
        'ok
        (begin
          (report-all-yields model
                             (apply ym (car p))
                             (string-append "==================================================   D:" (number->string (caar p)) " alpha:" (number->string (cadar p)) "  =================================================")
                             downbin-list
                             )
          (loop (cdr p))
          ))))

(report-yields-for-params
 (tfc-model)
 params
 (list (eohalf-25t-model) (lrhalf-25t-model))
 )

(load "reports-code.scm")

(define *n5-n* 32)

(define *alpha* 0.07)

(define (match-stapper D) (YieldModel.Stapper 500 D 32 *alpha*))

(define stapper-d0
  (solve (make-target match-stapper
                      (YieldModel.BoseEinstein 500 0.055 32))
         0.01 0.15)
  )

(define params `((,stapper-d0 ,*alpha*)))

(define (lrhalf-25t-model)
  (make-downbin tfc-model
                `(lrhalf-25t
                  (* lr-spare 2 1
                     (lr-half (scale ,1/2 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define (lhalf-25t-model)
  (make-downbin tfc-model
                `(lhalf-25t
                  (* lr-spare 1 1
                     (lr-half (scale ,9/16 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define (lhalf-8/16-25t-model) ;; pipe 0 not needed
  (make-downbin tfc-model
                `(lhalf-8/16-25t
                  (* lr-spare 1 1
                     (lr-half (scale ,1/2 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define 9/16 (/ 9 16))
(define 6/16 (/ 6 16))
(define 6/8 (/ 6 8))
(define 15/16 (/ 15 16))

(define (lrhalf-16t-model)
  (make-downbin tfc-model
                `(lrhalf-16t
                  (* lr-spare 2 1
                     (lr-half (scale ,6/16 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define (lhalf-16t-model)
  (make-downbin tfc-model
                `(lhalf-16t
                  (* lr-spare 1 1
                     (lr-half (scale ,6/16 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define (15/16-pipe-model)
  (make-downbin tfc-model
                `(15/16-pipe
                  (* lr-spare 1 1
                     (lr-half (scale ,15/16 (to-lrhalf evenodd tm-core))))
                  misc
                  gpio)
                )
  )

(define (tfc-twodie-model)
  (let ((split-overhead-per-die 35))
    (make-downbin tfc-model
                  `(tfc-twodie (eo-half (scale ,1/2 evenodd))
                               (tc-half (scale ,1/2 tm-core))
                               (misc-half (scale ,1/2 misc))
                               (gpio-half (scale ,1/2 gpio))
                               (d2d ,split-overhead-per-die
                                    serdes-area ,split-overhead-per-die)
                               )
                  )
    )
  )

(define (twodie-halfpipe-model)
  (make-downbin tfc-twodie-model
                `(twodie-halfpipe
                  (half-half (scale ,1/2 (to-eohh eo-half tc-half)))
                  misc-half
                  gpio-half
                  d2d)))

(define (twodie-halfpipe-spare-model)
  (make-downbin tfc-twodie-model
                `(twodie-halfpipe-spare
                  (half-half (* lr-spare 2 1 (scale ,1/2 (to-eohh eo-half tc-half))))
                  misc-half
                  gpio-half
                  d2d)))

(define (twodie-6/8-model)
  (make-downbin tfc-twodie-model
                `(twodie-6/8
                  (half-half (scale ,6/8 (to-eohh eo-half tc-half)))
                  misc-half
                  gpio-half
                  d2d)))




(report-yields-for-params
 (tfc-model)

 params

 `(   ;; downbins
   ;;,(lrhalf-25t-model)
   ,(15/16-pipe-model)
   ,(lhalf-25t-model)
   ,(lhalf-8/16-25t-model)
   ,(lhalf-16t-model)
   ,(lrhalf-25t-model)
   ,(lrhalf-16t-model)
   ) 
 )

(report-yields-for-params
 (tfc-twodie-model)

 params

 `(,(twodie-halfpipe-model)
   ,(twodie-halfpipe-spare-model)
   ,(twodie-6/8-model))
 )

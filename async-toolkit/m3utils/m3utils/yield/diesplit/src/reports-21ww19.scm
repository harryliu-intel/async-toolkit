;;(define factor (/ 495 444.16))
(define factor (/ 580 444.16))

(define (tfc-twodie-model-ww19)
  (make-downbin tfc-twodie-model
                `(tfc-twodie-ww19 (big (scale ,factor tfc-twodie)))
                )
  )

(define (twodie-halfpipe-model-ww19)
  (make-downbin twodie-halfpipe-model
                `(twodie-halfpipe (big (scale ,factor twodie-halfpipe)))
                )
  )

(define (twodie-halfpipe-spare-model-ww19)
  (make-downbin twodie-halfpipe-spare-model
                `(twodie-halfpipe-spare (big (scale ,factor twodie-halfpipe-spare)))
                )
  )

(define (twodie-6/8-model-ww19)
  (make-downbin twodie-6/8-model
                `(twodie-6/8 (big (scale ,factor twodie-6/8)))
                )
  )

(define (twodie-1.15-model-ww19)
  (make-downbin twodie-1.15-model
                `(twodie-bloated (big (scale ,factor tfc-twodie-bloated)))
                )
  )



(define (report-twodie) 
  (report-yields-for-params
   (tfc-twodie-model-ww19)
   
   params
   
   `(,(twodie-halfpipe-model-ww19)
     ,(twodie-halfpipe-spare-model-ww19)
     ,(twodie-6/8-model-ww19)
     ,(twodie-1.15-model-ww19))
   )
  )

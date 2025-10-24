; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(load "reports-code.scm")
(load "defs-21ww07.scm")

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
  (let ((split-overhead-per-die 46))
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


(define (twodie-1.15-model)
  (make-downbin tfc-twodie-model
                '(tfc-twodie-bloated (big (scale 1.15 tfc-twodie)))
                )
  )

(define (report-onedie)
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
  )

(define (report-twodie) 
  (report-yields-for-params
   (tfc-twodie-model)
   
   params
   
   `(,(twodie-halfpipe-model)
     ,(twodie-halfpipe-spare-model)
     ,(twodie-6/8-model)
     ,(twodie-1.15-model))
   )
  )

;;(dis "*spare-mac* " (stringify *spare-mac* )dnl)

;;(report-twodie)

;;(set! *spare-mac* (not *spare-mac*))

;;(dis "*spare-mac* " (stringify *spare-mac* )dnl)

;;(report-twodie)

;;(set! *spare-mac* (not *spare-mac*))


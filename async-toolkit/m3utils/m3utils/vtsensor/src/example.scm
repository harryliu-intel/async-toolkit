; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(require-modules "m3" "display")

(define (get-points) (TriData.TraverseData recs "tttt"))
               
(define (eval-estimator) (TriOsc.EvalEstimator temp-meshes *samples* *k* pts 1))


(define (partial-1 a b rel-step)
  ;; compute a single partial difference
  (list (assoc*- 'temp (/ rel-step) b a)
        (assoc2*- 'V (/ rel-step) b a)))

(define (partial-deriv test-point temp-meshes samples k rel-step)

  (define (estimate p)
    (TriOsc.Estimate p temp-meshes samples k))

  (let* ((e0 (estimate test-point))
         (px (make-advances test-point rel-step))
         (ex (map estimate px))
         (f (lambda(e)(partial-1 e0 e rel-step)))
         )

    (map f ex)

    )
  )


(define (assoc*- key m a b)
  (cons key
        (* m
           (- (cdr (assoc key a))
              (cdr (assoc key b))))))

(define (assoc2*- key m a b)
  (define (mult x) (* m x))
  
  (cons key
        (let* ((x (map mult (map -
                                 (map cdr (cdr (assoc key a)))
                                 (map cdr (cdr (assoc key b))))))
               (y (map car (cdr (assoc key a)))))
          (map cons y x))))



(define (make-advances p rel-step)
  ;; make all the steps in every direction
  
  (define (modify-point p rel-step k)
    ;; modify kth dimension of p by multiplying by (+ 1 rel-step)
    (let loop ((pp p)
               (i 0)
               (res '()))
      (cond ((null? pp) (reverse res))
            ((= i k)   (loop (cdr pp) (+ i 1)
                             (cons (* (+ 1 rel-step) (car pp))
                                   res)))
            (else (loop (cdr pp) (+ i 1) (cons (car pp) res))))))
                        

  (let loop ((n (length p))
             (res '()))
    (if (= n 0)
        res
        (loop (- n 1)
              (cons (modify-point p rel-step (- n 1)) res)))))


;; stuff that's actually run

(define *cal-temps* '(0 125))
(define *mesh-size* 100)
(define *samples*   500)
(define *k* 0)
(define *the-corner* "tttt")

(define test-point '(53.0008e6 87.8511e6 216.8556e6))

(define recs        '())
(define osc         '())
(define temp-meshes '())
(define pts         '())
(define p           '())

(define (do-it)
  
  (set! recs
        (let* ((rd (FileRd.Open "RING_OSCILLATOR.json"))
               (res (TriData.LoadJson rd)))
          (Rd.Close rd)
          res))

  (set! osc (TriOsc.Calibrate *the-corner* recs *cal-temps*))

  (set! temp-meshes (TriOsc.MakeMeshes `((0 . ,osc) (1 . ,osc) (2 . ,osc))
                                       *cal-temps*
                                       *mesh-size*))
  


  (set! pts (get-points))
  (set! p (TriOsc.Estimate test-point temp-meshes *samples* *k*))
)



(define (do-single)
  (set! (TriOsc.Estimate test-point temp-meshes *samples* *k*)))

(dis dnl "*** run (do-it) or (do-single) for a demo" dnl dnl)

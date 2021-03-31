;;
;; from
;; https://interferencetechnology.com/understanding-inductance-real-world/
;;

(define (A h w)
  (* h (log (/ (+ h (sqrt (+ (* h h) (* w w))))
               w))))

(define (B h w)
  (* w (log (/ (+ w (sqrt (+ (* h h) (* w w))))
               h))))


(define (C h w r0)
  (+
   (* h (log (/ (* 2 h) r0)))
   (* w (log (/ (* 2 w) r0)))))

(define pi (* 4 (atan 1)))

(define m0 (* 4 1e-7 pi))

(define (L h w r0)
  (* (/ m0 pi)
     (+ (* -2 (+ w h))
        (*  2 (sqrt (+ (* h h) (* w w))))
        (- (A h w))
        (- (B h w))
        (C h w r0))))


;; Eric's omega function
(define (omega rise-time)
  (/ (* 2 pi) rise-time))

(define omega0 (omega 50e-12)) ;; 50 ps rise time



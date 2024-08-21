;;
;; here we can define some constants or other useful utility thingies
;; they will get pulled in by the schemagraph program and you can then
;; call them from the scheme function evaluation code
;;

(define Kcycle 5.0)

(define (Carnot-efficiency Tc Th) ;; temps on thermodynamic scale (K or R)
  (if (>= Tc Th)
      1
      (/ Tc (- Th Tc))))

(define (achievable-efficiency TCc TCh) ;; temps in C
  (if (>= TCc TCh)
      1
      (let ((eta (- 0.880 (* 0.00248 (- TCh TCc)))) ;; empirical R717 results
            (Tc (+ 273.15 TCc))
            (Th (+ 273.15 TCh)))
        (* eta (Carnot-efficiency Tc Th)))))

(define (cooled-energy E TCc TCh) ;; temps in Celsius
  (* E (+ 1 (/ 1 (achievable-efficiency TCc TCh)))))


(define *base-temp* 50) ;; Celsius

(define *energy-cost-factor* 4.1677e-8)

(define *silicon-cost-factor* 2.767e-14)

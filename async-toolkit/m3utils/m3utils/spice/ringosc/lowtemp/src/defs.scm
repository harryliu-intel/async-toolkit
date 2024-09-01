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

(define (range-penalty-factor x0 delta-x)
  (lambda(x)
    (+ 1 (exp (/ (- x x0) delta-x)))))

(define (range-penalizer x0 delta-x)
  (lambda(value x)
    (* value ((range-penalty-factor x0 delta-x) x))))

(define (double-range-penalizer lo hi delta-x)
  (let ((lo-p (range-penalizer lo (- delta-x)))
        (hi-p (range-penalizer hi (+ delta-x))))
    (lambda(value x)
      (lo-p (hi-p value x) x))))

(define *temp-penalizer* (double-range-penalizer -50 (- *base-temp* 1) 1))





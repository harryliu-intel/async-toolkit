(require-modules "basic-defs" "display" "mergesort" "time")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple polynomials
;;
;; Mika Nystrom <mika.nystroem@intel.com>
;; Christmas 2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; basic definitions
;;
;; we represent a polynomial (sparsely) as a sum of monomials
;;
;; a monomial is written (t <a> <k>) an represents the expression
;;
;; $a x^k$
;;

(define (term? x)
  (and (list x) (eq? (car x) 't)))

(define (make-number-term n)
  (list 't n 0))

(define (make-power-term k)
  (list 't 1 k))

(define (term-factor t) (cadr t))

(define (term-exponent t) (caddr t))

;;;;;;;;;;;;;;;;;;;;
;;
;; constants
;;

(define 1-term (make-number-term 1))
(define -1-term (make-number-term -1))
(define 0-term (make-number-term 0))

(define (term-0? a)
  (and (term? a) (= 0 (term-factor a))))

(define (term-1? a)
  (and (term? a) (= 0 (term-exponent a)) (= 1 (term-factor a))))

;;;;;;;;;;;;;;;;;;;;
;;
;; basic operations
;;

(define (op? op)
  (lambda (a)
    (cond ((not (pair? a)) #f)
          ((eq? op (car a)) #t)
          (else #f)))
  )

(define +? (op? '+))

(define (*-term a b)
  (cond ((term-0? a) 0-term)
        ((term-0? b) 0-term)
        ((and (term? a) (term? b))
         (list 't
               (* (term-factor a) (term-factor b))
               (+ (term-exponent a) (term-exponent b))))
        ((+? b) (*-+-term a b))
        (else (*-+-term b a))))

(define (*-+-term x y)
  ;; y is a + expression
  (accumulate +-term
              (make-number-term 0)
              (map (lambda (yi) (*-term x yi)) (cdr y))))

(define (+-term a b)
  (cond ((term-0? a) b)

        ((term-0? b) a)

        ((and (term? a) (term? b)) (+-term a (list '+ b)))

        ((term? b) (+-term b a))

        ;; get here and b is a +-expression

        (else (+-term-term a b))))

(define (+-term-term a b)
  (if (term? a)
      
      (let loop ((p (cdr b))
                 (res '()))

        (cond ((null? p) ;; end of list, no match
               (cons '+ (cons a res)))
              
               ((= (term-exponent a) (term-exponent (car p))) ;; match
                (cons '+ (cons (list 't (+ (term-factor a) (term-factor (car p)))
                                     (term-exponent a))
                               (append (cdr p) res))))

               (else (loop (cdr p) (cons (car p) res)))))

      (accumulate +-term a (cdr b)))
  )

(define (^-term a k) ;; exponentiation by squaring
  (let loop ((n    k)
             (p    a)
             (r    (make-number-term 1)))
    (cond ((= 0 n) r)
          ((= 0 (modulo n 2))
           (loop (div n 2)
                 (*-term p p)
                 r))
          ((= 1 (modulo n 2))
           (loop (div n 2)
                 (*-term p p)
                 (*-term r p)))
          )))

;;;;;;;;;;;;;;;;;;;;
;;
;; higher level ops
;;

(define (sum-term lo hi f)
  (let loop ((s (make-number-term 0))
             (i lo))
    (if (= i hi)
        (+-term (f i) s)
        (loop (+-term (f i) s)
              (+ i 1)))))

;;;;;;;;;;;;;;;;;;;;
;;
;; printing, cleanup, output
;;

(define (canonical-order-term x)
  (if (term? x)
      x
      (cons '+ (mergesort
                (cdr x)
                (lambda (x y) (> (term-exponent x) (term-exponent y)))))))

(define (term-2-yield x)
  (if (term? x)
      (list '* (term-factor x) (list 'Y (term-exponent x)))
      (cons '+
            (map term-2-yield (cdr x)))))

(define (term-2-latex x)
  (if (term? x)
      (string-append
       (number->string (term-factor x))
       " \\, x^{"
       (number->string (term-exponent x))
       "}")
      (infixize (map term-2-latex (cdr x)) " + ")
      )
  )

(define (eval-yield x Y)
  (eval (term-2-yield x)))





        

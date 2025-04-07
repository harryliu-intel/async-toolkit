

(require-modules "fold.scm")

(define *big-m1* (BigInt.New -1))
(define *big-0*  (BigInt.New  0))
(define *big-1*  (BigInt.New  1))
(define *big-2*  (BigInt.New  2))
(define *big-tc* (rttype-typecode *big-1*))

(define (bigint? x)
  (cond ((null? x) #f)
        ((pair? x) #f)
        ((= *big-tc* (rttype-typecode x)) #t)
        (else #f)))

(define (make-big-op real-op big-op . init-val)

  (define (the-op a  b)
    (if (and (number? a) (number? b))
        (real-op a b)
        (big-op (if (bigint? a) a (BigInt.New a))
                (if (bigint? b) b (BigInt.New b)))))
  
  (lambda x
    (if (null? (cdr x))
        (if (not (null? init-val))
            (the-op (car init-val) (car x))
            (car x))
        (fold-left
         the-op
         (car x)
         (cdr x))
        )
    )
  )

(define (make-big-binop real-op big-op)
  (lambda(a . blst)
    (if (null? b)
    (if (and (number? a) (number? b))
                  (real-op a b)
                  (big-op (if (bigint? a) a (BigInt.New a))
                          (if (bigint? b) b (BigInt.New b)))))))
  
(let () ;; don't pollute the global environment
  
  (define + (make-big-op + BigInt.Add))
  (define * (make-big-op * BigInt.Mul))
  (define - (make-big-op - BigInt.Sub 0))
  (define / (make-big-op / BigInt.Div 1))
      
  (define-global-symbol `big+ +)
  (define-global-symbol `big- -)
  (define-global-symbol `big* *)
  (define-global-symbol `big/ /)

  )

(define (make-big x)
  (cond ((null? x) (error "make-big of nil"))
        ((bigint? x) x)
        (else (BigInt.New x))))

(define (big<< x sa)
  (let ((pow2 (BigInt.Pow *big-2* (make-big sa))))
    (BigInt.Mul (make-big x) pow2)))

(define (big>> x sa)
  (let ((pow2 (BigInt.Pow *big-2* (make-big sa))))
    (BigInt.Div (make-big x) pow2)))


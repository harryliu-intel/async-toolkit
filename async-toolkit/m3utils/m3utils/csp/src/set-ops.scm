(define cartesian-product 
  (lambda (xs ys)
    (apply append (map (lambda (x) (map (lambda (y) (list x y)) ys)) xs))))

(define (set-diff lst0 lst1)
  (filter (lambda(x)(not (member? x lst1))) lst0))

(define (set-union lst0 lst1)
  (uniq eq? (append lst0 lst1)))

(define (set-intersection lst0 lst1)
  (filter (lambda(x)(member? x lst1)) lst0))
  
(define (multi lst) ;; multiply defined items in a list
  (let loop ((res '())
             (p lst))
    (cond ((null? p) (uniq eq? res))
          ((member? (car p) (cdr p))
           (loop (cons (car p) res) (cdr p)))
          (else (loop res (cdr p))))
    )
  )

(define (count-in item lst)
  (let loop ((res 0)
             (p lst))
    (cond  ((null? p) res)
           ((eq? item (car p)) (loop (+ res 1) (cdr p)))
           (else (loop res (cdr p))))
    )
  )



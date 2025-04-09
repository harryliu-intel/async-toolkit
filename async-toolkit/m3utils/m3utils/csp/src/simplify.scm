(define (compound-stmt? s)
  (and (list? s)
       (not (null? s))
       (or (eq? (car s) 'parallel) (eq? (car s) 'sequence))))

(define (simplify-one-stmt s)
  ;; all that this does is flattens out parallel and sequence statements
  (cond ((compound-stmt? s)   (simplify-compound-stmt s))
        (else s)))

(define (simplify-compound-stmt s)

    (cond
     ((= (length (cdr s)) 0)
      'skip)
     
     ((= (length (cdr s)) 1)
      (cadr s))

     (else 
      (let loop ((p    (cdr s))
                 (res  '()))
        
        (if (null? p)
            
            (cons (car s) (reverse res)) ;; base case
            
            (let ((next (car p)))
              
              (cond ((and (compound-stmt? next)
                          (eq? (car next) (car s)))
                     
                     ;; same type of statement, just splice in the args
                     ;;                    (dis " splicing next  : " next dnl)
                     ;;                    (dis " splicing cdr next : " (cdr next) dnl)
                     
                     (loop (cdr p)
                           (append (reverse (cdr next)) res))
                     )
                    
                    
                    ((eq? 'skip (get-stmt-type next))
                     (loop (cdr p) res))
                    
                    (else
                     (loop (cdr p) (cons next res)))
                    )
              )
            )
        )
      )
     )
    )

(define (simplify-stmt stmt)
  (visit-stmt stmt simplify-one-stmt identity identity)
  )

  

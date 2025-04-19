(define (compound-stmt? s)
  (and (list? s)
       (not (null? s))
       (or (eq? (car s) 'parallel) (eq? (car s) 'sequence))))

(define (simplify-one-stmt s)
  (if #f (dis "simplify-one-stmt s : " s dnl))
  
  ;; all that this does is flattens out parallel and sequence statements
  (cond ((compound-stmt? s)   (simplify-compound-stmt s))
        ((eval? s) (simplify-eval s))
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
                    
                    
                    ((skip? next)
                     ;; this coding allows pseudocode to be simplified
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


(define (eval? s) (and (pair? s) (eq? 'eval (car s))))

(define (simplify-eval s)
  (let* ((expr (cadr s)))
    (if (and (call-intrinsic? expr)
             (eq? 'assert (cadr expr))
             (eq? #t (caddr expr)))
        ;; for now, just get rid of assert(#t)
        (begin
          (dis "simplify-eval : removing : " s dnl)
         'skip
         )
        s
        )
    )
  )
         

(define (compound-stmt? s)
  (and (list? s)
       (not (null? s))
       (or (eq? (car s) 'parallel) (eq? (car s) 'sequence))))

(define (guarded-stmt? s)
  (and (list? s)
       (not (null? s))
       (member (car s) '(if nondet-if do nondet-do))))

(define (loop-stmt? s)
  (and (list? s)
       (not (null? s))
       (or (eq? (car s) 'parallel-loop) (eq? (car s) 'sequential-loop))))

(define (simplify-one-stmt s)
  ;; all that this does is flattens out parallel and sequence statements
  (cond ((compound-stmt? s)   (simplify-compound-stmt s))
        ((guarded-stmt? s)    (simplify-guarded-stmt s))
        ((loop-stmt? s)       (simplify-loop-stmt s))
        ((waiting-if-stmt? s) (simplify-waiting-if-stmt s))
        (else s)))

(define (simplify-loop-stmt s)
  (list (car s)
        (cadr s)
        (caddr s)
        (simplify-stmt (cadddr s))))

(define (simplify-guarded-stmt s)
  (cons (car s)
        (map
         (lambda(gc)
           (let ((guard   (car gc))
                 (command (cadr gc)))
             (list guard (simplify-stmt command))))
         (cdr s))))

(define (simplify-waiting-if-stmt s)
  (cons (car s)
        (map
         (lambda(cl)
           (let ((dummy     (car cl))
                 (sens      (cadr cl))
                 (guardtext (caddr cl))
                 (cmd       (cadddr cl)))
             (list dummy sens (simplify-stmt guardtext) (simplify-stmt cmd))
             )
           )
         (cdr s))))
        
(define (simplify-compound-stmt s)
  (cond
   ((= (length (cdr s)) 0)
    'skip)
   
   ((= (length (cdr s)) 1)
    (simplify-stmt (cadr s)))

   (else 
    (let loop ((p    (cdr s))
               (res  (list (car s))))
      
      (if (null? p)
          
          (reverse res) ;; base case
          
          (let ((next (simplify-stmt (car p))))
            
            (cond ((and (compound-stmt? next)
                        (eq? (car next) (car s)))
                   
;; same type of statement, just splice in the args
;;                    (dis " splicing next  : " next dnl)
;;                    (dis " splicing cdr next : " (cdr next) dnl)
                
                   (loop (cdr p)
                         (append (reverse (cdr next)) res))
                   )
                  
                  
                  ((eq? 'skip (get-stmt-type (car p)))
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

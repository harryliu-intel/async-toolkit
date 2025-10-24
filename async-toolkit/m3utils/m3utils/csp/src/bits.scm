(define (bits? x) (and (pair? x) (eq? 'bits (car x))))

(define (get-bits-expr x)(cadr x))

(define (get-bits-min x) (caddr x))

(define (get-bits-max x) (cadddr x))
  

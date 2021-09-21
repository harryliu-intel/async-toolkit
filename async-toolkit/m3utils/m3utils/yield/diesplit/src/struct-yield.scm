(define (build-yield area . optional)
  (let ((ram-area          0)
        (channel-area      0)
        (serdes-area       0)
        (channel-nfactor-ratio   (/ 9 32))
        (repaired-ram-k   .6);;.6 ;;area includes ram-area
        (serdes-k         .8)
        (repair           'dummy)
        (repair-cost      'dummy)
        )
    
    (do-overrides! optional (current-environment)) 

    (let* ((logic-area
            (- area ram-area channel-area serdes-area))

           (logic-yield
            (area-yield logic-area))
           
           (ram-repaired-yield
            (area-yield (* ram-area repaired-ram-k)))

           (serdes-yield
            (area-yield (* serdes-area serdes-k)))
           
           (channel-yield
            (area-yield (* channel-area channel-nfactor-ratio)))
           )
      
      (modules-yield logic-yield
                     ram-repaired-yield
                     channel-yield
                     serdes-yield)
      
      ))
  )


(define (find mem lst)
  (let loop ((p lst)
        (i 0))
    (cond ((null? p) #f)
          ((eq? (car p) mem) i)
          (else (loop (cdr p) (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;

(define (area-repaired-yield s f)
  (let ((r   (nth s (+ 1 (find 'repair s))))
        (rc  (nth s (+ 1 (find 'repair-cost s)))))
    (list (list (car s))
          (scale-area (apply f (cdr s))
                      (* (- 1 r) (+ 1 rc))))))

(define (area-unrepaired-yield s f)
  (list '() (apply f (cdr s))))

;;;;;;;;;;;;;;;;;;;;

(define (compute-area-yields struct f)
  (let ((repidx (find 'repair struct)))
    (if repidx (list (area-unrepaired-yield          struct f)
                     (area-repaired-yield            struct f))
        (list (area-unrepaired-yield struct f)))))

;;;;;;;;;;;;;;;;;;;;

(define (redundant-yield-lst nam lst N M)
  (let loop ((p    lst)
             (res '()))
    (if (null? p)
        res
        (loop (cdr p)
              (cons 
               (list
                (cons nam (caar p))
                (redundant-yield (cadar p) N M))
               (cons
                (list (caar p)
                      (scale-area (cadar p) M))
                res))))))

   
;;;;;;;;;;;;;;;;;;;;

(define (caddddr x) (car (cddddr x)))

(define (define-yield! key yld)
  ;; no-op
  yld)

(define *r* 'hi)
(define *l* 'hi)
(define *ltags* 'hi)
(define *lpoly* 'hi)
(define *rtags* 'hi)
(define *rpoly* 'hi)


(define (combine-lst lst)
  (let ((first (car lst)))
    (if (null? (cdr lst))
        first
        (apply append
        (map (lambda(f)
               (let ((ltags (car f))
                     (lpoly (cadr f)))
                 (set! *l*     f)
                 (set! *ltags* ltags)
                 (set! *lpoly* lpoly)
                 (map (lambda(r)
                        (let ((rtags (car r))
                              (rpoly (cadr r)))
                          (set! *r*     r)
                          (set! *rtags* rtags)
                          (set! *rpoly* rpoly)
                          (list (append ltags rtags)
                                (modules-yield lpoly rpoly))))
                      (combine-lst (cdr lst)))))
             first)))))

(define (compute-yield-lst lst f)
  (let ((sub (map (lambda(x)(compute-yield x f)) lst)))

    (combine-lst sub)
    ))

(define (scale-yield-lst lst by)
  (map (lambda(item)(list (car item) (scale-area (cadr item) by))) lst))

(define (compute-yield struct f)
  (let ((key (car struct)))
    ;;(dis "doing " key dnl)
    (cond ((eq? '* key)
           (let ((nam (cadr struct))
                 (N (caddr struct))
                 (M (cadddr struct))
                 (sub (caddddr struct))
                 )
             (if (= N M)
                 (scale-yield-lst (compute-yield sub f) N)
                 (redundant-yield-lst nam (compute-yield sub f) N M))))

          ((eq? 'scale key)
           (scale-yield-lst (compute-yield (caddr struct) f)
                            (cadr struct)))
          
          ((and (symbol? key) (number? (cadr struct)))
           (define-yield! key (compute-area-yields struct f)))

          ((and (symbol? key) (list? (cadr struct)))
           (define-yield! key (compute-yield-lst (cdr struct) f)))

          (else (error "unknown spec " (stringify struct))))))
                 
(define (find-node model name)
  (cond ((null? model) #f)

        ((eq? name (car model)) model)

        ((eq? '* (car model))
         (if (eq? name (cadr model))
             model
             (find-node (caddddr model) name)))
             
        (else
         (let loop ((p (cdr model)))
           (cond ((null? p) #f)
                 ((list? (car p))
                  (let ((try (find-node (car p) name)))
                    (if try
                        try
                        (loop (cdr p))))))))))

(define *model* #f)
(define *cdr-model* #f)

(define (compute-total-area model optional)
  (if (null? model)
      0
      (let ((key (car model)))
        (cond ((eq? '* key)
               (let ((nam (cadr model))
                     (N (caddr model))
                     (M (cadddr model))
                     (sub (caddddr model))
                     )
                 (*
                  (if (member nam optional) N M)
                  (compute-total-area sub optional))))
              
              ((eq? 'scale key)
               (* (cadr model) (compute-total-area (caddr model) optional)))

              ((and (symbol? key) (number? (cadr model)))
               (let ((area (cadr model)))
                 (if (member key optional)
                     (let ((rc (nth model (+ 1 (find 'repair-cost model)))))
                       (* (+ 1 rc) area))
                     area)))

              ((and (symbol? key) (list? (cadr model)))
               (apply + (map (lambda(s)(compute-total-area s optional))
                             (cdr model))))

              (else (error "unknown spec " (stringify model)))))))


(define (get-kind-area model kind)
  ;; extract a particular kind of area from leaf
  ;; returns total area if kind is null
  ;; returns 0 if kind non null and no such area in leaf
  (cond ((null? kind) (cadr model))
        ((member kind model) (nth model (+ 1 (find kind model))))
        (else 0)))

(define (compute-total-area-of-kind model optional kind)
  ;; compute area of a particular kind of area
  (if (null? model)
      0
      (let ((key (car model)))
        (cond ((eq? '* key)
               (let ((nam (cadr model))
                     (N (caddr model))
                     (M (cadddr model))
                     (sub (caddddr model))
                     )
                 (*
                  (if (member nam optional) N M)
                  (compute-total-area-of-kind sub optional kind))))
              
              ((eq? 'scale key)
               (* (cadr model) (compute-total-area-of-kind (caddr model) optional kind)))

              ((and (symbol? key) (number? (cadr model)))
               (let ((area (get-kind-area model kind)))
                 (if (member key optional)
                     (let ((rc (nth model (+ 1 (find 'repair-cost model)))))
                       (* (+ 1 rc) area))
                     area)))

              ((and (symbol? key) (list? (cadr model)))
               (apply + (map (lambda(s)(compute-total-area-of-kind s optional kind))
                             (cdr model))))

              (else (error "unknown spec " (stringify model)))))))
              

(define (make-downbin model spec)
  (let ((proto (model)))
    (cond ((null? spec) '())

          ((symbol? spec) (find-node (model) spec))

          ((list? spec)
           (let ((key (car spec)))
             (cond ((eq? 'scale key)
                    ;; scale factor -- return it but edit subspec
                    (list 'scale
                          (cadr spec)
                          (make-downbin model (caddr spec))))

                   ((eq? '* key)
                    ;; redundant thingy
                    (list '*
                          (cadr spec)
                          (caddr spec)
                          (cadddr spec)
                          (make-downbin model (caddddr spec))))
                          
                    ((and (symbol? key) (number? (cadr spec)))
                    ;; this is an area
                    spec)

                   ((symbol? key)
                    ;; this is a list of modules
                    (cons key (map (lambda(q)(make-downbin model q))
                                   (cdr spec))))
                   
                   (else (error "unknown spec " (stringify spec)))
                   
                   );;dnoc
             );;tel
           )
          
          (else (error "unknown spec " (stringify spec)))
          );;dnoc
    )
  )
           
           
(define (union a b)
  (let loop ((p   a)
             (res b))
    (cond ((null? p) res)
          ((member? (car p) b) (loop (cdr p) res))
          (else (loop (cdr p) (cons (car p) res))))))

(define (set-diff a b)
  (let loop ((p a)
             (res '()))
    (cond ((null? p) res)
          ((member? (car p) b) (loop (cdr p) res))
          (else (loop (cdr p) (cons (car p) res))))))

(define (set-eq? a b)
  (if (not (= (length a) (length b)))
      #f
      (let loop ((p a))
        (cond ((null? p) #t)
              (else (if (member? (car p) b)
                        (loop (cdr p))
                        #f))))))

(define (decorate-yield yr model ym)
  (let* ((config (car yr))
         (poly   (cadr yr))
         (latex  (Polynomial.LaTeXFmt poly))
         (area   (compute-total-area model config))
         (y      (Mpfr.GetLR (eval-yield poly ym) 'N)))
    (cons area (cons y (cons latex yr)))))

(define (ym D0 alpha)
  (lambda(A) (stapper A D0 *n5-n* alpha)))


        


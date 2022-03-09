(require-modules "m3" "types.scm" "display")

(define (schemify-wrap x)
   (if (closest-opped-supertype (rttype-typecode x))
       (cons 'm3 (schemify x))
       x)
   )

(define (schemify-fp x)
  (cond ((pair? x)
         (cons (schemify-fp (car x)) (schemify-fp (cdr x))))
        (else
         (let ((try (schemify-wrap x)))
           (if (eq? try x) x (schemify-wrap try))))))


         
(define (test1) ((obj-method-wrap *lib* 'LibertyComponentChildren.Private) 'children))

(define (test2) ((obj-method-wrap ((obj-method-wrap *lib* 'LibertyComponentChildren.Private) 'children) 'LibertyComponentSeq.T) 'get 0))


;; example code, will this work?
(define (not-done)
  (find-grammar-object
 *lib*
 (lambda(x) (and (is-grammar-type x 'LibertySimpleAttr.T)
                 (equal? (has-field x 'ident) "time_unit"))))
  )

(define (test3)
  (RTType.IsSubtype (rttype-typecode *lib*) (lookup-typecode "LibertyComponent.T") )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *comp-tc* (lookup-typecode "LibertyComponent.T"))

(define (is-comp? c) (RTType.IsSubtype (rttype-typecode c)
                                          *comp-tc*))

(define (get-comp-children c)
  (if (is-comp? c)
      (let* ((wrap (obj-method-wrap c 'LibertyComponentChildren.Private))
             (seq  (wrap 'children))
             (wseq (obj-method-wrap seq 'LibertyComponentSeq.T))
             (size (wseq 'size)))
        (let loop ((lst '())
                   (i     0))
          (if (= i size)
              (reverse lst)
              (loop
               (cons (wseq 'get i) lst)
               (+ i 1)))))
      '()
      ))

(define (visit-comps c f)
  ;; apply f to all levels of syntax structure in postorder
  (let ((children (get-comp-children c)))
    (map (lambda(q)(visit-comps q f)) children)
    (f c)))

(define (make-counter)
  (let* ((cnt 0)
         (counter-proc (lambda()(let ((q cnt)) (set! cnt (+ cnt 1)) q))))
    counter-proc)
  )

(define (dummy-arg-proc f)
  (lambda(x)(f)))

(define (type-filter lib m3-type)
  (let* ((list '())
         (target-tc (lookup-typecode (stringify m3-type)))
         (visitor (lambda(x)
                    (if (= (rttype-typecode x) target-tc)
                        (set! list (cons x list))))))
    (visit-comps lib visitor)
    list))

(define (subtype-filter lib m3-type)
  (let* ((list '())
         (target-tc (lookup-typecode (stringify m3-type)))
         (visitor (lambda(x)
                    (if (RTType.IsSubtype (rttype-typecode x) target-tc)
                        (set! list (cons x list))))))
    (visit-comps lib visitor)
    list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Filtering
;;
;; a filter that succeeds returns the input object
;; a filter that fails returns #f
;;

(define (type-filter-proc m3-type)
  (let* ((target-tc (lookup-typecode (stringify m3-type)))
         (res (lambda(x)(if (= (rttype-typecode x) target-tc)
                            x
                            #f))))))

(define (subtype-filter-proc m3-type)
  (let* ((target-tc (lookup-typecode (stringify m3-type)))
         (res (lambda(x)(if (RTType.IsSubtype (rttype-typecode x) target-tc)
                            x
                            #f))))))

(define (and-filters . b)
  (lambda (x)
    (let loop ((p b))
      (cond ((null? b) x)
            (((car b) x) (loop (cdr b)))
            (else #f)))))

(define (or-filters . b)
  (lambda (x)
    (let loop ((p b))
      (cond ((null? b) #f)
            (((car b) x) x)
            (else (loop (cdr b)))))))

(define (visit-filtered-comps c filter f)
  ;; apply f to all levels of syntax structure in postorder that match filter
  (let ((children (get-comp-children c)))
    (map (lambda(q)(visit-comps q f)) children)
    (if (filter c) (f c))))

(define (get-syntax-field x tc fn)
  ;; requires that we have stubs for tc and that
  ;; x is an instance of a subtype of tc
  (modula-type-op tc 'get-field x fn))

(define (get-concrete-field x fn)
  ;; requires that we have stubs for the concrete type
  (modula-type-op (rttype-typecode x) 'get-field x fn))

(define (field-equal?-filter-proc tc fn val)
  (lambda(x)
    (and (RTType.IsSubtype (rttype-typecode x) tc)
         (equal? (get-syntax-field x tc fn) val))))







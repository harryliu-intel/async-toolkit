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

(define (force-string x)
  (if (string? x) x (stringify x)))

(define (type-filter lib m3-type)
  (let* ((list '())
         (target-tc (lookup-typecode (force-string m3-type)))
         (visitor (lambda(x)
                    (if (= (rttype-typecode x) target-tc)
                        (set! list (cons x list))))))
    (visit-comps lib visitor)
    list))

(define (subtype-filter lib m3-type)
  (let* ((list '())
         (target-tc (lookup-typecode (force-string m3-type)))
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
  (let* ((target-tc (lookup-typecode (force-string m3-type)))
         (res (lambda(x)(if (= (rttype-typecode x) target-tc)
                            x
                            #f))))
    res))

(define (subtype-filter-proc m3-type)
  (let* ((target-tc (lookup-typecode (force-string m3-type))))
    (lambda(x)(if (RTType.IsSubtype (rttype-typecode x) target-tc)
                  x
                  #f))))

(define (equality-filter eq? value)
  (lambda (x) (if (eq? x value) x #f)))

(define (equal?-filter value)
  (equality-filter equal? value))

(define (eq?-filter value)
  (equality-filter eq? value))

(define (=-filter value)
  (equality-filter = value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ops on filters themselves
;;

(define (and-filters . b)
  (lambda (x)
    (let loop ((p b))
      (cond ((null? p) x)
            (((car p) x) (loop (cdr p)))
            (else #f)))))

(define (or-filters . b)
  (lambda (x)
    (let loop ((p b))
      (cond ((null? p) #f)
            (((car p) x) x)
            (else (loop (cdr p)))))))

(define (not-filter f)
  (lambda(x)
    (if (f x) #f x)))

(define (field-filter-proc obj-filter fn field-filter)
  ;; pick an object that matches obj-filter and a field within that matches
  ;; field-filter
  ;; returns the object containing the field
  (lambda(x)
    (if (and (obj-filter x)
             (let ((fv (get-field-s x fn)))
               (field-filter fv)))
        x
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visit-filtered-comps c filter f)
  ;; apply f to all levels of syntax structure in postorder that match filter
  (let ((children (get-comp-children c)))
    (map (lambda(q)(visit-comps q f)) children)
    (if (filter c) (f c))))

(define (get-syntax-field x tc fn)
  ;; requires that we have stubs for tc and that
  ;; x is an instance of a subtype of tc
  (modula-type-op tc 'get-field x fn))

(define (get-field-s x fn)
  ;; requires that we have stubs for the concrete type
  (modula-type-op (rttype-typecode x) 'get-field x fn))

(define (set-syntax-field! x tc fn val)
  ;; requires that we have stubs for tc and that
  ;; x is an instance of a subtype of tc
  (modula-type-op tc 'set-field! x fn val))

(define (set-field-s! x fn val)
  ;; requires that we have stubs for the concrete type
  (modula-type-op (rttype-typecode x) 'set-field! x fn val))

(define (syntax-field-equal?-filter-proc tc fn val)
  (lambda(x)
    (if (and (RTType.IsSubtype (rttype-typecode x) tc)
             (equal? (get-syntax-field x tc fn) val))
        x
        #f)))

(define (field-equal?-filter-proc fn val)
  (let ((tc (lookup-typecode "LibertyComponent.T")))
    (lambda(x)
      (if (and (RTType.IsSubtype (rttype-typecode x) tc)
               (equal? (get-field x fn) val))
          x
          #f))))

(define (named-simple-attr-filter name)
  (and-filters
   (subtype-filter-proc "LibertySimpleAttr.T")
   (concrete-field-equal?-filter-proc 'ident name)))

(define (filter-all lib filter)
  (let* ((list '())
         (visitor (lambda(x)
                    (if (filter x)
                        (set! list (cons x list))))))
    (visit-comps lib visitor)
    (reverse list)))

(define (list-fields obj)
  (let ((c-tc (rttype-typecode obj)))
    (modula-type-op c-tc 'list-fields obj)))

(define (list-methods obj)
  (let ((c-tc (rttype-typecode obj)))
    (modula-type-op c-tc 'list-methods obj)))

(define (call-method obj mn . args)
  (let ((c-tc (rttype-typecode obj)))
    (modula-type-op c-tc 'call-method obj mn args)))

(define (deep-copy obj)
  ;; use pickles to make a deep copy for modifications
  ;; works for any traced object (almost all M3 objects i.o.w.)
  (let ((wr (TextWr.New)))
    (Pickle.Write wr obj)
    (Pickle.Read (TextRd.New (TextWr.ToText wr)))))

(define (format-comp comp)
  (define wr (TextWr.New))
  ((obj-method-wrap comp 'LibertyComponent.T) 'write wr "")
  (TextWr.ToText wr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test4)
  (define pct-fall
    (car (filter-all *lib*
                     (named-simple-attr-filter "input_threshold_pct_fall"))))
       )

(define (test5)
  (define wr (TextWr.New))
  ((obj-method-wrap *lib* 'LibertyGroup.T) 'write wr "")
  (TextWr.ToText wr))


(define (test6)
  (get-field-s (get-field-s (get-field-s (last complex-attrs) 'head) 'params) 'params)
)

(define (name-to-arcs nm)
  (let ((arcs (TextUtils.Shatter nm "." "" #f)))
    (let loop ((p arcs)
               (res '()))
      (if (null? p) (reverse res) (loop (get-field-s p 'tail)
                                        (cons (get-field-s p 'head) res))))))

(define (old-get-field obj fn)
  (let loop ((arcs (name-to-arcs (force-string fn)))
             (res obj))
    (if (null? arcs)
        res
        (loop (cdr arcs) (get-field-s res (string->symbol (car arcs)))))))

(define (get-field obj fn)
  (let loop ((arcs (LibertyArcs.Parse (force-string fn)))
             (res obj))
    (if (null? arcs)
        res
        (loop (cdr arcs)
              (cond ((eq? (caar arcs) 'field)
                     (get-field-s res (cdar arcs)))
                    (else
                     (call-method res (caar arcs) (cdar arcs))))))))

(define *e* '())
(define *f* '())
(define *g* '())

(define (set-field! obj fn val)
  (let loop ((arcs (LibertyArcs.Parse (force-string fn)))
             (res obj))
    (if (null? (cdr arcs))
        (begin
          (set! *e* res)
          (set! *f* (car arcs))
          (set! *g* val)
          (cond ((eq? (caar arcs) 'field)
                 (set-field-s! res (cdar arcs) val))
                ((eq? (caar arcs) 'field)
                 (call-method res 'put (cdar arcs) val))
                (else "Unknown search method " (caar arcs))))
          
        (loop (cdr arcs)
              (cond ((eq? (caar arcs) 'field)
                     (get-field-s res (cdar arcs)))
                    (else
                     (call-method res (caar arcs) (cdar arcs))))))))

(define (test7)
  (define complex-attrs
    (filter-all *lib* (subtype-filter-proc 'LibertyComplexAttr.T))))

(define (test8)
  
  (get-field-s (get-field-s (last complex-attrs) 'head) 'params)

  (get-field (last complex-attrs) 'head.params)

  (get-field (last complex-attrs) 'head.params.params)

  )

(define group-filter (subtype-filter-proc 'LibertyGroup.T))

(define (named-group-filter-proc name)
  (and-filters
   (subtype-filter-proc 'LibertyGroup.T)
   (field-equal?-filter-proc 'head.ident name)))
                                 
(define cell-fall-group-filter
  (named-group-filter-proc "cell_fall"))

(define (inspect comp)
  (dis comp dnl)
  (let ((fields (list-fields comp)))
    (map (lambda(f)
           (dis " ." f " : " (get-field comp f) dnl))
         fields))
  (let ((methods (list-methods comp)))
    (map (lambda(m)
           (dis " ." m "()" dnl))
         methods))
  'ok)


(define (test9)
  (define cell-falls (filter-all *lib* cell-fall-group-filter))
  (define x (last cell-falls))
  (list-methods (get-field x 'statements))
  )

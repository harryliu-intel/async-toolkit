(require-modules "m3" "types.scm" "display")

(define (only x)
  ;; get the only element from a singleton list
  (if (not (= 1 (length x)))
      (error "only : " (length x) " elements : " x)
      (car x)))

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
             (let ((fv (get-field x fn)))
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

(define (have-field-s? obj fn)
  (member? fn (list-fields obj)))

(define (list-methods obj)
  (let ((c-tc (rttype-typecode obj)))
    (modula-type-op c-tc 'list-methods obj)))

(define (have-method? obj mn)
  (member? mn (list-methods obj)))

(define (call-method obj mn . args)
  (let ((c-tc (rttype-typecode obj)))
    (modula-type-op c-tc 'call-method obj mn args)))

(define (deep-copy obj)
  ;; use pickles to make a deep copy for modifications
  ;; works for any traced object (almost all M3 objects i.o.w.)
  ;;
  ;; 'parent field in copy is left null (if it exists)
  
  (define (do-copy)
    (let ((wr (TextWr.New)))
      (Pickle.Write wr obj)
      (Pickle.Read (TextRd.New (TextWr.ToText wr)))))
  
  (if (have-field? obj 'parent)
      (let* ((save (get-field obj 'parent))
             (null (set-field! obj 'parent '()))
             (res  (do-copy)))
        (set-field! obj 'parent save)
        res)
      (do-copy)))
      

(define (format-comp comp . field)
   (if (not (null? field))
      (format-comp (get-field comp (car field)))
      (let ((wr (TextWr.New)))
        ((obj-method-wrap comp 'LibertyComponent.T) 'write wr "")
        (TextWr.ToText wr))
      )
   )


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

(define (have-field? obj fn)
  (let loop ((arcs (LibertyArcs.Parse (force-string fn)))
             (res obj))
    (cond((null? arcs) fn)
         ((eq? (caar arcs) 'field)
          (if (have-field-s? res (cdar arcs))
              (loop (cdr arcs) (get-field-s res (cdar arcs)))
              #f))
         (else
          (if (have-method? res (caar arcs))
              (loop (cdr arcs) (call-method res (caar arcs) (cdar arcs)))
              #f)))))

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

  (get-field (last complex-attrs) 'head.params)

  )

(define group-filter (subtype-filter-proc 'LibertyGroup.T))

(define (named-group-filter-proc name)
  (and-filters
   (subtype-filter-proc 'LibertyGroup.T)
   (field-equal?-filter-proc 'head.ident name)))
                                 
(define cell-fall-group-filter
  (named-group-filter-proc "cell_fall"))

(define (inspect comp . field)
  (if (not (null? field))
      (inspect (get-field comp (car field)))
      (begin
        (dis comp dnl)
        (let ((fields (list-fields comp)))
          (map (lambda(f)
                 (dis " ." f " : " (get-field comp f) dnl))
               fields))
        (let ((methods (list-methods comp)))
          (map (lambda(m)
                 (dis " ." m "()" dnl))
               methods))
        
        'ok)))


(define (test9)
  (define cell-falls (filter-all *lib* cell-fall-group-filter))
  (define x (last cell-falls))
  (list-methods (get-field x 'statements))
  )

(define (test10)
  (define idx 'statements[0].head.params[1].val.val)
  (define old-values
    (LibertyCsv.ToList (get-field x idx)))
  (define add-1 (lambda (x) (+ x 1)))
  (define new-values (map add-1 old-values))

  (set-field! x idx (LibertyCsv.ToCsv new-values))

 
  )

(define (process-param-list! param-list f)
  (let* ((seq (get-field param-list 'params))
         (n (call-method seq 'size)))
    (let loop ((i 0))
      (if (= i n)
          'ok
          (begin
            (let* ((m           (call-method seq 'get i))
                   (old-values  (LibertyCsv.ToList (get-field m 'val.val)))
                   (new-values  (map f old-values))
                   (new-csv     (LibertyCsv.ToCsv new-values)))
              (set-field!  m 'val.val new-csv))
            (loop (+ i 1))
            )
          ))))

(define (modify-named-group-value-list! lib named f)
  (let* ((filter    (named-group-filter-proc named))
         (groups    (filter-all lib filter))
         (base      'statements[0].head.params)
         (modifier  (lambda(group)
                      (process-param-list! (get-field group base) f))))
    (map modifier groups)
    'ok))

(define (parse-liberty-file fn)
  (let* ((rd (FileRd.Open fn))
         (res (LibertyParse.Parse rd)))
    (Rd.Close rd)
    res))

(define (map-seq seq f)
  (let ((n (call-method seq 'size)))
    (let loop ((i (- n 1))
               (res '()))
      (if (= i -1)
          res
          (loop (- i 1)
                (cons (f (call-method seq 'get i)) res))))))


(define (get-named-groups comp group-name)
  (let ((matches (filter-all comp (named-group-filter-proc group-name))))
    matches))
      
(define (get-unique-named-group comp group-name)
  (let ((matches (get-named-groups comp group-name)))
    (if (not (= (length matches) 1))
        (error "Named group " group-name " not unique, matches " (length matches))
        (car matches))))

(define (get-named-group cell type name)
  (let* ((name-path 'head.params[0].val.val)
         (groups      (get-named-groups cell type))
         (matches (filter
                   (lambda(g)
                     (equal? name
                             (get-field g name-path)))
                   groups))
         )
    (only matches)))

(define (get-named-bus-group cell name)
  (get-named-group cell "bus" name))

(define (get-named-pin-group cell name)
  (get-named-group cell "pin" name))

(define (get-named-type-group cell name)
  (get-named-group cell "type" name))

(define (copy-named-pin-group! cell name new-name)
  (let* ((g     (get-named-pin-group cell name))
         (c     (deep-copy g))
         (npath 'head.params[0].val.val))
    (set-field! c 'parent (get-field g 'parent))
    (set-field! c npath new-name)
    (call-method (get-field c 'parent.statements) 'addhi c)
    c))

(define (get-field-or-false obj fn)
  (if (have-field? obj fn) (get-field obj fn) #f))

(define (set-named-bus-variable! bus-group name to)
  (map-seq (get-field bus-group 'statements)
           (lambda(s)
             (if (equal? (get-field-or-false s 'ident) name)
                 (set-field! s 'attrValExpr.val.val to)))))

(define (un-val x)
  ;; iterate .val until we hit something that's not an object
  (if (have-type-ops? (rttype-typecode x))
      (un-val (get-field x 'val))
      x))

(define *q* '())

(define (get-group-variable group name)
  (only (filter (not-filter null?)
                (map-seq (get-field group 'statements)
                         (lambda(s)
                           (if (equal? (get-field-or-false s 'ident) name)
                               (begin
                                 (set! *q* s)
                                 (un-val (get-field s 'attrValExpr)))))))))
                              






;; rename cell
;; area?
;; block_distance?

  

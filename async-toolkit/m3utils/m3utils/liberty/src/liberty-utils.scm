
;; requires types.scm in the same directory as this file to be loaded also
(require-modules "m3" "display")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; debugging definitions
;;

(define *do-debug* #f) ;; override this with (set! *do-debug* #t) to debug

(define (debug . x)    ;; generic debug statement
  (if *do-debug* (apply dis "DEBUG " x) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


         
(define (test1) ((obj-method-wrap
                  *lib*
                  'LibertyComponentChildren.Private)
                 'children))

(define (test2) ((obj-method-wrap
                  ((obj-method-wrap *lib* 'LibertyComponentChildren.Private)
                   'children) 'LibertyComponentSeq.T) 'get 0))

;; example code, will this work?
(define (not-done)
  (find-grammar-object
 *lib*
 (lambda(x) (and (is-grammar-type x 'LibertySimpleAttr.T)
                 (equal? (has-field x 'ident) "time_unit"))))
  )

(define (test3)
  (RTType.IsSubtype (rttype-typecode *lib*)
                    (lookup-typecode "LibertyComponent.T") )
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

(define (have-field?-filter fn)
  (lambda(x)(have-field? x fn)))

(define (field-equal?-filter fn val)
  (lambda(x)(equal? (get-field x fn) val)))

(define (field-member?-filter fn values)
  (lambda(x)(member? (get-field x fn) values)))

(define (named-simple-attr-filter name)
  (and-filters
   (subtype-filter-proc "LibertySimpleAttr.T")
   (field-equal?-filter-proc 'ident name)))

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

(define (display-comp comp . field)
  (let ((fmt (apply format-comp (cons comp field))))
    (dis fmt dnl)
    comp))


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
  (let ((arcs (CitTextUtils.Shatter nm "." "" #f)))
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
         ((and (eq? (caar arcs) 'get)
               (have-method? res 'get)
               (have-method? res 'size)
               (< (cdar arcs) (call-method res 'size)))
          (loop (cdr arcs)
                (call-method res (caar arcs) (cdar arcs))))
         (else #f))))
              

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
        (let ((fields (list-fields comp)))
          (map (lambda(f)
                 (dis " ." f " : " (get-field comp f) dnl))
               fields))
        (let ((methods (list-methods comp)))
          (map (lambda(m)
                 (dis " ." m "()" dnl))
               methods))
        
        comp)))


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

(define (process-param-list! param-list f group)
  ;; note that f is called with two arguments : (f <value> <group>)
  ;; this allows f to act differently for different groups (eg. pins)
  (let* ((seq param-list)
         (n (call-method seq 'size)))
    (let loop ((i 0))
      (if (= i n)
          'ok
          (begin
            (let* ((m           (call-method seq 'get i))
                   (old-values  (LibertyCsv.ToList (get-field m 'val.val)))
                   (new-values  (map (lambda(x)(f x group)) old-values))
                   (new-csv     (LibertyCsv.ToCsv new-values)))
              (set-field!  m 'val.val new-csv))
            (loop (+ i 1))
            )
          ))))

(define (modify-named-group-value-list! lib named f)
  ;; note that f is called with two arguments : (f <value> <group>)
  ;; this allows f to act differently for different groups (eg. pins)
  (let* ((filter    (named-group-filter-proc named))
         (groups    (filter-all lib filter))
         (base      'statements[0].head.params)
         (modifier  (lambda(group)
                      (process-param-list!
                       (get-field group base)
                       f
                       group))))
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

(define *qq* '())

(define (get-paramed-groups comp pname0)
  ;; get all groups for which zeroth param matches pname0
  (let* ((fn 'head.params[0])
         (filter (lambda(x)
                   (set! *qq* x)
                   (if (have-field? x fn)
                       (equal? pname0 (un-val (get-field x fn)))
                       #f))))
    (filter-all comp filter)
    )
  )

(define (update-cell-pin-power-2! cell pin-name constraint rise-fall-pow)
  (let* ((pin (get-named-pin-group cell pin-name))
         (matching-attrs
          (filter-all pin
                      (and-filters
                       (subtype-filter-proc 'LibertySimpleAttr.T)
                       (field-equal?-filter-proc 'ident "when")
                       (field-equal?-filter-proc 'attrValExpr.val constraint))))
         (power-grp
          (get-field (only matching-attrs) 'parent))
         (f (and-filters
             (have-field?-filter 'head)
             (field-member?-filter 'head.ident '("rise_power" "fall_power"))))
         (powers (filter-all power-grp f))
         )

    ;; really painful, this...
    ;; XXX NOTYET : check that we actually updated the values in question?
    ;; or do we want to leave this out?
    (map (lambda(v)(update-val! (get-field v 'statements[0].head.params[0])
                                (force-string rise-fall-pow)))
         powers)
 ))   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                             
(define (get-named-bus-group cell name)
  (get-named-group cell "bus" name))

(define (get-named-pin-group cell name)
  (get-named-group cell "pin" name))

(define (get-named-type-group cell name)
  (get-named-group cell "type" name))

(define (copy-named-pin-group! cell name new-names)
  (debug "new-names " new-names dnl)
  
  (let* ((g     (get-named-pin-group cell name)) ;; this is slow
         (npath 'head.params[0].val.val))
    (let loop ((p new-names))
      (if (null? p)
          cell
          (loop
           (let ((c     (deep-copy g)))
;;             (dis "creating new pin " (car p) dnl)
             (set-field! c 'parent (get-field g 'parent))
             (set-field! c npath (car p))
             (call-method (get-field c 'parent.statements) 'addhi c)
             (cdr p))
           )))))

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
                              

(define (update-group-variable! group name to)
  (map-seq (get-field group 'statements)
           (lambda(s)
             (if (equal? (get-field-or-false s 'ident) name)
                 (begin
                   (let loop ((p (get-field s 'attrValExpr))
                              (q (get-field s 'attrValExpr.val)))
                     (if (have-type-ops? (rttype-typecode q))
                         (loop q (get-field q 'val))
                         (set-field! p 'val to))))))))

  
(define (update-val! obj to)
  (let loop ((p obj)
             (q (get-field obj 'val)))
    (if (have-type-ops? (rttype-typecode q))
        (loop q (get-field q 'val))
        (set-field! p 'val to))))

;; rename cell


(define (clog2 n)
  (ceiling (/ (log n) (log 2))))


(define *area-program-path* "/nfs/sc/disks/bfn_pd_cb_02/mnystroe/applications.design-automation.memory.lamb/python/pyarea/area.py")

(define (compute-macro-area width depth tech)

  (let ((the-command  (string-append
                   *area-program-path*
                   " -T " tech
                   " -q "
                   " -w " (number->string width)
                   " -d " (number->string depth)
                   )
                  ))
    
    (debug "compute-macro-area command : " the-command dnl)
    (string->number
     (run-command the-command))
    )
  )

(define (expand-bus-width! lib bus-name width)
  (let* ((the-cell          (get-lib-cell lib))
         (proto-pin-name    (string-append bus-name "[0]"))
         (proto-pin         (get-named-pin-group the-cell proto-pin-name))
         (the-bus           (get-field proto-pin 'parent))
         (the-bus-type-nm   (get-group-variable the-bus "bus_type"))
         (the-bus-type      (get-named-type-group the-cell the-bus-type-nm)))
    
    (if (not (equal? "bus" (get-field the-bus 'head.ident)))
        (error "pin " proto-pin-name " : parent is not a bus!"))

    (update-group-variable! the-bus-type "bit_width" width)
    (update-group-variable! the-bus-type "bit_from" (- width 1))

    (let ((new-names
           (let loop ((i 1)
                      (res '()))
             (if (= i width)
                 res
                 (let ((new-name (string-append bus-name "[" (number->string i) "]")))
                   (loop (+ i 1)
                         (cons new-name res)))))))
      (copy-named-pin-group! the-cell proto-pin-name (reverse new-names))
      )
    
    the-bus
    ))

(define (get-named-statements group name)
  (let ((statement-seq (get-field group 'statements)))
     (filter (lambda(x) x)
             (map-seq statement-seq
                      (lambda(s)
                        (if (and (have-field? s 'head.ident)
                                 (equal? (get-field s 'head.ident) name))
                            s
                            #f))))))

(define (get-named-statement group name)
  (only (get-named-statements group name)))

(define (get-lib-cell lib)
  (get-named-statement lib "cell"))

(define (get-named-simple-attrs group name)
  (let ((statement-seq (get-field group 'statements)))
     (filter (lambda(x) x)
             (map-seq statement-seq
                      (lambda(s)
                        (if (and (have-field? s 'ident)
                                 (equal? (get-field s 'ident) name))
                            s
                            #f))))))

;; area?
;; block_distance?

(define *ulca-l* '())
(define *ulca-a* '())
(define *ulca-t* '())

(define (update-lib-cell-attr! lib attr to)
  (set! *ulca-l* lib)
  (set! *ulca-a* attr)
  (set! *ulca-t* to)
  (let* ((the-cell (get-lib-cell lib))
         (updated (update-group-variable! the-cell attr to))
         (f-up    (filter (not-filter null?) updated)))
    (if (= 0 (length f-up))
        (error "couldn't find attr " attr)
        f-up)
    )
  )
  
  
(define (update-lib-area! lib new-area)
  (update-lib-cell-attr! lib "area" new-area))

(define (test11)
  (define the-cell (get-lib-cell *lib*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; timing
;;

(define (get-lut-usages lib named)
  ;; all usages of given LUT
  (let* ((timing-groups (get-named-groups c0 "timing"))
         (res (apply
               append
               (map (lambda(x)(get-paramed-groups x named))
                    timing-groups))))
    res))

(define (test12) (get-lut-usages c0 "lut_timing_1"))

(define (get-enclosing-group p type)
  ;; get enclosing group of type <type> for a given other group
  (cond ((null? p) p)
        ((and (have-field? p 'head.ident)
              (equal? (un-val (get-field p 'head.ident))  type))
         p)
        (else (get-enclosing-group (get-field p 'parent) type))))
           
(define (get-lut-references lib named)
  ;; get all the pins and their modes that reference a particular LUT
  (let ((usages (get-lut-usages lib named)))
    (map (lambda(u)(cons (un-val
                          (get-field
                           (get-enclosing-group u "pin") 'head.params[0]))
                         (un-val
                          (get-field u 'head.ident))))
         usages)))

(define (get-lib-templates lib)
  (get-named-statements lib "lu_table_template"))

(define (find-lut-variable lut named)
  (let loop ((i 1))
    (let ((vars (get-named-simple-attrs
                 lut
                 (string-append "variable_" (number->string i)))))
      (cond ((null? vars) #f)
            ((equal? (un-val (get-field (only vars) 'attrValExpr))
                     named) i)
            (else (loop (+ i 1)))))))
      

(define (update-lut-index! lut named f)
  ;; f takes two params: (f <old-value> <lut>)
  (let ((idx        (find-lut-variable lut named)))
    (if idx
        (let* 
            ((idx-name   (string-append "index_" (number->string idx)))
             (attr       (get-named-statement lut idx-name))
             (old-values (LibertyCsv.ToList
                          (get-field attr 'head.params[0].val.val)))
             (new-values (map (lambda(x)(f x lut)) old-values))
             (new-csv    (LibertyCsv.ToCsv new-values)))
          (set-field! attr 'head.params[0].val.val new-csv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MAIN ENTRY POINTS BELOW
;;

(define (update-lib-size! lib width depth tech-name)
  (let ((awidth (clog2 depth))
        (the-cell (get-lib-cell lib))
        (the-area (compute-macro-area width depth tech-name))
        )

    (debug "updating lib area..." dnl)
    (update-lib-area! lib the-area)
    ;; update the physical area
    
    (debug "expanding wdata to " width "..." dnl)
    (expand-bus-width! lib "wdata"  width)

    (debug "expanding dout to " width "..." dnl)
    (expand-bus-width! lib "dout"   width)

    
    (debug "expanding wadr to " awidth "..." dnl)
    (expand-bus-width! lib "wadr"   awidth)
    
    (debug "expanding radr to " awidth "..." dnl)
    (expand-bus-width! lib "radr"   awidth)
    
    )
  )

(define *last-pin* '())
(define *matching-groups* '())
(define *new-groups* '())

(define (expand-read-timing-arcs! lib depth)
  ;; broaden the read timing arcs radr[0] -> dout[0]
  ;; to encompass all radr[.]
  ;;
  ;; (copying of dout itself is done in a later step)
  (let ((rw (clog2 depth)))
    
    (debug "expanding read timing arcs to " rw "..." dnl)

    (let* ((the-cell (get-lib-cell lib))
           (the-pin-name "dout[0]")
           (the-pin (get-named-pin-group the-cell the-pin-name))
           (the-source-pin "radr[0]")
           (timing-groups (get-named-groups the-pin "timing"))
           (matching-groups
            (filter
             (lambda(tg)(equal? the-source-pin
                                (CitTextUtils.Replace
                                 (get-group-variable tg "related_pin") " " "")))
             timing-groups))
           )

      (set! *last-pin* the-pin)
      (set! *matching-groups* matching-groups)
      
      (let loop ((idx 1)
                 (add-groups '()))
        (if (>= idx rw)
            ;; base case, add new groups to parent
            (begin
              (set! *new-groups* add-groups)
              (map (lambda(g)(call-method (get-field g 'parent.statements)
                                          'addhi
                                          g))
                   add-groups)
              'ok)

            ;; recursion case, add group to new groups
            (loop (+ idx 1)
                  (let ((new (map (lambda(x)
                                    (let ((n (deep-copy x)))
                                      (set-field! n
                                                  'parent
                                                  (get-field x 'parent))
                                      n))
                                  matching-groups)))
                    (map (lambda(g)
                           (update-group-variable! g
                                                   "related_pin"
                                                   (string-append "radr["
                                                                  (stringify idx)
                                                                  "]")))
                         new)
                    (append add-groups new)))
                    )))))

(define (update-lib-names! lib new-lib-name new-cell-name)
  (set-field! lib 'head.params[0].val.val new-lib-name)
  (let ((the-cell (get-lib-cell lib)))
    (set-field! the-cell 'head.params[0].val.val new-cell-name))
  )

(define (update-lib-date! lib)
  (let ((new-date (Text.Sub (run-command "date") 0 28)))
    (update-group-variable! lib "date" new-date)
    new-date
    )
  )

(define (update-lib-pvt! lib volt temp pvt-name)

  (update-group-variable! lib "nom_temperature" temp)
  (update-group-variable! lib "nom_voltage" volt)
  (update-group-variable! lib "default_operating_conditions" pvt-name)
  
  (let ((the-op-conds (get-named-statement lib "operating_conditions")))
    (update-group-variable! the-op-conds "temperature" temp)
    (update-group-variable! the-op-conds "voltage" volt)
    (set-field! the-op-conds 'head.params[0].val.val pvt-name))

  ;; update the voltage maps
  (let ((the-vdd-attrs
         (filter-all lib
                     (and-filters
                      (subtype-filter-proc 'LibertyComplexAttr.T)
                      (lambda(x)(equal? "voltage_map"
                                        (get-field x 'head.ident)))
                      (lambda(x)(equal? "VDD"
                                        (un-val (get-field x 'head.params[0]))))
                      )
                     )))
    (map (lambda(a)(set-field! a 'head.params[1].val.val volt)) the-vdd-attrs))
  lib
  )

    
(define (update-lib-simple-attr! lib pname f)
  (let* ((filter (named-simple-attr-filter pname))
         (attrs  (filter-all lib filter)))
    (map (lambda(a)
           (let ((attr (get-field a 'attrValExpr)))
             (update-val! attr (f (un-val attr)))))
         attrs)))

(define (test13)
  (update-lib-simple-attr! *lib* "capacitance" (lambda(x)(* x 0.3))))

(define (update-lib-timings! lib named f)
  ;; f takes two parameters (f <value> <group>)
  ;; can e.g. do (define (f v g) ... (get-enclosing-group g "pin") ...)
  (modify-named-group-value-list! lib named f))

(define (update-lib-templates! lib named f)
  ;; named is the name of the independent variable
  ;; f takes two parameters (f <value> <LUT>)
  ;; can let f depend on the nature of the LUT
  (let ((luts (get-lib-templates lib)))
    (map (lambda(lut)(update-lut-index! lut named f))
         luts))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; examples for test & debug
;;

(define some-field '(field INITIAL_W0_OFFSET 56 8))

(define some-reg
  '((cont POL_DIRECT_MAP_POL1)
         ((field CTOK_HI 40 24))
         ((field _RSVD1_ 37 3))
         ((field CTOK_LO 24 13))
         ((field _RSVD0_ 4 20))
         ((field CFG 0 4))
         )
  )

(define some-hier2
  `((cont hier) ,some-reg )
  )
         

(define some-array '((cont GLORT_CAM)
                          ((array 64)
                                 ((field KEY_INVERT 16 16))
                                 ((field KEY 0 16))
                                 )
                          )
  )


;; working structure
(dis "building fields-tree..." dnl)

(define fields-tree (treesum 'nfields the-map))

;;(define bits-tree (treesum 'nbits the-map))

;;(iter tl 11 fields-tree)

;;(iter fc 11 the-map)


(define hy '(7 (1) (1) (1) (1) (1) (1) (1))   )

(define (trunc-stringify x)  (error-append (stringify x)))

(define (array-marker a)
  (if (eq? (get-tag a) 'array) (cadar a) #f))

(define (zip-trees-old a b)
 ;; (if (not (tree-iso? a b)) (error "not tree-iso"))
  (cond ((null? a) '())
        ((atom? a) (cons a b))
        (else (cons (zip-trees-old (car a) (car b))
                    (zip-trees-old (cdr a) (cdr b))))))

(define (zip-trees a b)
  (cond ((null? a) '())
        (else (cons (cons (car a) (car b))
                    (map (lambda(x y) (zip-trees x y)) (cdr a) (cdr b))))))

(define (nuller x) '())

(define (build-zip t)
  (zip-trees (treemap array-marker t)
             (zip-trees (treesum 'nfields t)
                        (treemap nuller t))))

(define some-zip (build-zip some-array))

(define (zip-array? z)
  (let ((as (caadr z)))
    (and (car as) (/ (cadr as) (car as)))))

(define (get-zip-seq-offset z seq)
  (let loop ((p 0)
             (s seq)
             (q z))
    ;;(dis "p " (stringify p) " s " (stringify s) " q " (stringify q) dnl)
    (cond ((null? s) p)
          ((zip-array? q) =>
           (lambda (m) (loop (+ p (* (car s) m))
                             (cdr s)
                             (get-aux-child-by-cnt q 0))))
          (else (loop (+ p (accumulate + 0
                            (map cadar
                                 (get-aux-children-by-cnt q (car s)))))
                      (cdr s)
                      (get-aux-child-by-cnt q (car s)))))))
  
(define (fielddata->lsb fd)
  (+ (* 8 (cdr (assoc 'byte fd)))
     (cdr (assoc 'lsb fd))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; offset tree format
;;   -- this is the format for computing final addresses
;;   -- it includes all the arraying and size info needed
;;

;; make offset tree

(define (make-address-getter addresses)
  (lambda(x)
    (fielddata->lsb
     (FieldData.ArrayGet addresses x))))

(define (get-stride-bits array-spec indexer)
  ;; given a spec as follows
  ;; (base-addr elems . size)
  ;; compute element stride in bits
  (if (and (cadr array-spec)
           (> (cadr array-spec) 1))
      (let*  ((zeroth-field (car array-spec))
              (field-stride (/ (cddr array-spec)
                               (cadr array-spec)))
              (stride-field (+ zeroth-field field-stride))
              (zeroth-bit (indexer zeroth-field))
              (stride-bit (indexer stride-field)))
        (- stride-bit zeroth-bit))
      '()))

(define (make-offset-tree accum-tree array-tree fields-tree indexer)
  ;;
  ;; format of an elem here is
  ;; (<offset> #f)
  ;;     -- offset from parent for non-array
  ;; (<offset> <elems> . <bits-stride>)
  ;;     -- offset from parent, # of elements, stride in bits
  ;;
  ;; indexer is a procedure of one argument that maps the in-order
  ;; field index to a linear index in some space
  ;;
  (define (helper p b)
    (if (null? p)
        '()
        (let ((this-addr (indexer (caar p))))
          (cons (cons (- this-addr b)
                      (cons (cadar p) (get-stride-bits (car p) indexer))
                      )
                (map (lambda(ff)(helper ff this-addr)) (cdr p))))))

  (helper (zip-trees accum-tree
                     (zip-trees array-tree fields-tree))
          0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use offset tree

(define (compute-array-pair-stride adesc idx)
  ;; format is (len . stride)
  ;; stride null for 0-length arrays
  (cond ((= idx 0) 0)
        ((or (< idx 0) (>= idx (car adesc)))
         (error "array index out of range : " idx " : " (stringify adesc)))
        (else  (* (cdr adesc) idx))))

(define (has-array-child? p)
  (and (cdr p)
       (= 1 (length (cdr p)))
       (car (cdaadr p))))
   
(define (compute-offset-from-seq ot seq)
  (define debug #f)
  
  (define (helper base p seq)
    (if debug
        (begin
          (dis "---" dnl)
          (dis "car p          : " (stringify (car p)) dnl)
          (dis "length (cdr p) : " (length (cdr p)) dnl)
          (dis "seq            : " (stringify seq)     dnl)
          )
        )
    
    (if (null? seq)
        base ;; done

        (if (has-array-child? p)
            ;; array case
            (let ((child (cadr p)))
              (if debug (dis "arr child      : " (trunc-stringify child) dnl))
              (helper
               (+ base (compute-array-pair-stride (cdar child) (car seq)))
               child
               (cdr seq)))

            ;; non-array case
            (let ((child (nth (cdr p) (car seq))))
              (if debug (dis "nonarr child   : " (trunc-stringify child) dnl))
              (helper
               (+ base (caar child))
               child
               (cdr seq))))))

  (helper 0 ot seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test the offset tree
;;

(define zz (build-zip the-map))

(cnt-sequence-by-name the-map '(mpp 0 shm FWD_TABLE0 1 0 DATA))

(define the-array-tree (treemap array-marker the-map))

(define the-accum-tree (tree-accum fields-tree))

(dis (stringify
      (FieldData.ArrayGet the-addresses
                          (get-zip-seq-offset
                           zz
                           (cnt-sequence-by-name
                            the-map
                            '(mpp 0 shm FWD_TABLE_MOD 9 16383 DATA))))
      ) dnl)

(define last-entry (FieldData.ArrayGet the-addresses
                                       (- (FieldData.ArraySize the-addresses)
                                          1)))
  
(dis "length of the-addresses " (FieldData.ArraySize the-addresses) dnl)

;; average width of field (including space) -- in bits
(define ave-width
  (* 8 (/ (+ (cdr (assoc 'byte last-entry))
             (/ (cdr (assoc 'wid last-entry)) 8))
          (+ (cdr (assoc 'id last-entry)) 1)))
  )

(dis "average width of fields " ave-width " bits" dnl)

(define the-chip-offset-tree
  (make-offset-tree the-accum-tree
                    the-array-tree
                    fields-tree
                    (make-address-getter the-addresses)))

(define the-fields-offset-tree
  (make-offset-tree the-accum-tree
                    the-array-tree
                    fields-tree
                    identity))

;; the following should give the same answer

(compute-offset-from-seq the-chip-offset-tree '(1 0 0 0 0 0 0))

(fielddata->lsb (FieldData.ArrayGet the-addresses
                                    (get-zip-seq-offset zz '(1 0 0 0 0 0 0))))

(define (make-spaces n)
  (if (= 0 n) "" (string-append " " (make-spaces (- n 1)))))

(define (compile-offset-c ot nm port)
  (define (helper b t sp)
    (let ((ind (make-spaces (* 4 (+ 1 sp)))))
      (if (has-array-child? t)
          (let* ((child  (cadr t))
                 (aspec  (cdar child))
                 (stride (if (null? (cdr aspec)) "0xc0edbabe" (cdr aspec)))
                 (size   (car aspec))
                 )
            (dis ind "{ /* array */" dnl
                 ind "  int idx = seq->d["sp"];" dnl
                 ind "  if (idx == -1) return arr + ADDR_LITERAL("b");" dnl
                 ind "  assert(idx >= 0 && idx < "size");" dnl
                 ind "  arr += idx * ADDR_LITERAL("stride");" dnl
                 port )
            (helper b child (+ 1 sp))
            (dis ind "}" dnl
                 port)
            )
          (begin
            (dis ind "{ /*nonarray */" dnl
                 ind "  switch(seq->d["sp"]) {" dnl
                 ind "    case -1: return arr + ADDR_LITERAL("b"); break;" dnl
                 port)
            (let loop ((p     0)
                       (c     (cdr t)))
              (if (not (null? c))
                  (begin
                    (dis ind "    case " p":" dnl
                         port)
                    (helper (+ b (caaar c)) (car c) (+ 1 sp))
                    (loop (+ p 1) (cdr c)))
                  )
              )
            (dis ind "    default: assert(0); break;" dnl
                 ind "  }" dnl
                 ind "}" dnl
                 port)
            )
          )
      )
    )
  (dis "chipaddr_t" dnl nm"(const seqtype_t *seq)" dnl
       "{" dnl
       "   chipaddr_t arr=ADDR_LITERAL(0);" dnl
       port)
  (helper 0 ot 0)
  (dis "}" dnl
       port)
  #t
  )

(define symbols (make-symbol-set 100))

(define (make-number-hash-table size) (make-hash-table size identity))

(define (make-number-set size)
  (make-set (lambda()(make-number-hash-table size))))

(define sizes (make-number-set 100))

(define (record-symbols)
  (treemap
   (lambda(x)
     (let ((nm (get-name x)))
       (cond ((number? nm) (sizes   'insert! nm))
             ((symbol? nm) (symbols 'insert! nm))
             (else (error (error-append " : " (stringify nm)))))))
   the-map))

(define (make-c-sym-constant sym port)
  (dis "static const char       symbol_" sym "[]     = \"" sym "\";" dnl
       "static const arc_t      symbol_arc_" sym "   =  { symbol_" sym ", NULL };" dnl
       port
       )
  #t
  )

(define (dump-symbols port)
  (map (lambda(s)(make-c-sym-constant s port)) (symbols 'keys))
  #t
  )

(define (make-c-siz-constant sz port)
  (dis "static const arrayarc_t size_"sz"         = { " sz " };" dnl
       "static const arc_t      size_arc_"sz"     = { NULL, &size_"sz" };" dnl
       "static const arc_t     *size_arc_"sz"_a[] = { &size_arc_"sz", NULL };" dnl
       port)
  #t
  )


(define (dump-sizes port)
  (map (lambda(q)(make-c-siz-constant q port)) (sizes 'keys))
  #t
  )


(define (compile-child-arc-c nt nm port)
  (define *arcarray-cnt* 0)

  (define sym-arcarray-mem '()) ;; memoization-memory

  (define (make-sym-arcarray names)
    (let loop ((p sym-arcarray-mem))
      (cond ((null? p)
             (let ((nm (string-append "syms_arc_" *arcarray-cnt* "_a")))
               (dis "static const arc_t     *"nm"[] = { " port)
               (map (lambda(sym)(dis "&symbol_arc_" sym ", " port)) names)
               (dis "NULL };" dnl port)
               (set! *arcarray-cnt* (+ 1 *arcarray-cnt*))
               (set! sym-arcarray-mem (cons (cons names nm) sym-arcarray-mem))
               nm))
            ((equal? (caar p) names) (cdar p))
            (else (loop (cdr p))))))
     

  (define defer-port (TextWr.New))
  
  (define (has-array-child? q)
    (and (cdr q)
         (= 1 (length (cdr q)))
         (number? (caadr q))))
  
  (define (helper t sp)
    (let ((ind (make-spaces (* 2 (+ 1 sp)))))
      (if (has-array-child? t)
          (let* ((child  (cadr t))
                 (size   (caadr t))
                 )
            (dis ind "{ /* array */" dnl
                 ind "  int idx = seq->d["sp"];" dnl
                 ind "  if (idx == -1) return size_arc_"size"_a;" dnl
                 ind "  assert(idx >= 0 && idx < "size");" dnl
                 defer-port )
            (helper child (+ 1 sp))
            (dis ind "}" dnl
                 defer-port)
            )
          (begin
            (dis ind "{ /*nonarray */" dnl
                 ind "  switch(seq->d["sp"]) {" dnl
                 ind "    case -1: return "
                 (make-sym-arcarray (map car (cdr t)))
                 "; break;" dnl
                 defer-port)
            (let loop ((p     0)
                       (c     (cdr t)))
              (if (not (null? c))
                  (begin
                    (dis ind "    case " p":" dnl
                         defer-port)
                    (helper (car c) (+ 1 sp))
                    (loop (+ p 1) (cdr c)))
                  )
              )
            (dis ind "    default: assert(0); break;" dnl
                 ind "  }" dnl
                 ind "}" dnl
                 defer-port)
            )
          )
      )
    )
  (dis "const arc_t **" dnl nm"(const seqtype_t *seq)" dnl
       "{" dnl
       defer-port)
  (helper nt 0)
  (dis "}" dnl
       defer-port)
  (dis (TextWr.ToText defer-port) port)
  #t
  )

(define name-tree (treemap get-name the-map))

(define (doit)
  (let ((qqq (FileWr.Open "hohum.h"))
        (ppp (FileWr.Open "hohum.c")))
    (dis "*** building C code..." dnl)

    (dis "#include \"hohum.h\"" dnl
         "#include <assert.h>" dnl
         "#include \"seqtype.h\"" dnl
         dnl
         ppp)

    (dis "#ifndef _HOHUM_H" dnl 
         "#define _HOHUM_H" dnl
         "#include \"seqtype.h\"" dnl

         qqq)
    
    (dis "*** compiling chip address offset tree..." dnl)
    (let ((nm "ragged2addr"))
      (compile-offset-c the-chip-offset-tree nm ppp)
      (dis "chipaddr_t "nm"(const seqtype_t *);" dnl qqq)
      )

    (dis "*** compiling in order field offset tree..." dnl)
    (let ((nm "ragged2inorderid"))
      (compile-offset-c the-fields-offset-tree nm ppp)
      (dis "long "nm"(const seqtype_t *);" dnl qqq)
      )

    (dis "*** setting up static symbols..." dnl)
    (record-symbols)
    (dump-symbols ppp)
    (dump-sizes ppp)

    (dis "*** compiling names tree..." dnl)
    (let ((nm "ragged2arcs"))
      (compile-child-arc-c name-tree nm ppp)
      (dis "const arc_t **"nm"(const seqtype_t *);" dnl qqq)
      )

    (dis "#endif" dnl qqq)
    (Wr.Close ppp)
    (Wr.Close qqq)
    )
  )


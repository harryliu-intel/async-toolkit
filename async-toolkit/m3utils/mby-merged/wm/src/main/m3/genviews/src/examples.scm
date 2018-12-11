;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; examples for test & debug
;;

(define some-field '(field INITIAL_W0_OFFSET 56 8))

(define some-reg
  '(cont POL_DIRECT_MAP_POL1
         (field CTOK_HI 40 24)
         (field _RSVD1_ 37 3)
         (field CTOK_LO 24 13)
         (field _RSVD0_ 4 20)
         (field CFG 0 4)
         )
  )

(define some-hier2
  `(cont hier ,some-reg )
  )
         

(define some-array '(cont GLORT_CAM
                          (array 64 
                                 (field KEY_INVERT 16 16)
                                 (field KEY 0 16)
                                 )
                          )
  )

(define some-2d-array '(cont EGRESS_MST_TABLE
                             (array 4096 
                                    (array 5 
                                           (field FORWARDING 0 64)
                                           )
                                    )
                             )
  )

(define some-hier
           '(cont policers
              (cont POL_DIRECT_MAP_CTRL
                (field GO_COMPL 63 1)
                (field STATUS 62 1)
                (field OP_TYPE 61 1)
                (field _RSVD0_ 48 13)
                (field REG_ID 40 8)
                (field REG_SUB_ID 32 8)
                (field REG_INDX 0 32)
              )
              (cont POL_DIRECT_MAP_CTR0
                (field DATA_CNTB 0 44)
              )
              (cont POL_DIRECT_MAP_CTR1
                (field DATA_CNTP 0 36)
              )
              (cont POL_CFG
                (array 2 
                  (array 16 
                    (field _RSVD2_ 23 41)
                    (field UNPOLICE_DROP_CM 22 1)
                    (field UNPOLICE_DROP_PRECM 21 1)
                    (field CREDIT_FRAME_ERR 20 1)
                    (field CREDIT_L3_LEN_ERR 19 1)
                    (field _RSVD1_ 17 2)
                    (field CB 16 1)
                    (field CF 15 1)
                    (field COLOR_SELECT 14 1)
                    (field PRECEDENCE 13 1)
                    (field DEBIT_MODE 12 1)
                    (field L3_LEN_MODE 11 1)
                    (field _RSVD0_ 0 11)
                  )
                )
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
  (if (eq? (get-tag a) 'array) (cadr a) #f))

(define (zip-trees a b)
 ;; (if (not (tree-iso? a b)) (error "not tree-iso"))
  (cond ((null? a) '())
        ((atom? a) (cons a b))
        (else (cons (zip-trees (car a) (car b))
                    (zip-trees (cdr a) (cdr b))))))

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

(define (get-stride-bits array-spec addresses)
  ;; given a spec as follows
  ;; (base-addr elems . size)
  ;; compute element stride in bits
  (if (and (cadr array-spec)
           (> (cadr array-spec) 1))
      (let*  ((zeroth-field (car array-spec))
              (field-stride (/ (cddr array-spec)
                               (cadr array-spec)))
              (stride-field (+ zeroth-field field-stride))
              (zeroth-bit (fielddata->lsb
                           (FieldData.ArrayGet addresses zeroth-field)))
              (stride-bit (fielddata->lsb
                           (FieldData.ArrayGet addresses stride-field))))
        (- stride-bit zeroth-bit))
      '()))

(define (make-offset-tree accum-tree array-tree fields-tree addresses)
  ;;
  ;; format of an elem here is
  ;; (<offset> #f)
  ;;     -- offset from parent for non-array
  ;; (<offset> <elems> . <bits-stride>)
  ;;     -- offset from parent, # of elements, stride in bits
  ;;
  (define (helper p b)
    (if (null? p)
        '()
        (let ((this-addr
               (fielddata->lsb (FieldData.ArrayGet addresses (caar p)))))
          (cons (cons (- this-addr b)
                      (cons (cadar p) (get-stride-bits (car p) addresses))
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

(define the-offset-tree
  (make-offset-tree the-accum-tree
                    the-array-tree
                    fields-tree
                    the-addresses))

;; the following should give the same answer

(compute-offset-from-seq the-offset-tree '(1 0 0 0 0 0 0))

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
                 ind "  if (idx == -1) return arr + "b"L;" dnl
                 ind "  assert(idx >= 0 && idx < "size");" dnl
                 ind "  arr += idx * "stride"L;" dnl
                 port )
            (helper b child (+ 1 sp))
            (dis ind "}" dnl
                 port)
            )
          (begin
            (dis ind "{ /*nonarray */" dnl
                 ind "  switch(seq->d["sp"]) {" dnl
                 ind "    case -1: return arr + "b"L; break;" dnl
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
  (dis "long "nm"(const seqtype_t *seq)" dnl
       "{" dnl
       "   long arr=0L;" dnl
       port)
  (helper 0 ot 0)
  (dis "}" dnl
       port)
  #t
  )

(define (doit)
  (let ((ppp (FileWr.Open "hohum.c")))
    (dis "#include <assert.h>" dnl
         "#include \"seqtype.h\"" dnl
         dnl
         ppp)
    (compile-offset-c the-offset-tree "f" ppp)
    (Wr.Close ppp)
    )
  )

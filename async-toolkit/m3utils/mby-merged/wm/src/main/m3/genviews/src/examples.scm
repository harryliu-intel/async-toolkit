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
             (zip-trees (treesum 'nfields t) (treemap nuller t))))

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
  
(define zz (build-zip the-map))

(cnt-sequence-by-name the-map '(mpp 0 shm FWD_TABLE0 1 0 DATA))

(dis "length of the-addresses " (FieldData.ArraySize the-addresses) dnl)

(dis (stringify
      (FieldData.ArrayGet the-addresses
                          (get-zip-seq-offset
                           zz
                           (cnt-sequence-by-name
                            the-map
                            '(mpp 0 shm FWD_TABLE_MOD 639 255 DATA))))
      ) dnl)

;; average width of field (including space)
(* 8 (/ (+ (cdr (assoc 'byte lst)) (/ (cdr (assoc 'wid lst)) 8)) (+ (cdr (assoc 'id lst)) 1)))

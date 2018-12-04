;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; examples for test & debug
;;

(define some-field '(field INITIAL_W0_OFFSET 56 8))

(define some-reg
  '(reg parser_port_cfg_r
        (field INITIAL_W0_OFFSET 56 8)
        (field INITIAL_W1_OFFSET 48 8)
        (field INITIAL_W2_OFFSET 40 8)
        (field INITIAL_PTR 32 8)
        (field INITIAL_STATE 16 16)
        (field INITIAL_OP_MASK 4 12)
        (field INITIAL_OP_ROT 0 4)
        )
  )

(define some-array `(array 16 ,some-reg))

(define some-2d-array `(array 256 ,some-array))

(define some-child ' (child EM_HASH_LOOKUP
                          (array 32768 
                            (reg em_hash_lookup_r
                              (field PTR 64 20)
                              (field RSVD1_ 52 12)
                              (field SELECT_4 48 4)
                              (field SELECT_3 44 4)
                              (field SELECT_2 40 4)
                              (field SELECT_1 36 4)
                              (field SELECT_0 32 4)
                              (field MASK 0 32)
                            )
                          )
                          )
  )

(define some-container
  '(container addrmap mby_ppe_cgrp_em_map
              (child A
                     (reg xxx
                          (field STUFF 0 64)
                          )
                     )
              (child B
                     (array 32 
                            (container regfile b_rf
                                       (child BB
                                              (array 8 
                                                     (reg bb_r
                                                          (field DATA 0 64)
                                                          )
                                                     )
                                              )
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

(define hx
  '(16117
    (112 (112 (7 (1) (1) (1) (1) (1) (1) (1))))
    (128 (128 (8 (1) (1) (1) (1) (1) (1) (1) (1))))
    (1 (1 (1)))
    (1 (1 (1)))
    (2048 (2048 (64 (64 (64 (4 (1) (1) (1) (1)))))))
    (1024 (1024 (32 (32 (32 (2 (1) (1)))))))
    (2048 (2048 (64 (64 (64 (4 (1) (1) (1) (1)))))))
    (1536 (1536 (48 (48 (48 (3 (1) (1) (1)))))))
    (1024 (1024 (32 (32 (32 (2 (1) (1)))))))
    (5120 (5120 (160 (160 (160 (5 (1) (1) (1) (1) (1)))))))
    (256 (256 (128 (128 (128 (2 (1) (1)))))))
    (256 (256 (128 (128 (128 (2 (1) (1)))))))
    (2560 (2560 (160 (160 (160 (2 (1) (1)))))))
    (3 (3 (1) (1) (1)))
    )
  )

(define hy '(7 (1) (1) (1) (1) (1) (1) (1))   )

(define (trunc-stringify x)  (error-append (stringify x)))

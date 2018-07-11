(define deriv-dir "../AMD64_LINUX/")

(define action-sets
  `((bitstruct action-set-nop
               ((m3 ActionSetNop))
               ((rsvd0 24 (constant 16_0))
                (op 5 (constant 16_0))
                (prec 3))
    )

    (bitstruct action-set4-4b
               ((m3 ActionSet4_4b))
               ((value4 (array 4 4))
                (index 4)
                (enable (array 4 1))
                (op 5 (constant 16_1))
                (prec 3))
    )

    (bitstruct action-set8-1b
               ((m3 ActionSet8_1b))
               ((value1 (array 8 1))
                (enable (array 8 1))
                (index 4)
                (rsvd0 4 (constant 16_0))
                (op 5 (constant 16_2))
                (prec 3))
    )

    (bitstruct action-set3-1b
               ((m3 ActionSet3_1b))
               ((index_a 6)
                (va 1)
                (ea 1)
                (index_b 6)
                (vb 1)
                (eb 1)
                (index_c 6)
                (vc 1)
                (ec 1)
                (op 5 (constant 16_4))
                (prec 3))
    )

    (bitstruct action-set3-4b
               ((m3 ActionSet3_4b))
               ((value4_a 4)
                (value4_b 4)
                (value4_c 4)
                (index_a 4)
                (index_b 4)
                (index_c 4)
                (op 2 (constant 16_1))
                (prec 3))
    )

    (bitstruct action-set1-24b
               ((m3 ActionSet1_24b))
               ((value24 24)
                (index 4)
                (op 1 (constant 16_1))
                (prec 3))
    )
 ))
    
(compile! action-sets)
(exit)
    
    

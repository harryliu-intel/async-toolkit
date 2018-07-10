(define deriv-dir "../AMD64_LINUX/")

(define action-sets
  `((bitstruct action-set-nop
               ((m3 ActionSetNop))
               ((rsvd0 24 (constant 16_0))
                (op 5 (constant 16_0))
                (prec 3)))

    (bitstruct action-set4-4b
               ((m3 ActionSet4_4b))
               ((value4_0 4)
                (value4_1 4)
                (value4_2 4)
                (value4_3 4)
                (index 4)
                (enable0 1)
                (enable1 1)
                (enable2 1)
                (enable3 1)
                (op 5 (constant 16_1))
                (prec 3)))
    ))

(compile! action-sets)
(exit)
    
    

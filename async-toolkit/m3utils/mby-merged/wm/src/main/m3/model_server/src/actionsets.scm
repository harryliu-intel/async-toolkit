(define deriv-dir "../AMD64_LINUX/")

(define action-sets
  `((bitstruct action-set-nop
               ((m3 ActionSetNop))
               ((rsvd0 24 (constant 16_0))
                (op 5 (constant 16_0))
                (prec 3)))

    (bitstruct action-set4-4b
               ((m3 ActionSet4_4b))
               ((value4 (array 4 4))
                (index 4)
                (enable (array 4 1))
                (op 5 (constant 16_1))
                (prec 3)))
    )

  )
(compile! action-sets)
(exit)
    
    

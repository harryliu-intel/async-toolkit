(define constants
  `((constants parser-sizes
               u64 ;; doesnt matter here
               ((m3 MbyParserSizes))

               (
                (n-meta 32)
                (n-keys 84)
                (n-flags 48)
                (n-ptrs 8)
                )
               )))

(compile! constants)




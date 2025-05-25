(require-modules "hashtable" "struct" "display")

;; must have Modula-3 interface RTType loaded

(define lookup-typecode '())

(define (do-setup)

  (define typecode-table (make-string-hash-table 100))

  (define (make-typecode-table max-tc)    

    (unwind-protect
     (begin(typecode-table 'update-entry! (rtbrand-getname max-tc) max-tc) #t)
     () ())
    
    (if (>= max-tc 0)
        (make-typecode-table (- max-tc 1))))
  
  (set! lookup-typecode (lambda(typename) (typecode-table 'retrieve typename)))

  (make-typecode-table (RTType.MaxTypecode))

  'ok
  )

(do-setup)



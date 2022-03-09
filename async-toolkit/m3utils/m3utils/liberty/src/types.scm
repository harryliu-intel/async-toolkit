(require-modules "hashtable" "struct")

(define typecode-table (make-string-hash-table 100))

(define (make-typecode-table max-tc)    

  (unwind-protect
   (begin(typecode-table 'update-entry! (rtbrand-getname max-tc) max-tc) #t)
   '() '())

  (if (>= max-tc 0)
      (make-typecode-table (- max-tc 1))))

(define (lookup-typecode typename) (typecode-table 'retrieve typename))

(make-typecode-table (RTType.MaxTypecode))



(define (m3-expand-type type)
  (cond ((boolean-type? type) "BOOLEAN")
        ((string-type? type) "TEXT")
        ((integer-type? type) (m3-expand-integer-type type))
        ((array-type? type) (m3-expand-array-type type))
        ((struct-type? type) (m3-expand-struct-type type)) ;; hmm
        (else (error "Unknown type " type))
        )
  )

(define (m3-expand-array-type type)
  ;; this isnt right, this is just an open array
  ;; -- we need the range.
  
  (string-append "ARRAY OF " (m3-expand-type (caddr type))))

(define *big64* (bn 64))
(define *big63* (bn 64))

(define m3-word-min *big0*)
(define m3-word-max (xnum--(xnum-<< *big1* *big64*) *big1*))

(define m3-integer-min (xnum-- (xnum-<< *big1* *big63*)))
(define m3-integer-max (xnum-- (xnum-<< *big1* *big63*) *big1*))

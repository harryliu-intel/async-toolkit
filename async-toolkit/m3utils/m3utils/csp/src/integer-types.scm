;; we'll keep the maximum size stupidly small for now, just to exercise
;; the code without bogging down the machines with printing (which is
;; the slow part of the routine)

(define *maximum-size* 128) ;; largest finite size we'll tolerate

(define *maximum-sint-range* (make-sint-range *maximum-size*))
(define *maximum-uint-range* (make-uint-range *maximum-size*))

(define (make-intdecls prog)

  ;; return a table of all the integer type declarations
  ;; if a type is an array type, we store the element type, not the array type
  ;; itself
  
  (define tbl (make-hash-table 100 atom-hash))
  
  (define (s-visitor s)
    (case (get-stmt-type s)
      ((var1)
       (let ((id (get-var1-id s))
             (ty (get-var1-base-type s)))

             (if (integer-type? ty)

                 (tbl 'add-entry! id (get-type-range ty))))
       )

      ;; we need to handle the loops separately, since we know
      ;; more about their ranges than we do about declared variables
      ;; (which we only know widths for)
      
      );;esac
    s
    )
  (visit-stmt prog s-visitor identity identity)
  tbl
  )

(define (make-arrdecls prog)

  ;; return a table of all the array type declarations
  
  (define tbl (make-hash-table 100 atom-hash))
  
  (define (s-visitor s)
    (case (get-stmt-type s)
      ((var1)
       (let ((id (get-var1-id s))
             (ty (get-var1-type s)))

             (if (array-type? ty) (tbl 'add-entry! id ty))
             );;tel
       )

      );;esac
    s
    )
  (visit-stmt prog s-visitor identity identity)
  tbl
  )

(define (make-integer-type signed bits)
  `(integer #f ,signed ,(force-bigint bits) ()))

(define (get-smallest-type range)
  ;; we preferentially choose the uint (what CSP calls "int(.)")
  
  (cond ((range-contains? *maximum-uint-range* range)
         (make-uint-type range))
        
        ((range-contains? *maximum-sint-range* range)
         (make-sint-type range))

        (else *default-int-type*)))

(define (make-uint-type range)
  (if (range-infinite? range)
      *default-int-type*
      (let ((bits (xnum-clog2 (xnum-+ (range-max range) *big1*))))
        (make-integer-type #f (max 1 bits)))))

(define (make-sint-type range)
  (if (range-infinite? range)
      *default-int-type*
      (let* ((max (range-max range))
             (min (range-min range))
             
             (max0
              (if (xnum-< min *big0*)
                  (xnum-- (xnum-abs min) *big1*) ;; need one less value for neg
                  (xnum-abs min)))
             
             (max1 (xnum-abs max))

             (mmax (xnum-max max0 max1))

             (bits (xnum-+ *big1* (xnum-clog2 (xnum-+ mmax *big1*)))))
        (make-integer-type #t (max 1 bits)))))


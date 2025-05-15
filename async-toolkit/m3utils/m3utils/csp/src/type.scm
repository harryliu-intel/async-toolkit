(define (type-dbg . x)
;;  (apply dis x)
  )

(define (boolean-type? t) (eq? 'boolean (car t)))

(define *default-boolean-type* '(boolean #f))
(define *const-boolean-type* '(boolean #t))

(define (string-type? t) (eq? 'string (car t)))

(define *default-string-type* '(string #f))
(define *const-string-type* '(string #t))

(define (make-array-type extent elemtype) `(array ,extent ,elemtype))

(define (array-type? t) (and (pair? t) (eq? 'array (car t))))

(define (array-extent      at)  (cadr at))
(define (array-elemtype    at)  (caddr at))

(define (array-base-type   at)
  (let* ((res
          (if (array-type? at) (array-base-type (array-elemtype at)) at)))
    (dis "array-base-type : " at " -> " res dnl)
    res
    )
  )

(define (make-array-type extent elem-type)
  `(array ,extent ,elem-type))

(define (struct-type? t) (and (pair? t) (eq? 'structure (car t))))
(define (struct-type-name t)
  (if (not (struct-type? t))
      (error "not a struct type : " t)
      (caddr t)
      )
  )

(define s-x  #f)
(define s-bt #f)
(define s-sd #f)

(define *special-return-funcs*
  `(
;; these are intrinsics that don't return int    
;;   fname    return-type    
    (string ,*default-string-type*)
    )
  )

(define (derive-type x syms func-tbl struct-tbl cell-info)
  (type-dbg "derive-type : x : " x dnl)
  
  (cond  ((ident? x)
          ;;
          ;; this part is very tricky!
          ;; derive-type is used just to construct types for new
          ;; temporary variables, so if we are examining an integer
          ;; variable, we return the default int type.
          ;;
          ;; cleaning up the integer types will be done during a
          ;; later pass...
          ;;
          (let ((id-type (retrieve-defn (cadr x) syms)))
            (if (integer-type? id-type) *default-int-type* id-type)))
         
         ((bigint? x) *default-int-type*)
         ((boolean? x) *default-boolean-type*)
         ((probe? x) *default-boolean-type*)
         ((string? x) *default-string-type*)

         ((or (recv-expression? x) (peek? x))
          (let* ((port-tbl  (make-port-table cell-info)) ;; hrmph, slow
                 (channel-designator (cadr x))
                 (id        (get-designator-id channel-designator))
                 (chan-decl (port-tbl 'retrieve id))
                 (width     (get-channel-type-bit-width (cadddr chan-decl))))
            (make-integer-type #f width)
            )
          )
         
         ((pair? x)
          (cond ((member (car x) *string-ops*) *default-string-type*)
                ((member (car x) *integer-ops*) *default-int-type*)
                ((member (car x) *boolean-ops*) *default-boolean-type*)
                ((member (car x) *polymorphic-ops*)
                 (derive-type (cadr x) syms func-tbl struct-tbl cell-info))

                ((apply? x)
                 (dis "derive-type of apply : " x dnl)
                 
                 (let* (
                        (fnam         (get-apply-funcname x))
                        (fdef         (func-tbl 'retrieve fnam))
                        (failed       (eq? fdef '*hash-table-search-failed*))
                        (special-func (assoc fnam *special-return-funcs*))
                        (res
                         (cond
                          (special-func (cadr special-func))
                          (failed *default-int-type*)
                          (else (get-function-return fdef)))
                         )
                        )
                   
                   (dis "derive-type of apply : result : " res dnl)
                   res
                   )
                 
                 ;;          (error)
                 )


                ((loopex? x)
                 ;; a bit tricky: a loopex is the only expression that
                 ;; also introduces a new symbol into the environment!
                 (derive-type (construct-loopex-binop x)
                              (make-loopex-frame x syms)
                              func-tbl
                              struct-tbl cell-info))
                
                ((array-access? x)
                 (peel-array
                  (derive-type (array-accessee x) syms func-tbl struct-tbl cell-info)))
                
                ((member-access? x)
                 (set! s-x x)
                 (let* ((base-type (derive-type
                                    (member-accessee x) syms func-tbl struct-tbl cell-info))
                        (struct-def (struct-tbl 'retrieve (get-struct-name base-type)))
                        (struct-flds (get-struct-decl-fields struct-def))
                        (accesser   (member-accesser x))
                        )
                   
                   (set! s-bt base-type)
                   (set! s-sd struct-def)
                   
                   (dis "member-access   x           : " x dnl)
                   (dis "member-access   base-type   : " (stringify base-type) dnl)
                   (dis "member-access   struct-def  : " (stringify struct-def) dnl)
                   (dis "member-access   struct-flds : " (stringify struct-flds) dnl)
                   
                   (if (not (symbol? accesser))
                       (error "member-access : not an accesser : " accesser))
                   
                   (get-struct-decl-field-type struct-def accesser)
                   )
                 )
                
                ((bits? x)
                 (if (equal? (get-bits-min x) (get-bits-max x))
                     *single-bit-type*
                     *default-int-type*))
                
                (else (error "derive-type : don't know type of " x))
                )
          )
         (else (error "derive-type : don't know type of " x)))
  )

  
(define (literal-type literal)
  (cond ((boolean? literal) '(boolean #t))
        ((bigint? literal)  *default-int-type*)
        ((string? literal)  '(string #t))
        (else (error "literal-type : not a literal : " literal))))


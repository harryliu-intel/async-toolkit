
(define (m3-expand-type type)
  (cond ((boolean-type? type) "BOOLEAN")
        ((string-type? type) "TEXT")
        ((integer-type? type) (m3-expand-int-type type))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; map identifiers (we could do this so much better directly in M3)
;;

(define (string->integer str1)
  ;; single character symbol sym1
  (char->integer (car (string->list str1))))

(define (symbol->integer sym1)
  ;; single character symbol sym1
  (string->integer (symbol->string sym1)))


(define (make-char-in-range? lo hi)
  (lambda(ch)
    (and (>= (char->integer ch) (string->integer lo))
         (<= (char->integer ch) (string->integer hi)))))

(define alpha-checker
  (filter-or (make-char-in-range? "a" "z")
             (make-char-in-range? "A" "Z")))

(define digit-checker
  (make-char-in-range? "0" "9"))

(define *us* (char->integer #\_))

(define (m3-char c)
  (cond ((alpha-checker c) (list (char->integer c)))
        ((digit-checker c) (list (char->integer c)))
        (else  `(,*us* 
                  ,@(map char->integer
                         (string->list (number->string (char->integer c))))

                  ,*us*))))
        
(define (m3-ident scm-ident) ;; Good grief

  ;; this just turns a Scheme symbol into a legal Modula-3 identifier
  
;;  (dis "m3-ident : " scm-ident dnl)
  (string-append "m3__"
                 (list->string
                  (map integer->char
                       (apply append
                              (map m3-char
                                   (string->list
                                    (symbol->string scm-ident))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;
;;

(define (m3-write-port-decl w pdef)
  (w "    " (pad 40 (m3-ident (get-port-id pdef))) " : REF "
     (m3-convert-port-type (get-port-def-type pdef))
     ".T;" dnl
     )
  )

(define (m3-format-port-ass pdef)
  (let ((ident (m3-ident (get-port-id pdef))))
    (string-append ident " := " ident)
    )
  )
                  
(define (m3-map-decltype type)
  (cond ((string-type?  type) "TEXT")
        ((boolean-type? type) "BOOLEAN")
        ((array-type?   type) (error "not done") )
        ((struct-type?  type) (error "not done") )
        ((integer-type? type)
         (let ((width (cadddr type)))
           (cond ((not (bigint? width)) "DynamicInt.T")
                 ((caddr type)
                  (string-append "SInt" (BigInt.Format width 10) ".T"))
                 (else
                  (string-append "UInt" (BigInt.Format width 10) ".T"))
                 );;dnoc
           );;tel
         )
        (else (error "m3-map-decltype : unknown type to map : " type))
        );;dnoc
  )

(define (m3-convert-vardecl v1)
  (let ((id (get-var1-id v1))
        (ty (get-decl1-type (get-var1-decl1 v1)))
        )
    (string-append (pad 40 (m3-ident id)) " : " (m3-map-decltype ty) ";")
    )
  )

(define (m3-write-shared-locals w the-blocks cell-info the-decls)
  (let* ((shared-local-ids (get-shared-variables the-blocks cell-info))
         (v1s              (map (curry find-decl the-decls) shared-local-ids))
         )
    (w "    (* shared locals list *)" dnl)
    (map (lambda(dt) (w "    " dt dnl))
         (map m3-convert-vardecl v1s))
    )
  )

(define (m3-write-process-closure-list w the-blocks)
  (w "    (* closures *)" dnl)

  (map (lambda(blk)
         (let* ((btag (m3-ident (cadr (get-block-label blk)))))
           (w "    " (pad 40 btag "_Cl") " : Closure;" dnl)
           );;*tel
         )
       (cdr the-blocks)
       );;pam
  )

(define (m3-write-proc-frame-decl w port-tbl the-blocks cell-info the-decls)
  (w dnl
     "TYPE" dnl)
  (w "  Frame = Process.Frame OBJECT" dnl)
  (m3-write-port-list w cell-info)
  (w dnl)
  (m3-write-shared-locals w the-blocks cell-info the-decls)
  (w dnl)
  (m3-write-process-closure-list w the-blocks)
  (w dnl)
  (w "  END;" dnl dnl)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m3-write-block-decl w)
  (w dnl
     "TYPE Block = PROCEDURE (cl : Closure) : BOOLEAN;" dnl
     dnl)
  )
  
(define (m3-write-closure-decl w)
  (w dnl
     "TYPE" dnl
     "  Closure = Process.Closure OBJECT" dnl
     "    frame  :  Frame;" dnl
     "    block  :  Block;" dnl
     "  OVERRIDES" dnl
     "    run := Run;" dnl
     "  END;" dnl
     dnl
     "PROCEDURE Run(cl : Closure) = BEGIN EVAL cl.block(cl) END Run;" dnl
     dnl
     )
  )

(define (m3-write-port-list w cell-info)
  (let ((plist (get-ports cell-info)))

    (w "    (* port list *)" dnl)
  
    (map (curry m3-write-port-decl w) plist) 
    )
  )

(define (m3-write-build-signature w cell-info)
  (w "PROCEDURE Build(" dnl)
  (m3-write-port-list w cell-info)
  (w ")")
  )

(define (m3-write-build-decl w cell-info)
  (m3-write-build-signature w cell-info)
  (w ";" dnl dnl)
  )

(define (indent-writer w by) (lambda x (apply w (cons by x))))

(define (m3-write-build-defn w cell-info the-blocks)
  (let ((proc-ports (get-ports cell-info)))
    (m3-write-build-signature w cell-info)
    (w " = " dnl)
    (w "  BEGIN" dnl)
    (w "    WITH frame = NEW(Frame," dnl
       "                     id := NextFrameId()," dnl)

    (let ((asslist (map m3-format-port-ass proc-ports)))
      (map (lambda(ass)
             (w "                     " ass ";" dnl))
           asslist)
      );;tel
    
    (w "      ) DO" dnl)

    ;; build body
    (let ((iw (indent-writer w "     ")))

      (let* ((inlist (filter input-port? proc-ports))
             (ids    (map m3-ident (map get-port-id inlist)))
             )
        (map (lambda(m3id)
               (iw "MarkReader(" m3id ", frame);" dnl))
             ids
             )
        )
      (w dnl)
      
      (let* ((outlist (filter output-port? proc-ports))
             (ids    (map m3-ident (map get-port-id outlist)))
             )
        (map (lambda(m3id)
               (iw "MarkWriter(" m3id ", frame);" dnl))
             ids
             )
        )

      (w dnl)

      (let* ((blk-labels (map m3-ident
                              (map cadr
                                   (map get-block-label (cdr the-blocks)))))
             (iiw (indent-writer iw (pad 22 "")))
             )
        (map (lambda(m3lab)
               (iw  (pad 22 "frame." m3lab "_Cl")
                    " := NEW(" m3lab "Closure," dnl)
               (iiw "        id      := NextId()," dnl)
               (iiw "        frameId := frame.id," dnl)
               (iiw "        frame   := frame," dnl)
               (iiw "        block   := Block_" m3lab ");" dnl dnl)
               )
             blk-labels)
        );;*tel

      (iw "Schedule(frame." (m3-ident (cadar the-blocks))"_Cl);" dnl)
      );;tel (iw)
    
    (w "    END(*WITH*)" dnl)
    (w "  END Build;" dnl dnl)
    )
  )

;; note that the block closures need to be extended!
;; some block closures will have pointers to parallel-loop indices

(define (m3-write-text w the-blocks cell-info)

  )

(define (m3-write-modu-imports w)
  (w "IMPORT CspCompiledProcess AS Process;" dnl)

  )

(define (m3-write-intf-imports w)
  (w "IMPORT CspCompiledProcess AS Process;" dnl)
  
  )

(define (m3-convert-port-type ptype)
  ;; return string name of interface that defines the channel type
  ;; requested by the CSP code
  (let ((stype (port-type-short ptype)))
    (case (car stype)
      ((node) (string-append "Node" (BigInt.Format (cadr stype) 10)))
      ((bd)   (string-append "UInt" (BigInt.Format (cadr stype) 10) "Chan"))
      (else (error))
      )
    )
  )

(define (m3-convert-port-ass-type ptype)
  ;; return string name of interface that defines the assignable data
  ;; on a port requested by the CSP code
  (let ((stype (port-type-short ptype)))
    (case (car stype)
      ((node) (string-append "UInt" (BigInt.Format (cadr stype) 10)))
      ((bd)   (string-append "UInt" (BigInt.Format (cadr stype) 10)))
      (else (error))
      )
    )
  )

(define (m3-convert-port-ass-bits ptype)
  (let ((stype (port-type-short ptype)))
    (BigInt.ToInteger (cadr stype))))

(define (m3-convert-port-ass-category ptype)
  (if (> (m3-convert-port-ass-bits ptype) 64) 'wide 'native))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (declared-type pc expr)

  ;;
  ;; This is similar to derive-type in the front-end;
  ;; the difference is that derive-type actually ignores declared bit
  ;; widths, so that these can be inferred later.
  ;;
  ;; Here, we have already inferred the bit widths, so we need to pull
  ;; them out of our tables.
  ;; 

  
  (let ((symtab       (car pc))
        (cell-info    (cadr pc))
        (struct-tbl   (caddr pc)))
    (if (not (designator? expr))
        (error "declared-type : not a designator : " expr))

    (let* ((declared-id (get-designator-id expr))
           (id-type     (symtab 'retrieve declared-id)))
      (cond ((array-access? expr)
             (peel-array (declared-type pc (array-accessee expr))))

            ((member-access? expr)
             (let* ((base-type (declared-type
                                pc (member-accessee expr) ))
                    (struct-def (struct-tbl 'retrieve (get-struct-name base-type)))
                    (struct-flds (get-struct-decl-fields struct-def))
                        (accesser   (member-accesser x))
                        )
               (get-struct-decl-field-type struct-def accesser)
               )
             )

            ((bits? expr) (error) )  ;; do we support LHS bits?  I forget...

            (else id-type))
         )
    
    )
  )

(define (m3-derive-type pc expr)
  (let ((symtab       (car pc))
        (cell-info    (cadr pc))
        (struct-tbl   (caddr pc)))
    (derive-type expr (list symtab) '() struct-tbl cell-info)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m3-format-designator pc designator)
  ;; actually, we need to look up the designator to know
  ;; whether it is
  ;;
  ;; (1) a block local (which is just declared in a VAR in the block) or
  ;;
  ;; (2) a process local (shared across blocks) or
  ;;
  ;; (3) a parallel-loop dummy (which is accessed via a pointer from the block
  ;;     closure)
  ;;
  ;; (each of which has a different access method)

  (cond ((ident? designator) (m3-format-varid pc (cadr designator)))

        (else (error "not yet"))))

(define (m3-format-varid pc id)
  ;; given an id (as part of a designator) in Scheme,
  ;; generate a correctly formatted reference for it for the
  ;; compiled program, from within a block procedure

  (let ((the-scopes (cadddr pc)))
    (case (the-scopes 'retrieve id)
      ((*hash-table-search-failed*) (error "unknown id : " id))
      
      ((port)           (string-append "cl.frame." (m3-ident id)))
      
      ((process)        (string-append "cl.frame." (m3-ident id)))
      
      ((block)          (m3-ident id))
      
      ((parallel-dummy) ;; this is sketchy
       (string-append "cl." (m3-ident id)))
      
      (else (error))
      )
    )
  )


;;
;; integer types --
;;
;; these have a whole host of representations!
;;
;; narrow types are fundamentally INTEGERs (64 bits wide)
;; and the operations distinguish between Word.T and INTEGER.
;; This only really matters for integers exactly 64 bits wide.
;;
;; narrow uint (0..64)
;; narrow sint (0..64)
;;
;; There are wider integers than 64 bits, which are stored as
;; multiple words of Word.T and a sign bit (sign-magnitude), for fixed
;; widths.
;;
;; wide uint (65..??)
;; wide sint (65..??)
;;
;; Finally, there are dynamic integers, which are stored with a master
;; record containing a single word (the LSW) plus control information,
;; and any higher-order words in an auxiliary heap-allocated array of
;; Word.T
;;
;; dynamic int (0 .. +inf)
;;

(define (m3-sint-type? t)
  (and (integer-type? t) (caddr t)))

(define (m3-uint-type? t)
  (and (integer-type? t) (not (caddr t)) (bigint? (cadddr t))))

(define (m3-dynamic-int-type? t) ;; what's this?
  (and (integer-type? t) (not (caddr t)) (not (bigint? (cadddr t)))))

(define (m3-int-type-width t)
  (if (m3-dynamic-int-type? t) (error) (BigInt.ToInteger (cadddr t))))

(define (m3-natively-representable-type? t)
  ;; if a type is "narrow", we can do regular math on it

  ;; this means that the bit pattern matches the bit pattern of
  ;; a Modula-3 INTEGER, and that the operations do too

  ;; hmm there will be a very special case for operands that are
  ;; unsigned and exactly 64 bits wide.
  
  (and (integer-type? t)
       (or (and (m3-sint-type? t) (<= (m3-int-type-width t) 64))
           (and (m3-uint-type? t) (<= (m3-int-type-width t) 63)))) ;; hmm.
  )

(define (m3-uint64-type? t) ;; 64-bit uint is a special case: rep : Word.T
  (and (m3-uint-type? t) (= (m3-int-type-width t) 64))) 
  
(define (m3-wide-int-type? t)
  (and (not (m3-dynamic-int-type?            t))
       (not (m3-uint64-type?                 t))
       (not (m3-natively-representable-type? t))
       );;dna
  )

(define (exists? pred? lst)
  (eval (apply or (map pred? lst)))) ;; why do we need eval?

(define (forall? pred? lst)
  (eval (apply and (map pred? lst)))) ;; why do we need eval?
  
(define (m3-compile-scalar-int-assign pc stmt)
  (let* ((lhs (get-assign-lhs stmt))
         (lty (declared-type pc lhs)))

    (cond ((m3-natively-representable-type? lty)
           (m3-compile-native-int-assign pc stmt))

          ((m3-dynamic-int-type? lty)
           (m3-compile-wide-int-assign pc stmt))

          ((m3-wide-int-type? lty)
           (m3-compile-wide-int-assign pc stmt))

          (else
           (error)))
    )
  )

(define (operand-type pc expr)
  (if (literal? expr)
      (let ((lt (literal-type expr)))
        (if (integer-type? lt)
            (get-smallest-type (list expr expr))
            lt))
      (declared-type pc expr)))


(define (classify-type pc expr)
  (let ((ty (operand-type pc expr)))
    (cond ((m3-natively-representable-type? ty) 'native)
          ((m3-dynamic-int-type? ty) 'dynamic)
          ((integer-type? ty) 'wide)
          (else (error "not an integer : " expr))
          )
    )
  )

(define (max-type . x)
  (cond ((member 'dynamic x) 'dynamic)
        ((member 'wide    x) 'wide)
        (else (car x))))

(define (get-m3-int-intf t)
  (case t
    ((dynamic) "DynamicInt")
    ((wide)    "WideInt")
    ((native)  "NativeInt")
    (else (error "get-m3-int-intf : " t))
    )
  )

(define (m3-compile-convert-type from to arg)
  (if (eq? from to)
      arg
      
      (sa (get-m3-int-intf to) ".Convert" (get-m3-int-intf from)
          "("
          arg
          ")"
          )
      )
  )

(define (format-int-literal cat x)
  (case cat
    ((native)
     (Fmt.Int (BigInt.ToInteger x) 10))

    ((wide) ;; we need to dump it out in 64-bit chunks
     (error "not yet"))

    ((dynamic)
     (sa "DynamicInt.FromWideInt(" (format-int-literal 'wide x) ")"))

    (else (error))
    )
  )

(define (force-type pc cat x)
  (if (literal? x)
      (format-int-literal cat x)
      (let ((x-category (classify-type pc x)))
        (m3-compile-convert-type x-category
                                 cat
                                 (m3-format-designator pc x)
                                 ))))


(define m3-binary-infix-ops '(+ / % * -))

(define (m3-map-symbol-op op)
  (case op
    ((/) "DIV")
    ((%) "MOD")
    ((+) "+")
    ((*) "*")
    ((-) "-")
    )
  )

(define (m3-map-named-op op)
  (case op
    ((&)    "And")
    ((^)    "Xor")
    ((<<)   "Shl")
    ((**)   "Pow")
    ((|) ;; |)
            "Or")
    (else (error "m3-map-named-op : " op))
    );; esac
  )


(define (m3-compile-binop cat op a-arg b-arg)
  (cond ((and (eq? 'native cat) (member op m3-binary-infix-ops))
         (sa "( " a-arg " " (m3-map-symbol-op op) " " b-arg " )"))
        ((and (eq? 'native cat) (eq? '>> op))
         (sa "NativeInt.Shift( " a-arg " , -( " b-arg " ) )"))
        (else (sa (get-m3-int-intf cat) "." (m3-map-named-op op) "( "
                  a-arg " , " b-arg " )"))
        )
  )

(define (m3-compile-typed-binop pc tgt op a b)
  (let* ((op-type (max-type tgt (classify-type pc a) (classify-type pc b)))
         (opx     (m3-compile-binop op-type
                                    op
                                    (force-type pc op-type a)
                                    (force-type pc op-type b))))
    (m3-compile-convert-type op-type tgt opx))
  )

  
(define (m3-compile-native-int-assign pc x)
  (dis "m3-compile-native-int-assign : x : " x dnl)
  ;; assign when lhs is native
  (let* ((lhs (get-assign-lhs x))
         (rhs (get-assign-rhs x)))
    (sa (m3-format-designator pc lhs) " := "
        (cond ((ident? rhs)
               (force-native rhs))
        
              ((bigint? rhs)
               (BigInt.Format rhs 10))
              
              ((binary-expr? rhs)
               (m3-compile-typed-binop pc 'native (car rhs) (cadr rhs) (caddr rhs)))
              
              ((unary-expr? rhs)
               (m3-compile-native-unop pc rhs))
              
              ((bits? rhs)
               (m3-compile-native-bits pc rhs))

              (else (error "m3-compile-native-int-assign"))
              );;dnoc
        )
    );;*tel
  )

(define (m3-compile-stringify-integer-value pc x)
  (if (bigint? x)
      (BigInt.Format x 10)
      (let ((type (declared-type pc x)))
        (cond
         
         ((m3-natively-representable-type? type)
          (string-append
           "Fmt.Unsigned("
           (m3-compile-integer-value pc x)
           ", base := 10)"
           ))
         
         ((m3-dynamic-int-type? type)
          (string-append
           "DynamicInt.Fmt("
           (m3-compile-integer-value pc x)
           ", base := 10)"
           ))
         
         (else
          (string-append
           "FixedBigInt.Format("
           (m3-compile-integer-value pc x)
           ", base := 10)"
           ))
         )
        )
      )
  )

(define (m3-compile-integer-value pc x)
  (define (err) (error "m3-compile-integer-value : can't map to string : " x))
  
  (cond ((ident? x)  
         (let ((type (declared-type pc x)))
           (cond
            ((integer-type?  type)
             (m3-format-varid pc (cadr x)))

            (else (err))
            )
           )
         )

        (else (err))
        );;dnoc
  )

(define (+? x) (and (pair? x) (eq? '+ (car x))))

(define (m3-compile-string-expr pc x)
  (define (err) (error "m3-compile-string-expr : can't map to string : " x))
  
  (cond ((ident? x)  
         (let ((type (declared-type pc x)))
           (cond
            ((integer-type?  type)
             (m3-compile-stringify-integer-value pc x))

            ((string-type? type)
             (m3-format-varid pc (cadr x)))

            (else (err))
            )
           )
         )

        ((string? x) (stringify x))
        
        ((+? x)      (string-append
                      "Text.Cat( "
                      (m3-compile-string-expr pc (cadr x))
                      " , "
                      (m3-compile-string-expr pc (caddr x))
                      " )"))
        (else (err))
        );;dnoc
  )

(define sa string-append)

(define (m3-compile-assign pc stmt)
  (dis "m3-compile-assign : " stmt dnl)
  (let* ((lhs (get-assign-lhs stmt))
         (lty (declared-type pc lhs))
         (rhs (get-assign-rhs stmt)))

    (dis "m3-compile-assign : lhs : " lhs dnl)
    (dis "m3-compile-assign : lty : " lty dnl)
    (dis "m3-compile-assign : rhs : " (stringify rhs) dnl)

    (cond ((boolean-type? lty)
           (sa (m3-format-designator pc lhs) " := "
              (m3-compile-boolean-expr rhs)))

          ((string-type? lty)
           (sa (m3-format-designator pc lhs) " := "
              (m3-compile-string-expr pc rhs)))

          ((array-type? lty)
           (error "not yet"))

          ((struct-type? lty)
           (error "not yet"))
          
          ((integer-type? lty)
           (m3-compile-scalar-int-assign pc stmt))

          (else
           (error "???")))
    
    )
  )

(define (m3-compile-goto pc stmt)
  (dis "m3-compile-goto" dnl)

  (if (not (null? (cddr stmt))) (error "cant compile complex gotos yet"))
  
  (string-append "Release(cl.frame." (m3-ident (cadr stmt)) "_Cl);"
  " RETURN TRUE")
  )

(define (get-port-def-type pdef) (cadddr pdef))

(define (m3-compile-send pc stmt)
  (let* ((port-tbl     (caddddr pc))

         (port-des     (get-send-lhs stmt)) ;; doesnt work for arrays/structs
         (rhs          (get-send-rhs stmt))
         
         (port-id      (get-designator-id port-des))
         (port-def     (port-tbl 'retrieve port-id))
         (port-type    (get-port-def-type port-def))
         (port-typenam (m3-convert-port-type port-type))
         (copy-type    (m3-convert-port-ass-type port-type))
         (send-class   (classify-type pc (get-send-rhs stmt)))
         
         (m3-pname     (m3-format-varid pc port-id))
         (port-class   (m3-convert-port-ass-category port-type))

         (literal      (literal? (get-send-rhs stmt)))
         )

    (sa "VAR toSend : "
        copy-type " := " (force-type pc port-class rhs)
        "; BEGIN IF NOT "
        port-typenam ".Send( " m3-pname " , toSend , cl ) THEN RETURN FALSE END END"
        )
    )
  )

(define (m3-compile-recv pc stmt)
  (let* ((port-tbl     (caddddr pc))

         (port-des     (get-recv-lhs stmt)) ;; doesnt work for arrays/structs
         (rhs          (get-recv-rhs stmt))
         
         (port-id      (get-designator-id port-des))
         (port-def     (port-tbl 'retrieve port-id))
         (port-type    (get-port-def-type port-def))
         (port-typenam (m3-convert-port-type port-type))
         (copy-type    (m3-convert-port-ass-type port-type))
         (recv-class   (classify-type pc (get-recv-rhs stmt)))
         
         (m3-pname     (m3-format-varid pc port-id))
         (port-class   (m3-convert-port-ass-category port-type))
         (rhs-class    (classify-type pc rhs))

         )

    (sa "VAR toRecv : "
        copy-type
        "; BEGIN IF NOT "
        port-typenam ".Recv( " m3-pname " , toRecv , cl ); "
        (m3-format-designator pc rhs) " := "
        (m3-compile-convert-type
         recv-class
         rhs-class
         "toRecv"
         )
        " THEN RETURN FALSE END END"
        )
    )
  )

(define (m3-compile-eval pc stmt)
  (let ((expr (cadr stmt)))
    (string-append "EVAL " (m3-compile-intrinsic pc expr))
    )
  )

(define (m3-compile-intrinsic pc expr)
  (if (not (call-intrinsic? expr)) (error "not an intrinsic : " expr))

  (sa "CspIntrinsics." (symbol->string (cadr expr)) "("
      (m3-format-varid pc (cadaddr expr))
                       ")")

  )

(define *known-stmt-types* '(recv send assign eval goto local-if while))

(define (m3-compile-write-stmt w pc stmt)
  (let (
        (stmt-type (get-stmt-type stmt))
        (iw        (indent-writer w "      "))
        )

    (dis "m3-compile-write-stmt : stmt : " stmt dnl)
    
    (if (member stmt-type *known-stmt-types*)
        (iw ((eval (symbol-append 'm3-compile- stmt-type)) pc stmt) ";" dnl)
        (error "Unknown statement type in : " stmt)
        )          

  
  
    (iw "(* " (stringify stmt) " *)" dnl dnl)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The PROCESS CONTEXT pc contains three things:
;; car   : a symtab built from the-decls
;; cadr  : the cell-info
;; caddr : the struct-tbl
;;

(define (m3-write-block w pc blk)
  (dis (get-block-label blk) dnl)
  (dis "m3-write-block : " blk dnl)
  (let* ((btag (m3-ident (cadr (get-block-label blk))))
         (bnam (string-append "Block_" btag))
         (the-code (cddr (filter-out-var1s blk)))
         )
    ;; write decl.  We have to change this later for dynamic parallelism
    (w
     "PROCEDURE "bnam"(cl : Closure) : BOOLEAN =" dnl
     "  BEGIN" dnl
     "    WITH frame = cl.frame DO" dnl)
    
    ;; the block text goes here
    (map (curry m3-compile-write-stmt w pc) the-code)

    (w 
     "    END(*WITH*)" dnl
     "  END " bnam ";" dnl
     dnl)
    )
  )

(define (m3-write-blocks w the-blocks pc)
  (map (curry m3-write-block w pc)
       (cdr the-blocks))
  )

(define (m3-make-scope-map the-blocks cell-info)
  ;; a bound identifier can have four types of scope:
  ;; 1. block local
  ;; 2. process local
  ;; 3. parallel-loop dummy
  ;; 4. a port reference

  (define tbl (make-hash-table 100 atom-hash))

  (define (make-add! tag) (lambda(id)(tbl 'add-entry! id tag)))

  (map (make-add! 'port) (map get-port-id (get-ports cell-info)))

  (map (make-add! 'process) (get-shared-variables the-blocks cell-info))

  (map (make-add! 'parallel-dummy)
                  (apply append (map find-parallel-loop-dummies the-blocks)))


  (let* ((all-refs     (apply append (map find-referenced-vars the-blocks)))
         (block-locals (set-diff all-refs (tbl 'keys))))
    (map (make-add! 'block) block-locals)
    )

  tbl
  )

(define (find-parallel-loop-dummies prog)
  (define res '())
  (define (visit s)
    (if (eq? 'parallel-loop (get-stmt-type s))
        (begin (set! res (cons (get-loop-dummy s) res)) s)
        s)
    )

  (visit-stmt prog visit identity identity)
  res
  )

(define (do-m3!)
  (let* ((the-blocks text9)
         (cell-info  *cellinfo*)
         (port-tbl   *the-prt-tbl*)
         (the-decls  (gen-decls the-blocks *proposed-types*))
         (root       (m3-ident (string->symbol *the-proc-type-name*)))
         (i3fn       (string-append "build/" root ".i3"))
         (i3wr       (FileWr.Open i3fn))
         (m3fn       (string-append "build/" root ".m3"))
         (m3wr       (FileWr.Open m3fn))
         (the-scopes (m3-make-scope-map the-blocks cell-info))
         )

    (define (intf . x) (Wr.PutText i3wr (apply string-append x)))
    (define (modu . x) (Wr.PutText m3wr (apply string-append x)))
    ;; interface file
    
    (intf "INTERFACE " root ";" dnl dnl)

    (m3-write-intf-imports intf)

    (m3-write-build-decl intf cell-info)
    

    (intf dnl "END " root "." dnl)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (modu "MODULE " root ";" dnl dnl)

    (m3-write-modu-imports    modu)
    (m3-write-proc-frame-decl modu port-tbl the-blocks cell-info the-decls)
    (m3-write-block-decl      modu)
    (m3-write-closure-decl    modu)
    (m3-write-build-defn      modu cell-info the-blocks)

    (let ((pc (list
                         (m3-make-symtab the-decls)
                         cell-info
                         *the-struct-tbl*
                         the-scopes
                         (make-port-table cell-info))))
      (set! *proc-context* pc)
      (m3-write-blocks          modu the-blocks pc)
      )
    
    (modu dnl "BEGIN END " root "." dnl)
    
    (Wr.Close i3wr)
    (Wr.Close m3wr)
    )
  )

(define (m3-make-symtab the-decls)
  (define tbl (make-hash-table 100 atom-hash))

  (map (lambda(v1)(tbl 'add-entry!
                       (get-var1-id v1)
                       (get-var1-type v1)))
       the-decls)
  tbl
  )

(define (find-decl decls id)
  (cond ((null? decls) #f)
        ((eq? id (get-var1-id (car decls))) (car decls))
        (else (find-decl (cdr decls) id)))
  )

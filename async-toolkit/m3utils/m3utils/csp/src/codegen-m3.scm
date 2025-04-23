
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

(define (map-char c)
  (cond ((alpha-checker c) (list (char->integer c)))
        ((digit-checker c) (list (char->integer c)))
        (else  `(,*us* 
                  ,@(map char->integer
                         (string->list (number->string (char->integer c))))

                  ,*us*))))
        
(define (m3-map-ident scm-ident) ;; Good grief
;;  (dis "m3-map-ident : " scm-ident dnl)
  (string-append "m3__"
                 (list->string
                  (map integer->char
                       (apply append
                              (map map-char
                                   (string->list
                                    (symbol->string scm-ident))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;

(define (m3-write-port-decl w pdef)
  (w "    " (pad 40 (m3-map-ident (get-port-id pdef))) " : REF "
     (m3-convert-port-type (cadddr pdef))
     ".T;" dnl
     )
  )

(define (m3-format-port-ass pdef)
  (let ((ident (m3-map-ident (get-port-id pdef))))
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
           (cond ((not (bigint? width)) "DynamicInteger.T")
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
    (string-append (pad 40 (m3-map-ident id)) " : " (m3-map-decltype ty) ";")
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
         (let* ((btag (m3-map-ident (cadr (get-block-label blk)))))
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
             (ids    (map m3-map-ident (map get-port-id inlist)))
             )
        (map (lambda(m3id)
               (iw "MarkReader(" m3id ", frame);" dnl))
             ids
             )
        )
      (w dnl)
      
      (let* ((outlist (filter output-port? proc-ports))
             (ids    (map m3-map-ident (map get-port-id outlist)))
             )
        (map (lambda(m3id)
               (iw "MarkWriter(" m3id ", frame);" dnl))
             ids
             )
        )

      (w dnl)

      (let* ((blk-labels (map m3-map-ident
                              (map cadr
                                   (map get-block-label (cdr the-blocks)))))
             (iiw (indent-writer iw (pad 45 "")))
             )
        (map (lambda(m3lab)
               (iw  (pad 45 "frame." m3lab "_Cl := NEW(") m3lab "Closure," dnl)
               (iiw        "id      := NextId()," dnl)
               (iiw        "frameId := frame.id," dnl)
               (iiw        "frame   := frame," dnl)
               (iiw        "block   := Block_" m3lab ");" dnl dnl)
               )
             blk-labels)
        );;*tel

      (iw "Schedule(frame." (m3-map-ident (cadar the-blocks))"_Cl);" dnl)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (declared-type proc-context expr)

  ;;
  ;; This is similar to derive-type in the front-end;
  ;; the difference is that derive-type actually ignores declared bit
  ;; widths, so that these can be inferred later.
  ;;
  ;; Here, we have already inferred the bit widths, so we need to pull
  ;; them out of our tables.
  ;; 

  
  (let ((symtab       (car proc-context))
        (cell-info    (cadr proc-context))
        (struct-tbl   (caddr proc-context)))
    (if (not (designator? expr))
        (error "declared-type : not a designator : " expr))

    (let* ((declared-id (get-designator-id expr))
           (id-type     (symtab 'retrieve declared-id)))
      (cond ((array-access? expr)
             (peel-array (declared-type proc-context (array-accessee expr))))

            ((member-access? expr)
             (let* ((base-type (declared-type
                                proc-context (member-accessee expr) ))
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

(define (m3-derive-type proc-context expr)
  (let ((symtab       (car proc-context))
        (cell-info    (cadr proc-context))
        (struct-tbl   (caddr proc-context)))
    (derive-type expr (list symtab) '() struct-tbl cell-info)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m3-format-designator proc-context designator)
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

  (cond ((ident? designator) (m3-map-ident (cadr designator)))

        (else (error "not yet"))))

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

(define (m3-bigint-type? t) ;; what's this?
  (and (integer-type? t) (not (caddr t)) (not (bigint? (cadddr t)))))

(define (m3-int-type-width t)
  (BigInt.ToInteger (cadddr t)))

(define (m3-narrow-integer-type? t)
  ;; if a type is "narrow", we can do regular math on it

  ;; hmm there will be a very special case for operands that are
  ;; unsigned and exactly 64 bits wide.
  
  (and (integer-type? t)
       (or (and (m3-sint-type? t) (<= (m3-int-type-width t) 64))
           (and (m3-uint-type? t) (<= (m3-int-type-width t) 63)))) ;; hmm.
  )
       
       
       
(define (m3-compile-narrow-integer-assign w proc-context stmt)

  )

(define (m3-compile-scalar-integer-assign w proc-context stmt)
  (let* ((lhs (get-assign-lhs stmt))
         (lty (declared-type proc-context lhs)))

    (cond ((m3-narrow-integer-type? lty)
           (m3-compile-narrow-integer-assign w proc-context stmt))

          ((m3-wide-integer-type? lty)
           (m3-compile-wide-integer-assign w proc-context stmt))

          (else
           (error)))
  )
)

(define (m3-format-string-expr proc-context x)
  (let ((type (declared-type proc-context x)))
    (cond ((ident? x)  (m3-format-designator proc-context x))
          ((string? x) (stringify x))
          ((+? x)      (string-append
                        "Text.Cat("
                        (m3-format-string-expr proc-context (cadr x))
                        ","
                        (m3-format-string-expr proc-context (cadr x))
                        ")"))

          ((m3-narrow-uint-type? type)
           (string-append
            "Fmt.Unsigned("
            (m3-format-integer-expr proc-context x)
            ", base := 10)"
            ))
            
          ((m3-narrow-sint-type? type)
           (string-append
            "Fmt.Int("
            (m3-format-integer-expr proc-context x)
            ", base := 10)"
            ))

          ((m3-wide-int-type? type)
           (string-append
            "DynamicInteger.Fmt("
            (m3-format-integer-expr proc-context x)
            ", base := 10)"
            ))
           
           
           
      
          )
))

(define (m3-compile-assign w proc-context stmt)
  (dis "m3-compile-assign : " stmt dnl)
  (let* ((lhs (get-assign-lhs stmt))
         (lty (declared-type proc-context lhs))
         (rhs (get-assign-rhs stmt)))

    (dis "m3-compile-assign : lhs : " lhs dnl)
    (dis "m3-compile-assign : lty : " lty dnl)
    (dis "m3-compile-assign : rhs : " (stringify rhs) dnl)

    (cond ((boolean-type? lty)
           (w (m3-format-designator proc-context lhs) " := " (m3-format-boolean-expr rhs)))

          ((string-type? lty)
           (w (m3-format-designator proc-context lhs) " := " (m3-format-string-expr proc-context rhs)))

          ((array-type? lty)
           (error "not yet"))

          ((struct-type? lty)
           (error "not yet"))
          
          ((integer-type? lty)
           (m3-compile-scalar-integer-assign w proc-context stmt))

          (else
           (error "???")))
    
    )
  )

(define (m3-compile-goto w proc-context stmt)
  (dis "m3-compile-goto" dnl)

  (if (not (null? (cddr stmt))) (error "cant compile complex gotos yet"))
  
  (w (string-append "Release(" (m3-map-ident (cadr stmt)) "_Cl);") dnl)
  (w "RETURN TRUE;" dnl)
  )

(define (m3-compile-send w proc-context stmt)
  )

(define (m3-compile-recv w proc-context stmt)
  )

(define (m3-compile-eval w proc-context stmt)
  )


(define *known-stmt-types* '(recv send assign eval goto local-if while))



(define (m3-compile-stmt w proc-context stmt)
  (let (
        (stmt-type (get-stmt-type stmt))
        (iw        (indent-writer w "      "))
        )
    
    (if (member stmt-type *known-stmt-types*)
        ((eval (symbol-append 'm3-compile- stmt-type)) iw proc-context stmt)
        (error "Unknown statement type in : " stmt)
        )          

  
  
    (iw "(* " (stringify stmt) " *)" dnl dnl)
    )
  )

;; proc-context contains three things:
;; car   : a symtab built from the-decls
;; cadr  : the cell-info
;; caddr : the struct-tbl

(define (m3-write-block w proc-context blk)
  (dis (get-block-label blk) dnl)
  (dis "m3-write-block : " blk dnl)
  (let* ((btag (m3-map-ident (cadr (get-block-label blk))))
         (bnam (string-append "Block_" btag))
         (the-code (cddr (filter-out-var1s blk)))
         )
    ;; write decl.  We have to change this later for dynamic parallelism
    (w
     "PROCEDURE "bnam"(cl : Closure) : BOOLEAN =" dnl
     "  BEGIN" dnl
     "    WITH frame = cl.frame DO" dnl)
    
    ;; the block text goes here
    (map (curry m3-compile-stmt w proc-context) the-code)

    (w 
     "    END(*WITH*)" dnl
     "  END " bnam ";" dnl
     dnl)
    )
  )

(define (m3-write-blocks w the-blocks proc-context)
  (map (curry m3-write-block w proc-context)
       (cdr the-blocks))
  )

(define (do-m3!)
  (let* ((the-blocks text9)
         (cell-info  *cellinfo*)
         (port-tbl   *the-prt-tbl*)
         (the-decls  (gen-decls the-blocks *proposed-types*))
         (root       (m3-map-ident (string->symbol *the-proc-type-name*)))
         (i3fn       (string-append "build/" root ".i3"))
         (i3wr       (FileWr.Open i3fn))
         (m3fn       (string-append "build/" root ".m3"))
         (m3wr       (FileWr.Open m3fn))
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

    (m3-write-blocks          modu the-blocks (list
                                               (m3-make-symtab the-decls)
                                               cell-info
                                               *the-struct-tbl*))
    
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

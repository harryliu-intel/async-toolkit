
(define (m3-expand-type type)
  (cond ((boolean-type? type) "BOOLEAN")
        ((string-type? type)  "TEXT")
        ((integer-type? type) (m3-expand-int-type type))
        ((array-type? type)   (m3-expand-array-type type))
        ((struct-type? type)  (m3-expand-struct-type type)) ;; hmm
        (else (error "Unknown type " type))
        )
  )

(define (m3-expand-array-type type)
  ;; this isnt right, this is just an open array
  ;; -- we need the range.
  
  (string-append "ARRAY OF " (m3-expand-type (caddr type))))

(define *target-word-size* 64) ;; word size of target machine
(define *bwsz*    (bn *target-word-size*))
(define *bwszm1*  (xnum-- *bwsz* *big1*))

(define m3-word-min *big0*)
(define m3-word-max (xnum--(xnum-<< *big1* *bwsz*) *big1*))

(define m3-integer-min (xnum-- (xnum-<< *big1* *bwszm1*)))
(define m3-integer-max (xnum-- (xnum-<< *big1* *bwszm1*) *big1*))

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

(define (m3-char c) ;; superseded by M3Ident.Escape
  (cond ((alpha-checker c) (list (char->integer c)))
        ((digit-checker c) (list (char->integer c)))
        (else  `(,*us* 
                  ,@(map char->integer
                         (string->list (number->string (char->integer c))))

                  ,*us*))))

(define m3-ident (compose M3Ident.Escape symbol->string))

(define (oldm3-ident scm-ident) ;; Good grief

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
     ".T := NIL;" dnl
     )
  )

(define (m3-format-port-ass pdef)
  (let ((ident (m3-ident (get-port-id pdef))))
    (string-append ident " := " ident)
    )
  )

(define (bf x) (BigInt.Format x 10))

(define (m3-native-literal expr)
  (if (not (bigint? expr))
      (error "not a constant integer : " expr)
      (BigInt.FormatLiteral expr 16)
      );;fi
  )

(define (m3-map-decltype type)
  ;; returns interface
  (cond ((string-type?  type) "CspString")
        ((boolean-type? type) "CspBoolean")

        ((array-type?   type)
         (m3-map-decltype (array-elemtype type)))
        
        
        ((struct-type?  type)
         '() ;; dont need interfaces for structs
         )
        
        ((integer-type? type)
         (let ((width (cadddr type)))
           (cond ((not (bigint? width)) "DynamicInt")
                 ((caddr type)
                  (string-append "SInt" (bf width)))
                 (else
                  (string-append "UInt" (bf width)))
                 );;dnoc
           );;tel
         )
        (else (error "m3-map-decltype : unknown type to map : " type))
        );;dnoc
  )

(define (m3-mask-native-assign lhs-type rhs)
  (if (not (integer-type? lhs-type)) (error))

  (let* ((signed  (caddr lhs-type))
         (width   (cadddr lhs-type))
         (m3-type (m3-map-decltype lhs-type))
         (sx0    (if signed (sa m3-type "Ops.SignExtend") ""))
         )
         

    (cond ((not (bigint? width)) (error))

          (else (sa sx0
                    "(Word.And("
                    rhs
                    " , "
                    m3-type
                    ".Mask)) "
                    ))
          )
    )
  )

(define (m3-map-declbuild type)
  ;; returns interface to build
  (cond ((string-type?  type) #f)
        ((boolean-type? type) #f)
        ((array-type?   type) #f)
        ((struct-type?  type) #f)
        ((integer-type? type)
         (let ((width (cadddr type)))
           (cond ((not (bigint? width)) #f)
                 ((caddr type)
                  (cons 'SInt (BigInt.ToInteger width)))
                 (else
                  (cons 'UInt (BigInt.ToInteger width)))
                 );;dnoc
           );;tel
         )
        (else (error "m3-map-declbuild : unknown type to map : " type))
        );;dnoc
  )

(define (m3-type type)
  (let ((decltype (m3-map-decltype type)))
    (cond

          ((array-type? type)
           (let ((extent (array-extent type))
                 (elem   (array-elemtype type)))
             (sa "ARRAY [ " (m3-native-literal (cadr extent))
                 " .. "     (m3-native-literal (caddr extent))
                 " ] OF (" (m3-type elem) ")")
             );;tel
           )

          (decltype (sa decltype ".T"))
          
          (else (error))
          );;dnoc
    );;tel
  )
           
(define (m3-convert-vardecl v1)
  (let ((id (get-var1-id v1))
        (ty (get-decl1-type (get-var1-decl1 v1)))
        )
    (string-append (pad 40 (m3-ident id)) " : " (m3-type ty) ";")
    )
  )

(define (m3-frame-variables the-blocks the-decls cell-info)

  ;; the "frame variables" are the variables that need to be 
  ;; declared in the process frame.  They include:
  ;; (1) variables that are shared across blocks
  ;; (2) variables expensive to initialize : dynamic ints
  ;; (3) variables expensive to initialize : arrays

  (let* ((svars (get-shared-variables the-blocks cell-info))
         (dvars
          (map get-var1-id
               (filter
                (compose m3-dynamic-int-type? get-var1-type)
                the-decls)))

         (avars
          (map get-var1-id
               (filter
                (compose array-type? get-var1-type)
                the-decls)))

          )
    (set-union svars dvars avars)
    )
  )

(define (m3-write-shared-locals w the-blocks cell-info the-decls)
  (let* ((shared-local-ids (m3-frame-variables the-blocks the-decls cell-info))
         (v1s              (map (curry find-decl the-decls) shared-local-ids))
         )
    (w "    (* shared locals list *)" dnl)
    (map (lambda(dt) (w "    " dt dnl))
         (map m3-convert-vardecl v1s))

    (w dnl
       "    (* dynamic scratchpad *)" dnl)
    (w "    " (pad 40 "a, b, c") " : DynamicInt.T;" dnl)
    );;*tel
  )

(define (m3-closure-type-text fork-count)

  ;; pass #f or the pair of (L<x> . <cnt>)
  (if fork-count
      (sa "ARRAY [ 0 .. ("(cdr fork-count)" - 1) ] OF Process.Closure" )
      "Closure")
  )

(define (m3-write-process-closure-list w the-blocks fork-counts)
  (w "    (* closures *)" dnl)

  (define (do-one-id lab-id)
    (dis "do-one-id : " lab-id dnl)
    (let* ((btag  (m3-ident lab-id))
           (count (assoc lab-id fork-counts))
           (type  (m3-closure-type-text count))
           )
      
      (w "    " (pad 40 btag "_Cl") " : "type";" dnl)
      );;*tel
    )
      
  (let ((the-ids
         (uniq eq? (map cadr (map get-block-label (cdr the-blocks))))))

    (map do-one-id the-ids)
    )
  )

(define (m3-write-process-fork-counters w fork-counts)
  (w "    (* fork counters *)" dnl)

  (map (lambda(fc)
         (let* ((cvar (m3-ident (symbol-append 'fork-counter- (car fc)))))
           (w "    " (pad 40 cvar) " : [ 0 .. " (cdr fc) " ];" dnl)
           );;*tel
         )
       fork-counts
       );;pam
  )

(define (m3-write-proc-frame-decl w port-tbl the-blocks cell-info the-decls fork-counts)
    
    (w dnl
       "TYPE" dnl)
    (w "  Frame = Process.Frame OBJECT" dnl)
    (m3-write-port-list w cell-info)
    (w dnl)
    (m3-write-shared-locals w the-blocks cell-info the-decls)
    (w dnl)
    (m3-write-process-closure-list w the-blocks fork-counts)
    (w dnl)
    (m3-write-process-fork-counters w fork-counts)
    (w dnl)
    (w "  END;" dnl dnl)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m3-write-block-decl w)
  (w dnl
     "TYPE Block = PROCEDURE (cl : Closure) : BOOLEAN;" dnl
     dnl)
  )

(define (get-all-labels the-blocks)
  (map cadr (filter identity(map (curry find-stmt  'label) the-blocks)))
  )

(define (get-fork-labels label-lst)
  (multi label-lst)
  )

(define (count-occurrences eq? lst of)
  (let loop ((p lst)
             (cnt 0))
    (cond ((null? p) cnt)
          ((eq? of (car p)) (loop (cdr p) (+ cnt 1)))
          (else             (loop (cdr p) cnt))
          )
    )
  )

(define (get-fork-label-counts the-blocks)
  (let* ((labels       (get-all-labels the-blocks))
         (fork-labels  (multi labels)))
    (map (lambda(m) (cons m
                          (count-occurrences eq? labels m)))
         fork-labels)))
  
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
  (w dnl
     "PROCEDURE Build( name : TEXT;" dnl)
  (m3-write-port-list w cell-info)
  (w ")")
  )

(define (m3-write-build-decl w cell-info)
  (m3-write-build-signature w cell-info)
  (w ";" dnl dnl)
  )

(define (indent-writer w by) (lambda x (apply w (cons by x))))

(define (suffix-writer w by) (lambda x (apply w (append x (list by)))))

(define (m3-mark-reader w m3id)
  (w "<*ASSERT " m3id ".reader = NIL*>" dnl)
  (w m3id ".reader := frame;" dnl
     dnl)
  )

(define (m3-mark-writer w m3id)
  (w "<*ASSERT " m3id ".writer = NIL*>" dnl)
  (w m3id ".writer := frame;" dnl
     dnl)
  )

(define (node-port? pdef) (eq? 'node (car (cadddr pdef))))

(define (channel-port? pdef) (eq? 'channel (car (cadddr pdef))))

(define td #f)

(define (m3-format-mpz-new arr-tbl id)
  (let* ((arrdef    (arr-tbl 'retrieve id))
         (arrdims   (if (eq? arrdef '*hash-table-search-failed*)
                        0
                        (array-dims arrdef))))
    
    (sa
     (m3-initialize-array
      (lambda(stuff) (sa stuff  " := Mpz.New()"))
      (sa "frame." (M3Ident.Escape (symbol->string id)))
      arrdims
      'i
      )
     ";" dnl
     )
    
    )
  )

       
(define (m3-write-build-defn w
                             cell-info the-blocks the-decls fork-counts
                             arr-tbl)
  (define *comma-indent* "                     ,")
  (let ((proc-ports (get-ports cell-info)))
    (m3-write-build-signature w cell-info)
    (w " = " dnl)
    (w "  BEGIN" dnl)
    (w "    WITH frame = NEW(Frame," dnl
       "                     name := name," dnl
       "                     id := Process.NextFrameId()" dnl)

    (let ((asslist (map m3-format-port-ass proc-ports)))
      (map (lambda(ass)
             (w *comma-indent* ass  dnl))
           asslist)
      );;tel

    (set! td the-decls)
    
    (let ((iw (indent-writer w *comma-indent*)))
      (iw "a := Mpz.New()" dnl)
      (iw "b := Mpz.New()" dnl)
      (iw "c := Mpz.New()" dnl)
      )
    
  
    (w "      ) DO" dnl
       dnl)

    (let ((dynamics (filter (compose m3-dynamic-int-type? get-var1-type)

                            the-decls))
          )

      ;; write in initialization of Mpz variables
      (dis "dynamics : " dynamics dnl)
      
      (map w
           (map (curry m3-format-mpz-new arr-tbl) (map get-var1-id dynamics)))
      );;tel
    
    
    ;; build body
    (let ((iw (indent-writer w "     ")))


      ;; mark channels as read and written by us
      
      (let* ((inlist (filter channel-port? (filter input-port? proc-ports)))
             (ids    (map m3-ident (map get-port-id inlist)))
             )
        (map (curry m3-mark-reader iw) ids)
        )
      (w dnl)
      
      (let* ((outlist (filter channel-port? (filter output-port? proc-ports)))
             (ids    (map m3-ident (map get-port-id outlist)))
             )
        (map (curry m3-mark-writer iw) ids)
        )
      

      (w dnl)

      (let* ((blk-labels 
              (uniq eq?
                    (map cadr
                         (map get-block-label (cdr the-blocks)))))
             (iiw (indent-writer iw (pad 22 "")))
             )
        (map

         
         (lambda(lab)
           (let ((m3lab (m3-ident lab))
                 (count (assoc lab fork-counts)))

             (if count

                 (begin ;; a fork
                   (iw  (pad 22 "frame." m3lab "_Cl")
                        " := " (m3-closure-type-text count) " { "dnl)
                   (count-execute
                    (cdr count)
                    (lambda(i)
                      (iw (pad 22 "") "   NEW(Closure," dnl)
                      (iiw "       id      := Process.NextId()," dnl)
                      (iiw "       frameId := frame.id," dnl)
                      (iiw "       frame   := frame," dnl)
                      (iiw "       block   := Block_" m3lab "_" i ")"
                           (if (= i (- (cdr count) 1)) "" ",") ;; blah!
                           dnl)
                      
                      )
                    );;etucexe-tnuoc
                   (iw "};" dnl dnl)
                   )
                 
                 (begin  ;; not a fork
                   (iw  (pad 22 "frame." m3lab "_Cl")
                        " := NEW(Closure," dnl)
                   (iiw "        id      := Process.NextId()," dnl)
                   (iiw "        frameId := frame.id," dnl)
                   (iiw "        frame   := frame," dnl)
                   (iiw "        block   := Block_" m3lab ");" dnl dnl)
                   )
                 )
             )
           )

         
         blk-labels)
        );;*tel

      (iw "Scheduler.Schedule(frame." (m3-ident (cadar the-blocks))"_Cl);" dnl)
      );;tel (iw)
    
    (w "    END(*WITH*)" dnl)
    (w "  END Build;" dnl dnl)
    )
  )

(define (m3-write-imports w intfs)
  (dis "m3-write-imports : intfs : " intfs dnl)
  
  (w "<*NOWARN*>IMPORT CspCompiledProcess AS Process;" dnl)
  (w "<*NOWARN*>IMPORT CspCompiledScheduler AS Scheduler;" dnl)
  (w "<*NOWARN*>IMPORT CspString, Fmt;" dnl)
  (w "<*NOWARN*>IMPORT CspBoolean;" dnl)
  (w "<*NOWARN*>IMPORT CspIntrinsics;" dnl)
  (w "<*NOWARN*>IMPORT NativeInt, DynamicInt;" dnl)
  (w "<*NOWARN*>IMPORT Word;" dnl)
  (map (lambda(intf)(w "IMPORT " intf ";" dnl))
       (map format-intf-name intfs))
  )

(define *map-chantypes* ;; these are types generated by generics..
  '((UIntChan UInt Chan)
    (SIntChan SInt Chan)
    (UIntOps UInt Ops)
    (SIntOps SInt Ops))
  )

(define (format-intf-name intf-pair)
  (let* ((n  (car intf-pair))
         (w  (cdr intf-pair))
         (ws (number->string w))
         (m  (assoc n *map-chantypes*))
         )
    (if (eq? 'Node n)
        (sa "NodeUInt" (number->string w) " AS Node" ws)
        (if m
            (sa (format-intf-name (cons (cadr m) w))
                (symbol->string (caddr m)))
            (sa (symbol->string n) ws)
            )
        )
    )
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

(define (m3-convert-port-type-build ptype)
  (let ((stype (port-type-short ptype)))
    (case (car stype)
      ((node) (cons 'Node (BigInt.ToInteger (cadr stype))))
      ((bd)   (cons 'UInt (BigInt.ToInteger (cadr stype))))
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
  (if (> (m3-convert-port-ass-bits ptype) *target-word-size*) 'wide 'native))

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
  
  (let ((symtab       (pc-symtab     pc))
        (cell-info    (pc-cell-info  pc))
        (struct-tbl   (pc-struct-tbl pc)))
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
  (let ((symtab       (pc-symtab     pc))
        (cell-info    (pc-cell-info  pc))
        (struct-tbl   (pc-struct-tbl pc)))
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

  (dis "m3-format-designator : " designator dnl)

  (cond ((ident? designator) (m3-format-varid pc (cadr designator)))

        ((array-access? designator)
         (sa (m3-format-designator pc (array-accessee designator))
             "[" (m3-compile-native-value pc (array-accessor designator)) "]"
             )
         )

        ;; need to add struct here
        
        (else (error "m3-format-designator : not yet"))))

(define (m3-format-varid pc id)
  ;; given an id (as part of a designator) in Scheme,
  ;; generate a correctly formatted reference for it for the
  ;; compiled program, from within a block procedure

  (let ((the-scopes (pc-scopes pc)))
    (case (the-scopes 'retrieve id)
      ((*hash-table-search-failed*) (error "unknown id : " id))
      
      ((port)           (string-append "frame." (m3-ident id)))
      
      ((process)        (string-append "frame." (m3-ident id)))
      
      ((block)          (m3-ident id))
      
      ((parallel-dummy) ;; this is sketchy
       (string-append "cl." (m3-ident id)))
      
      (else (error))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Wider integers than 64 bits are stored as dynamic integers, see below.
;; Assignments are range-checked and truncated as needed.
;;
;; wide uint (65..??)
;; wide sint (65..??)
;;
;; Finally, there are dynamic integers, which are stored as instances of
;; Mpz.T (GNU MP mpz_t C type wrapper).
;;
;; dynamic int (0 .. +inf)
;;

(define (m3-sint-type? t)
  (and (integer-type? t) (caddr t)))

(define (m3-uint-type? t)
  (and (integer-type? t) (not (caddr t)) (bigint? (cadddr t))))

(define (m3-dynamic-int-type? t)

  ;; this is really not quite right.  Anything wider than 63 bits
  ;; s.b. dynamic. (for the purposes this procedure is used.. need to
  ;; check on that)

  ;; also.. do the uses want the "stripped" type or the array type?
  
  (or
   (and (integer-type? t)
        (not (caddr t))  ;; would be sint 
        (not (bigint? (cadddr t))))
   (and (array-type? t)
        (m3-dynamic-int-type? (array-elemtype t)))
   );;ro
  )

(define (m3-int-type-width t)
  (if (m3-dynamic-int-type? t) (error) (BigInt.ToInteger (cadddr t))))

(define (m3-natively-representable-type? t)
  ;; if a type is "narrow", we can do regular math on it

  ;; this means that the bit pattern matches the bit pattern of
  ;; a Modula-3 INTEGER, and that the operations do too

  ;; hmm there will be a very special case for operands that are
  ;; unsigned and exactly 64 bits wide.  Actually, we can represent
  ;; those as fixed wide integers, only exactly one word wide.
  
  (and (integer-type? t)
       (or (and (m3-sint-type? t) (<= (m3-int-type-width t) *target-word-size*))
           (and (m3-uint-type? t) (<= (m3-int-type-width t) (- *target-word-size* 1))))) ;; hmm.
  )

(define (m3-wide-int-type? t)
  (and (not (m3-dynamic-int-type?            t))
       (not (m3-natively-representable-type? t))
       );;dna
  )

(define (m3-compile-scalar-int-assign pc stmt)
  (let* ((lhs (get-assign-lhs stmt))
         (lty (declared-type pc lhs)))

    (cond ((m3-natively-representable-type? lty)
           (m3-compile-native-int-assign  pc stmt))

          ((m3-dynamic-int-type? lty)
           (m3-compile-dynamic-int-assign pc stmt))

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
          ((m3-dynamic-int-type? ty)            'dynamic)
          ((integer-type? ty)                   'wide)
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

(define (m3-force-type pc cat x)
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

(define m3-unary-ops '(-))

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
  ;; this approach may only work for tgt = 'native
  (let* ((op-type (max-type tgt (classify-type pc a) (classify-type pc b)))
         (opx     (m3-compile-binop op-type
                                    op
                                    (m3-force-type pc op-type a)
                                    (m3-force-type pc op-type b))))
    (m3-compile-convert-type op-type tgt opx))
  )

(define (m3-compile-unop cat op a-arg)
  (cond ((and (eq? 'native cat) (member op m3-unary-ops))
         (sa "( " (m3-map-symbol-op op) " " a-arg " )"))
        (else (sa (get-m3-int-intf cat) "." (m3-map-named-op op) "( "
                   a-arg " )"))
        )
  )

(define (m3-compile-typed-unop pc tgt op a)
  ;; this approach may only work for tgt = 'native
  (let* ((op-type (max-type tgt (classify-type pc a)))
         (opx     (m3-compile-unop op-type
                                   op
                                   (m3-force-type pc op-type a))))
    (m3-compile-convert-type op-type tgt opx))
  )

(define (m3-compile-native-int-assign pc x)
  (dis "m3-compile-native-int-assign : x : " x dnl)
  ;; assign when lhs is native
  (let* ((lhs (get-assign-lhs x))
         (rhs (get-assign-rhs x))
         (comp-lhs 
          (sa (m3-format-designator pc lhs) " := "))

         (comp-rhs
          (cond ((ident? rhs)
                 (m3-force-type pc 'native rhs))
        
                ((bigint? rhs)
                 (BigInt.Format rhs 10))
                
                ((binary-expr? rhs)
                 (m3-compile-typed-binop pc 'native
                                         (car rhs)
                                         (cadr rhs)
                                         (caddr rhs)))
                
                ((unary-expr? rhs)
                 (m3-compile-typed-unop pc 'native
                                        (car rhs)
                                        (cadr rhs)))
                
                ((bits? rhs)
                 (m3-compile-native-bits pc rhs))
                
                ((recv-expression? rhs) 'recv) ;; this is an exception
                
                (else (error "m3-compile-native-int-assign"))
                );;dnoc
          )

         (ass-rng  (assignment-range (make-ass x) (pc-port-tbl pc)))

         (des      (get-designator-id lhs))
         (tgt-type ((pc-symtab pc) 'retrieve des))
         (tgt-rng  (get-type-range tgt-type))
                  
         (in-range (range-contains? tgt-rng ass-rng))
         )

    (dis "m3-compile-native-int-assign : x        : " x dnl)
    (dis "m3-compile-native-int-assign : ass-rng  : " ass-rng dnl)
    (dis "m3-compile-native-int-assign : in-range : " in-range dnl)
    

    (if (eq? comp-rhs 'recv)
        (m3-compile-recv pc `(recv ,(cadr rhs) ,lhs))
        (if in-range
            (sa comp-lhs comp-rhs)
            (sa comp-lhs (m3-mask-native-assign tgt-type comp-rhs))
            )
        )
    );;*tel
  )

(define (make-ass ass-stmt)
  (let* ((tgt     (get-designator-id (get-assign-lhs ass-stmt)))
         (an-ass  (car (*the-ass-tbl* 'retrieve tgt))))
    `(,ass-stmt ,(cadr an-ass) ,(caddr an-ass) ,(cadddr an-ass))))

(define (make-dynamic-constant! pc bigint)
  (let ((nam (M3Ident.Escape (sa "constant" (stringify bigint)))))
    ((pc-constants pc) 'update-entry! nam bigint)
    nam
    )
  )

(define (m3-mpz-op op)
  (case op
    ;; binary ops:
    ((+)    "Mpz.add")
    ((-)    "Mpz.sub")
    ((*)    "Mpz.mul")
    ((/)    "Mpz.tdiv_q")
    ((%)    "Mpz.tdiv_r")
    ((&)    "Mpz.and")
    ((|)    "Mpz.ior") ;|))
    ((^)    "Mpz.xor")
    ((**)   "Mpz.pow")

    ;; unary ops: 
    ((uneg) "Mpz.neg")
    ((~)    "Mpz.com")
    (else (error))
    )
  )

(define (test-mpz op a b)
  (let ((ma (Mpz.New))
        (mb (Mpz.New))
        (mc (Mpz.New))
        (mpz-op (eval (string->symbol (m3-mpz-op op)))))
    
    (Mpz.init_set_si ma a)
    (Mpz.init_set_si mb b)

    (mpz-op mc ma mb)
    (Mpz.Format mc 'Decimal)
    )
  )

(define (dynamic-type-expr? pc expr)
  (eq? 'dynamic (classify-type pc expr)))

(define (native-type-expr? pc expr)
  (eq? 'native (classify-type pc expr)))

(define (m3-set-dynamic-value pc m3id expr)
  (cond ((bigint? expr)
         (sa "Mpz.set(" m3id ", " (make-dynamic-constant! pc expr) ")"))

        ((and (ident? expr) (dynamic-type-expr? pc expr)) #f)

        ((and (ident? expr) (native-type-expr? pc expr))
         (sa "Mpz.set_si(" m3id ", " (m3-ident (cadr expr)) ")")
         )

        (else (error "can't set dynamic value : " m3id " <- " expr))
        )
  )

(define (m3-compile-dynamic-binop lhs pc mpz-op a b)
  (let* ((a-stmt (m3-set-dynamic-value pc "frame.a" a))
         (b-stmt (m3-set-dynamic-value pc "frame.b" b))
         (res
          (sa (if a-stmt (sa a-stmt "; ") "")
              (if b-stmt (sa b-stmt "; ") "")
              mpz-op
              "("
              lhs
              " , "
              (if a-stmt "frame.a" (m3-format-designator pc a))
              " , "
              (if b-stmt "frame.b" (m3-format-designator pc b))
              ")"
              ))
         )

    (dis "m3-compile-dynamic-binop : " res dnl)
;;    (error)
    
    res
    )      
  )

(define (m3-compile-dynamic-unop lhs pc mpz-op a)
  (let* ((a-stmt (m3-set-dynamic-value pc "frame.a" a))
         (res
          (sa (if a-stmt (sa a-stmt "; ") "")
              mpz-op
              "("
              lhs
              " , "
              (if a-stmt "frame.a" (m3-format-designator pc a))
              ")"
              ))
         )

    (dis "m3-compile-dynamic-unop : " res dnl)
;;    (error)
    
    res
    )      
  )

(define (m3-compile-dynamic-int-assign pc x)
  ;;
  ;; this approach should work for arrays and structs, but will
  ;; it work for "bits"?
  ;;
  
  (dis "m3-compile-dynamic-int-assign : x : " x dnl)

  (let* ((lhs      (get-assign-lhs x))
         (rhs      (get-assign-rhs x))
         (comp-lhs (m3-format-designator pc lhs))
         (ass-rng  (assignment-range (make-ass x) (pc-port-tbl pc)))

         (des      (get-designator-id lhs))
         (tgt-type ((pc-symtab pc) 'retrieve des))
         (tgt-rng  (get-type-range tgt-type))
                  
         (in-range (range-contains? tgt-rng ass-rng))

         ;; here we can add a call to push the result in range if it is wide
         ;; but not fully dynamic
         )

    (cond
     ((bigint? rhs)
      (sa "Mpz.set(" comp-lhs ", " (make-dynamic-constant! pc rhs) ")")
      )
               
     ((or (ident? rhs) (array-access? rhs))
      (sa "Mpz.set(" comp-lhs ", " (m3-force-type pc 'dynamic rhs) ")")
      )

     ((binary-expr? rhs)
      (m3-compile-dynamic-binop
       comp-lhs pc (m3-mpz-op (car rhs)) (cadr rhs) (caddr rhs))
      )

     ((unary-expr? rhs)
      (let* ((op     (car rhs))
             (map-op (if (eq? '- op) 'uneg op))
             (m3-op  (m3-mpz-op map-op))
             )
        (m3-compile-dynamic-unop comp-lhs pc m3-op (cadr rhs))
        );;*tel
      )
     
     (else (error "m3-compile-dynamic-int-assign : dunno RHS object : " rhs)))

    )
  
 )

(define (m3-initialize-array format-init name dims dummy)
  ;; name is the text name in m3 format
  ;; dims is the number of dimensions
  ;; dummy is the prefix of the dummy
  ;; format-init takes one parameter, the name of the object to initialize

  ;; code currently BROKEN
  
  (let loop ((i 0)
             (what name)
             (index "")
             (indent "")
             (prefix "")
             (suffix "")
             )
    (cond ((= i dims) (sa prefix indent (format-init (sa name index)) suffix))

          (else
           (let ((frst (sa "FIRST(" what ")")))
             (loop
              (+ i 1)
              (sa what "[" frst "]")
              (sa index "[" (symbol->string dummy) i "]")
              (sa indent "  ")
              (sa prefix 
                  indent "FOR "(symbol->string dummy) i
                  " :=  FIRST(" what ") TO LAST(" what ") DO" dnl)
              (sa dnl indent "END"
                  suffix))
             );;tel
           );;esle
          );;dnoc
    );;tel
  )

(define (m3-compile-native-recv-expression pc rhs)
  )

(define (m3-compile-stringify-integer-value pc x)
  (if (bigint? x) ;; are the quotes right here?
      (sa "\"" (BigInt.Format x 10) "\"") 
  
      (let ((type (declared-type pc x)))
        (cond
         
         ((m3-natively-representable-type? type)
          (string-append
           "NativeInt.Format("
           (m3-compile-integer-value pc x)
           ", base := 10)"
           ))
         
         ((m3-dynamic-int-type? type)
          (string-append
           "DynamicInt.Format("
           (m3-compile-integer-value pc x)
           ", base := Mpz.FormatBase.Decimal)"
           ))
         
         (else
          (string-append
           "WideInt.Format("
           (m3-compile-integer-value pc x)
           ", base := 10)"
           ))
         )
        )
      )
  )

(define (m3-compile-integer-value pc x)
  (define (err) (error "m3-compile-integer-value : can't map to integer : " x))
  
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; strings
;;

(define (m3-compile-string-expr pc x)
  (define (err) (error "m3-compile-string-expr : can't map to string : " x))
  
  (cond ((ident? x)  
         (let ((type (declared-type pc x)))
           (cond
            ((integer-type?  type)
             (m3-compile-stringify-integer-value pc x))

            ((string-type? type)
             (m3-format-varid pc (cadr x)))

            ((boolean-type? type)
             (sa "NativeInt.Format(CspBoolean.ToInteger("
                 (m3-format-varid pc (cadr x))
                 ") )"
                 )
             )

            (else (err))
            )
           )
         )

        ((string? x) (stringify x))
        
        ((+? x)      (string-append
                      "CspString.Cat( "
                      (m3-compile-string-expr pc (cadr x))
                      " , "
                      (m3-compile-string-expr pc (caddr x))
                      " )"))

        ((bigint? x)
;;         (error) ;; shouldnt happen...
         (sa "\"" (BigInt.Format x 10) "\""))
        
        (else (err))
        );;dnoc
  )

(define (boolean-value? pc x)
  (cond ((boolean? x) #t)
        ((literal? x) #f)
        ((boolean-type? (declared-type pc x)))
        (else #f)))

(define (string-value? pc x)
  (cond ((string? x) #t)
        ((literal? x) #f)
        ((string-type? (declared-type pc x)))
        (else #f)))

(define (integer-value? pc x)
  (cond ((bigint? x) #t)
        ((literal? x) #f)
        ((integer-type? (declared-type pc x)))
        (else #f)))

(define (m3-compile-value pc int-class x)
  (cond ((string? x) (stringify x))

        ((and (bigint? x) (eq? int-class 'native))
         (BigInt.FormatLiteral x 16))

        ((and (bigint? x) (eq? int-class 'dynamic))
         (make-dynamic-constant! pc x))

        ((and (boolean? x) x)        "TRUE")

        ((and (boolean? x) (not x)) "FALSE")

        ((designator? x) (m3-format-designator pc x))

        (else (error "cannot compile value : " x))
        )
  )

(define (m3-compile-native-value pc x)
  ;; compile "x", an integer value, to something with native type
  (cond ((bigint? x)
         (BigInt.FormatLiteral x 16))

        ((native-integer-value? pc x)
         (m3-format-designator pc x))

        ((dynamic-integer-value? pc x)
         (sa "Mpz.ToInteger(" (m3-format-designator pc x) ")" ))

        (else (error ("m3-compile-native-value : " x dnl)))
        );;dnoc
  )

(define (native-integer-value? pc x)
  (and (integer-value? pc x)
       (eq? 'native (classify-type pc x))))

(define (dynamic-integer-value? pc x)
  (and (integer-value? pc x)
       (not (native-integer-value? pc x))))

(define (m3-compile-equals pc m3-lhs a b)
  ;; this is special, as operands can be boolean or integer

  (let* ((do-int (integer-value? pc a)))
    (cond (do-int ;; integer comparison
           (m3-compile-comparison pc m3-lhs "=" a b)
           
           );;tni-od
          
          (else   ;; not integer comparison, so boolean comparison

           (sa m3-lhs
               "( " (m3-compile-value pc 'x a)
               " = " (m3-compile-value pc 'x b)  " )" )
           );;esle
          );;dnoc
    );;*tel
  )

(define (m3-compile-comparison pc m3-lhs m3-cmp a b)
  ;; where cmp is one of '> '< '= '>= '<=

  (cond ((forall? (curry native-integer-value? pc) (list a b))
         ;; native int comparison
         (sa m3-lhs
             "( "  (m3-compile-value pc 'native a)
             " " m3-cmp " "
             (m3-compile-value pc 'native b) " )")
         )
        
        (else ;; do it dynamic
         
         (let* ((a-stmt (m3-set-dynamic-value pc "frame.a" a))
                
                (b-stmt (m3-set-dynamic-value pc "frame.b" b))
                
                (res
                 (sa (if a-stmt (sa a-stmt "; ") "")
                     (if b-stmt (sa b-stmt "; ") "")
                     m3-lhs
                     "(Mpz.cmp("
                     (if a-stmt "frame.a" (m3-format-designator pc a))
                     " , "
                     (if b-stmt "frame.b" (m3-format-designator pc b))
                     ") " m3-cmp "  0)"
                     ));;ser
                )
           res);;*tel
         );;esle
        );; dnoc
  )

(define (m3-compile-boolean-numeric-binop pc m3-lhs op a b)
  (let ((m3-cmp (caddr (assoc op *boolean-numeric-binops*))))
    (m3-compile-comparison pc m3-lhs m3-cmp a b)
    )
  )

(define (m3-compile-boolean-logical-binop pc m3-lhs op a b)
  (let ((av    (m3-compile-value pc 'x a))
        (bv    (m3-compile-value pc 'x a))
        (m3-op (caddr (assoc op *boolean-logical-binops*))))
    (sa m3-lhs "( (" av ") " m3-op " (" bv ") )")
    );;tel
  )

(define *boolean-binop-map*
  '(
    (!= numeric "#" )
    (<  numeric "<" )
    (>  numeric ">" )
    (>= numeric ">=")
    (<= numeric "<=")
    (&  logical "AND")
    (|  logical "OR") ;;|)
    (&& logical "AND")
    (|| logical "OR") ;;|)
    (^  logical "#")
    )
  )
  
(define *boolean-numeric-binops*
  (filter (compose (curry eq? 'numeric) cadr) *boolean-binop-map*)
  )

(define *boolean-logical-binops*
  (filter (compose (curry eq? 'logical) cadr) *boolean-binop-map*)
  )

;; note that CSP (oddly) does not allow != between booleans.

(define (m3-compile-boolean-assign pc lhs x)
  (let ((m3-lhs (sa (m3-format-designator pc lhs) " := ")))
    (cond ((boolean? x)
           (sa m3-lhs (if x "TRUE" "FALSE")))
          
          ((ident? x) ;; should cover arrays and structs, too
           (sa m3-lhs (m3-format-designator pc x)))
          
          ((and (pair? x) (eq? (car x) 'not))
           (sa m3-lhs "(NOT ( " (m3-compile-value pc rhs) " ) )"))
          
          ((and (pair? x) (eq? (car x) '==))
           (m3-compile-equals pc m3-lhs (cadr x) (caddr x)))
          
          ((and (pair? x)
                (= 3 (length x))
                (member (car x) (map car *boolean-numeric-binops*)))
           (m3-compile-boolean-numeric-binop pc m3-lhs (car x) (cadr x) (caddr x)))
          
          ((and (pair? x)
                (= 3 (length x))
                (member (car x) (map car *boolean-logical-binops*)))
           (m3-compile-boolean-logical-binop pc m3-lhs (car x) (cadr x) (caddr x)))

          (else (error "m3-compile-boolean-assign : don't understand " x))
        );;dnoc
    );;tel
  )

(define (m3-compile-intrinsic-assign pc lhs lty rhs)
  (if (integer-type? lty)
      (begin
        ;; if LHS is integer, we need to ensure we can type-convert return
        ;; value

        (sa "WITH retval = " (m3-compile-intrinsic pc rhs) " DO" dnl

            (cond ((m3-dynamic-int-type? lty)
                   (sa
                    "  Mpz.set_ui(" (m3-format-designator pc lhs) " , retval)")
                   )

                  ((m3-native-int-type? lty)

                   (sa "  " (m3-format-designator pc lhs) " := retval")
                   )

                  (else (error "m3-compile-intrinsic-assign : don't know type " lty))
                  )
            dnl
            "END" dnl)
        )
      ;; not an integer, a simple assignment will do
      (sa (m3-format-designator pc lhs) " := "
          (m3-compile-intrinsic pc rhs))
      )
  )

(define (m3-compile-assign pc stmt)
  (dis "m3-compile-assign : " (stringify stmt) dnl)
  (let* ((lhs (get-assign-lhs stmt))
         (lty (declared-type pc lhs))
         (rhs (get-assign-rhs stmt)))

    (dis "m3-compile-assign : lhs : " lhs dnl)
    (dis "m3-compile-assign : lty : " lty dnl)
    (dis "m3-compile-assign : rhs : " (stringify rhs) dnl)

    (cond ((call-intrinsic? rhs)
           (m3-compile-intrinsic-assign pc lhs lty rhs))
          
          ((boolean-type? lty)
           (m3-compile-boolean-assign pc lhs rhs))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; goto -- not complete yet
;; 

(define (m3-compile-goto pc stmt)
  (dis "m3-compile-goto" dnl)

  (let* ((is-fork (and (not (null? (cddr stmt)))
                       (eq? 'fork (caddr stmt))))
         (is-join (and (not (null? (cddr stmt)))
                       (eq? 'join (caddr stmt))))
         )

    (cond (is-fork (m3-compile-fork-goto  pc stmt))
          (is-join (m3-compile-join-goto  pc stmt))
          (else    (m3-compile-plain-goto pc stmt))
          )
    )
  )

(define  (m3-compile-plain-goto pc stmt)
    (string-append "Scheduler.Release (cl.frame." (m3-ident (cadr stmt)) "_Cl);"
                   " RETURN TRUE")
    )

(define  (m3-compile-fork-goto pc stmt)
  (let* ((fork-counter (sa "frame."
                           (m3-ident (symbol-append 'fork-counter-
                                                    (cadr stmt)))))
         (counter-lhs  (sa "<*ASSERT " fork-counter " = 0*> "
                           fork-counter " := "))
         )
    (string-append "BEGIN "  counter-lhs

                   "Scheduler.ReleaseFork(frame."
                   (m3-ident (cadr stmt)) "_Cl);"
                   " RETURN TRUE END")
    )
  )

(define  (m3-compile-join-goto pc stmt)
  (let* ((fork-counter (sa "frame."
                           (m3-ident (symbol-append 'fork-counter-
                                                    (cadddr stmt)))))
         (counter-lhs  (sa "<*ASSERT " fork-counter " # 0*> "
                           "DEC(" fork-counter ")"))
         )
    (string-append "IF " fork-counter " = 0 THEN "
                   
                   "Scheduler.Release(cl.frame."
                   (m3-ident (cadr stmt)) "_Cl) END;"
                   " RETURN TRUE")
    )
  )

(define (get-port-def-type pdef) (cadddr pdef))

(define (m3-compile-send pc stmt)
  (let* ((port-tbl     (pc-port-tbl pc))

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

    (sa "VAR toSend := " ;; this isnt right, need to be more careful!
        port-typenam ".Item  { " (m3-force-type pc port-class rhs)
        "} ; BEGIN IF NOT "
        port-typenam ".Send( " m3-pname "^ , toSend , cl ) THEN RETURN FALSE END END"
        )
    )
  )

(define (m3-compile-recv pc stmt)
  (dis "m3-compile-recv : " stmt dnl)
  (let* (
         (port-des     (get-recv-lhs stmt)) ;; doesnt work for arrays/structs
         (rhs          (get-recv-rhs stmt))

         
         (port-tbl     (pc-port-tbl pc))
         (port-id      (get-designator-id port-des))
         (port-def     (port-tbl 'retrieve port-id))
         (port-type    (get-port-def-type port-def))
         (port-typenam (m3-convert-port-type port-type))
         (copy-type    (m3-convert-port-ass-type port-type))
         
         (m3-pname     (m3-format-varid pc port-id))
         (port-class   (m3-convert-port-ass-category port-type))
         )
    (define (null-rhs)
      (sa "VAR toRecv : "
          port-typenam".Item; BEGIN IF NOT "
          port-typenam ".Recv( " m3-pname "^ , toRecv , cl ) THEN RETURN FALSE END END")
      )

    (define (nonnull-rhs)
      (let ((rhs-class    (classify-type pc rhs)))
        (sa "VAR toRecv : "
            port-typenam".Item; BEGIN IF NOT "
            port-typenam ".Recv( " m3-pname "^ , toRecv , cl ) THEN RETURN FALSE END;"
            (m3-format-designator pc rhs) " := "
            (m3-compile-convert-type
             port-class
             rhs-class
             "toRecv[0]"
             )
            " END"
            )
        )
      )

    (if (null? rhs) (null-rhs) (nonnull-rhs))
    )
  )

(define (m3-compile-eval pc stmt)
  (dis "m3-compile-eval : " stmt dnl)
  (let ((expr (cadr stmt)))
    (string-append "EVAL " (m3-compile-intrinsic pc expr))
    )
  )

(define (m3-compile-print-value pc x)
  ;; the rules for printing a value are different from
  ;; the rules for adding a value to a string
  (cond ((string? x) (stringify x))
        
        ((bigint? x)
         (sa "\"0x" (CitTextUtils.ToLower (BigInt.Format x 16)) "\""))
        
        ((and (boolean? x) x)       (m3-compile-print-value pc *bigm1*))
        
        ((and (boolean? x) (not x)) (m3-compile-print-value pc *big0*))

        ;; if we get here, x is not a literal
        ;;
        ;; we can print variables of the following CSP types:
        ;; bool, string, int

        ((boolean-value? pc x)
         (sa "\"0x\" & NativeInt.Format(CspBoolean.ToInteger(" (m3-format-designator pc x) "), 16)")
         )

        ((string-value? pc x)
         (m3-format-designator pc x)
         )
        
        ;; if we get here, x must be a number, and it is either native or
        ;; dynamic
        

        ((native-integer-value? pc x)
         (sa "\"0x\" & NativeInt.Format(" (m3-format-designator pc x) ", 16)")
         )
        
        ((dynamic-integer-value? pc x)
         (sa "\"0x\" & DynamicInt.FormatHexadecimal(" (m3-format-designator pc x) ")")
         )
        
        (else (error "cannot compile print-value : " x))
        )
  )

(define (m3-compile-intrinsic pc expr)
  (dis "m3-compile-intrinsic : " expr dnl)
  (if (not (call-intrinsic? expr)) (error "not an intrinsic : " expr))

  (let ((in-sym (cadr expr)))

    (case in-sym
      ((print)
       (sa "CspIntrinsics.print(frame, "
           (m3-compile-print-value pc (caddr expr))
           ")")

       )

      ((walltime simtime)
       (sa  "CspIntrinsics." (symbol->string in-sym) "(frame)"))

      ((string)
       ;; first convert base to native
       (let* ((val         (caddr expr))
              (base        (cadddr expr))
              (native-base (m3-compile-value pc 'native base)))
         (cond ((native-integer-value? pc val)
                (sa "CspIntrinsics.string_native(frame, "
                    (m3-compile-value pc 'native val) " , "
                    native-base
                    ")" )
                )

               ((dynamic-integer-value? pc val)
                (sa "CspIntrinsics.string_dynamic(frame, "
                    (m3-compile-value pc 'dynamic val) " , "
                    native-base
                    ")" )
                )

               (else (error)))
         );;*tel
       )

      (else (error "unknown intrinsic : " expr))
      
      );;esac
    
    );;tel
  )

(define (m3-compile-sequence pc stmt)
  (define wx (Wx.New))

  (define (w . x) (Wx.PutText wx (apply string-append x)))

  (let ((writer  (suffix-writer (indent-writer w "  ") "")))
    (w "BEGIN" dnl)

    (map (curry m3-compile-write-stmt writer pc) (cdr stmt))
    (w "END" dnl)
    )

  (Wx.ToText wx)
  )

(define (m3-compile-skip pc skip) "BEGIN (*skip*) END"  )

(define (m3-compile-sequential-loop pc seqloop)

  (let* ((dummy (get-loop-dummy seqloop))
         (range (get-loop-range seqloop))
         (stmt  (get-loop-stmt  seqloop))
         (desig (m3-format-designator pc `(id, dummy)))
         (native (native-type-expr? pc `(id ,dummy)))
         )

    (define (compile-stmt)
      (define wx (Wx.New))
      
      (define (w . x) (Wx.PutText wx (apply string-append x)))
      
      (m3-compile-write-stmt w pc stmt)
      
      (Wx.ToText wx)
      )
    
    (if native
        (sa ;; native integer FOR loop
         "FOR " desig
         " := " (m3-compile-value pc 'native (cadr range))
         " TO " (m3-compile-value pc 'native (caddr range))
         " DO" dnl
         (compile-stmt) dnl
         "END" dnl
         )

        (sa ;; dynamic integer FOR loop
         "BEGIN(*sequential-loop*)  Mpz.set("
         desig
         "," (m3-compile-value pc 'dynamic (cadr range)) "); "
         " WHILE Mpz.cmp( " desig " , "
         (m3-compile-value pc 'dynamic (caddr range)) ") # 1 DO " dnl
         (compile-stmt) dnl
         "Mpz.add_ui(" desig " , " desig " , 1)" dnl
         "END END(*sequential-loop*)" dnl
         )

        )
    )
  )

  
(define (m3-compile-local-if pc stmt)

  (dis  dnl
        "m3-compile-local-if : " stmt dnl
        dnl)

  (define wx (Wx.New))

  (define (w . x) (Wx.PutText wx (apply string-append x)))
  
  (w "IF FALSE THEN" dnl)
  (let loop ((p (cdr stmt)))

    (define (command)
      (m3-compile-write-stmt (indent-writer w "  ") pc (cadar p)))

    (cond ((null? p) (w "END") (Wx.ToText wx)
           )
          
          ((eq? (caar p) 'else)
           (w "ELSE" dnl)
           (command)
           (loop (cdr p))
           )
          
          (else          
           (w "ELSIF " (m3-compile-value pc 'x (caar p)) " THEN" dnl)
           (command)
           (loop (cdr p))
           )
          );;dnoc
        );;tel
    )

(define (convert-sequential-loop-to-while seqloop)
  ;; unused for now (we can't call it from code generation---that's too late
  (let* ((dummy (get-loop-dummy seqloop))
         (range (get-loop-range seqloop))
         (stmt  (get-loop-stmt  seqloop)))
    `(sequence
       (assign (id ,dummy) ,(cadr range))
       (while (<= (id ,dummy) ,(caddr range))
              (sequence
                ,stmt
                (assign (id ,dummy) (+ ,*big1* (id ,dummy)))
                )
              )
       )
    )
  )

(define *known-stmt-types*
  '(recv send assign eval goto local-if while sequence sequential-loop skip)
  )

(define (m3-space-comments txt)
  (CitTextUtils.Replace
   (CitTextUtils.Replace txt "(*" "( *")
   "*)" "* )"))

(define (m3-compile-write-stmt w pc stmt)
  (let (
        (stmt-type (get-stmt-type stmt))
        (iw        (indent-writer w "      "))
        )

    (dis "m3-compile-write-stmt : stmt : " (stringify stmt) dnl)
    
    (if (member stmt-type *known-stmt-types*)
        (iw ((eval (symbol-append 'm3-compile- stmt-type)) pc stmt) ";(*m3cws*)" dnl)
        (error "Unknown statement type in : " stmt)
        )          
  
    (iw "(* " (m3-space-comments (stringify stmt)) " *)" dnl dnl)
    )
  )

(define (number->symbol x) (string->symbol (number->string x)))

(define (distinguish-labels label-lst)
  ;; here label-lst is a list of strings (not symbols)
  (let ((mls (multi label-lst)))
    (if (null? mls)
        label-lst
        (let loop ((p label-lst)
                   (res '())
                   (i 0))
          (cond ((null? p) (distinguish-labels (reverse res)))
                ((equal? (car mls) (car p))
                 (loop (cdr p)
                       (cons (sa (car p) "_" (number->string i))
                             res)
                       (+ i 1)))
                (else (loop (cdr p)
                            (cons (car p) res)
                            i))))
        );; fi
    )
  )

(define (m3-write-block w pc blk m3-label)

  ;; use the write procedure w
  ;; the process context pc
  ;; write the block blk
  ;; which is labelled m3-label, in Modula-3 code (a string)
  
  (dis (get-block-label blk) dnl)
  (dis "m3-write-block : blk : " (stringify blk) dnl)
  (dis "m3-write-block : lab : " (stringify m3-label) dnl)
  (let* ((btag        m3-label)
         (bnam        (string-append "Block_" btag))
         (the-code    (cddr (filter-out-var1s blk)))

         (symtab      (pc-symtab pc))
         (the-scopes  (pc-scopes pc))
         (refvar-ids  (find-referenced-vars blk))
         (the-locals  (filter
                       (lambda(id)(eq? 'block (the-scopes 'retrieve id)))
                       refvar-ids))

         (v1s         (map make-var1-decl
                           the-locals
                           (map (curry symtab 'retrieve) the-locals)))
         )
    ;; write decl.  We have to change this later for dynamic parallelism
    (w
     "PROCEDURE "bnam"(cl : Closure) : BOOLEAN =" dnl
     "  VAR" dnl)

    (map (lambda(dt) (w "    " dt dnl)) (map m3-convert-vardecl v1s))

    (w
     "  BEGIN" dnl
     "    WITH frame = cl.frame DO" dnl)
    
    ;; the block text goes here
    (map (curry m3-compile-write-stmt w pc) the-code)

    (w 
     "    END(*WITH*);" dnl
     "    RETURN TRUE;" dnl ;; in case it falls off the end
     "  END " bnam ";(*m3wb*)" dnl
     dnl)
    )
  )

(define (m3-write-blocks w the-blocks pc)
  (let* ((wr-blks      (cdr the-blocks)) ;; skip the entry point
         (labs         (map cadr (map get-block-label wr-blks)))
         (m3-labs      (map m3-ident labs))
         (dist-m3-labs (distinguish-labels m3-labs))
         )
  (map (curry m3-write-block w pc) wr-blks dist-m3-labs)
  )
  )


(define (m3-make-intf mkfile-write intf)
  (dis "m3-make-intf : " intf dnl)
  (let ((intf-kind  (car intf))
        (intf-width (cdr intf)))
    (cond ((member intf-kind '(UInt SInt))
           (m3-write-int-intfs mkfile-write intf-kind intf-width))

          (else 'skip)
          )
    )
  )

(define (build-dir) "build/src/")

(define (m3-write-int-intfs mkfile-write intf-kind intf-width)
  (let* ((inm    (sa (symbol->string intf-kind) (number->string intf-width)))
         (intf   (cons intf-kind intf-width))
         (iwr    (FileWr.Open (sa (build-dir) inm ".i3")))
         (mwr    (FileWr.Open (sa (build-dir) inm ".m3")))
         (type   (int-intf->type intf))
         (native (m3-natively-representable-type? type))
         )
    
    (define (iw . x) (Wr.PutText iwr (apply string-append x)))
    (define (mw . x) (Wr.PutText mwr (apply string-append x)))

    (mkfile-write "Smodule     (\"" inm "\")" dnl
                  "Channel     (\"" inm "\" , \"" inm "\")" dnl
                  "Node        (\"" inm "\" , \"" inm "\")" dnl
                  "SchemeStubs (\"" inm "Chan\")" dnl
                  (if native
                      "NarrowIntOps"
                      "WideIntOps")  " (\"" inm "Ops\",\"" inm "\")" dnl
                  "SchemeStubs (\"" inm "Ops\")" dnl
                       
                  )
    
    (iw "INTERFACE " inm ";" dnl
        "IMPORT Mpz;" dnl
        "<*NOWARN*>IMPORT Word;" dnl
       dnl)

    (iw "CONST Width    = " intf-width ";" dnl)
    (iw "CONST Signed   = " (Fmt.Bool (eq? intf-kind 'SInt)) ";" dnl)
    (iw "CONST Mask     = Word.Minus(Word.Shift(1, Width), 1);" dnl)
    (iw "CONST NotMask  = Word.Not(Mask);" dnl)
    (iw "VAR(*CONST*)   Min, Max : Mpz.T;" dnl)
    
    (if native
        (let* ((range  (m3-get-intf-range intf))
               (lo-txt (BigInt.FormatLiteral (car range)  16))
               (hi-txt (BigInt.FormatLiteral (cadr range) 16))
               ) 
          (iw
           "TYPE T = [ " lo-txt " .. " hi-txt " ];" dnl
           dnl
           "CONST Wide     = FALSE;" dnl
           dnl
           "CONST Brand = \"" inm "\";" dnl
           dnl
           )
        )

        (iw
         dnl
         "TYPE T = ARRAY [ 0 .. " (ceiling (/ intf-width *target-word-size*) ) "-1 ] OF Word.T;" dnl
         dnl
         "CONST Wide     = TRUE;" dnl
         dnl
         "CONST Brand = \"" inm "\";" dnl
         dnl
        )
        )

    (iw dnl
        "END " inm "." dnl)
    (mw "MODULE " inm ";" dnl
        dnl
        "IMPORT Mpz;" dnl
        dnl)
        
    (mw dnl
        "BEGIN" dnl
        )
    
        (let* ((range  (m3-get-intf-range intf))
               (lo-txt (BigInt.FormatLiteral (car range)  16))
               (hi-txt (BigInt.FormatLiteral (cadr range) 16))
               ) 
          (if native
              (begin
                (mw "  Min := Mpz.New();" dnl)
                (mw "  Max := Mpz.New();" dnl)
                (mw "  Mpz.init_set_si(Min, " lo-txt ");" dnl)
                (mw "  Mpz.init_set_si(Max, " hi-txt ");" dnl)
                )
              (begin
                (mw "  Min := Mpz.New();" dnl)
                (mw "  Max := Mpz.New();" dnl)
                (mw "  EVAL Mpz.init_set_str(Min, \"" lo-txt "\", 16);" dnl)
                (mw "  EVAL Mpz.init_set_str(Max, \"" hi-txt "\", 16);" dnl)
                )
              )
          )
        
    (mw "END " inm "." dnl)
    (Wr.Close iwr)
    (Wr.Close mwr)
    )
  )

(define (m3-get-intf-range intf)
  (get-type-range (int-intf->type intf))
  )

(define (int-intf? intf)
  (member (car intf) '(UInt SInt)))

(define (int-intf->type intf)
  (case (car intf)
    ((UInt) (make-integer-type #f (cdr intf)))
    ((SInt) (make-integer-type #t (cdr intf)))
    (else (error))))

(define (m3-make-scope-map the-blocks the-decls cell-info)
  ;; a bound identifier can have four types of scope:
  ;; 1. block local
  ;; 2. process local
  ;; 3. parallel-loop dummy
  ;; 4. a port reference

  (define tbl (make-hash-table 100 atom-hash))

  (define (make-add! tag) (lambda(id)(tbl 'add-entry! id tag)))

  (map (make-add! 'block)
       (apply append (map get-loop-dummies the-blocks)))
  
  (map (make-add! 'port)
       (map get-port-id (get-ports cell-info)))

  (map (make-add! 'process)
       (m3-frame-variables the-blocks the-decls cell-info))

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

;;(define (make-decls) (gen-decls text9 *proposed-types* ))

(define (truncate-file fn) (Wr.Close (FileWr.Open fn)))

(define (append-text fn . text)
  (let ((wr (FileWr.OpenAppend fn)))
    (map (lambda(txt)(display txt wr)) text)
    (Wr.Close wr)
    )
  'ok
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The PROCESS CONTEXT pc contains five things:
;; car     : a symtab built from the-decls
;; cadr    : the cell-info
;; caddr   : the struct-tbl
;; cadddr  : scopes for each variable
;; caddddr : the port table
;;

(define (make-proc-context
         symtab cell-info struct-tbl scopes port-tbl constants arrays)
  (list          symtab cell-info struct-tbl scopes port-tbl constants arrays))

(define pc-symtab      car)
(define pc-cell-info   cadr)
(define pc-struct-tbl  caddr)
(define pc-scopes      cadddr)
(define pc-port-tbl    caddddr)
(define pc-constants   cadddddr)
(define pc-arrays      caddddddr)

(define (write-m3overrides!)
  (let ((wr (FileWr.Open (sa (build-dir) "m3overrides"))))
    (Wr.PutText wr (sa "include(\"" *m3utils*"/m3overrides\")" dnl))
    (Wr.Close wr)
    )
  )

(define *m3-standard-interfaces*
  '("libm3"
    "cit_util"
    "parseparams"
    "simpletoken"
    "mscheme"
    "modula3scheme"
    "sstubgen"
    "parseparams"
    "cspsimlib"
    )
  )

(define (write-m3makefile-header!)
  (let ((wr (FileWr.Open (sa (build-dir) "m3makefile"))))

    (map (curry Wr.PutText wr)
         (map (lambda(intf)(sa "import(\"" intf "\")" dnl))
              *m3-standard-interfaces*))

    (Wr.PutChar wr dnl)
    
    (Wr.Close wr)
    )
  )

(define (write-m3makefile-footer!)
  (let ((wr (FileWr.OpenAppend (sa (build-dir) "m3makefile"))))
    (Wr.PutChar wr dnl)
    (Wr.PutText wr (sa "ExportSchemeStubs (\"sim\")" dnl))
    (Wr.PutText wr (sa "importSchemeStubs ()" dnl))
    (Wr.PutText wr (sa "implementation (\"SimMain\")" dnl))
    (Wr.PutText wr (sa "program (\"sim\")" dnl))
    (Wr.Close wr)
    )
  )


(define (node->uint-intf intf)
  (if (eq? 'Node (car intf))
      (cons 'UInt (cdr intf))
      intf)
  )

(define (ops-intfs intfs)
  (filter identity
          (map
           (lambda(intf)
             (if (member (car intf) '(SInt UInt))
                 (cons (symbol-append (car intf) 'Ops) (cdr intf))
                 #f))
           intfs)
          )
  )

(define (do-m3!)
  (let* ((the-blocks text9)
         (cell-info  *cellinfo*)
         (port-tbl   *the-prt-tbl*)
         (the-decls  (gen-decls the-blocks *proposed-types*))
         (root       (m3-ident (string->symbol *the-proc-type-name*)))
         (i3fn       (string-append (build-dir) root ".i3"))
         (i3wr       (FileWr.Open i3fn))
         (m3fn       (string-append (build-dir) root ".m3"))
         (m3wr       (FileWr.Open m3fn))
         (ipfn       (string-append (build-dir) root ".imports"))
         (ipwr       (FileWr.Open ipfn))
         (fork-counts (get-fork-label-counts the-blocks))
         (the-scopes (m3-make-scope-map the-blocks the-decls cell-info))

         (m3-port-data-intfs
          (uniq equal?
                (map m3-convert-port-type-build
                     (map get-port-def-type
                          (port-tbl 'values)))))

         (m3-port-chan-intfs
          (set-union
;;           (map node->uint-intf
           (filter (lambda(pt)(eq? 'Node (car pt))) m3-port-data-intfs)
           ;;)
           
           (map (lambda(pt)(cons (symbol-append (car pt) 'Chan)
                                 (cdr pt)
                                 ))
                
                (filter (lambda(pt)(not (eq? 'Node (car pt))))
                        m3-port-data-intfs))))

         (m3-var-intfs
          (filter
           identity
           (uniq equal?
                 (map m3-map-declbuild
                      (map get-decl1-type
                           (map get-var1-decl1 the-decls))))))

         (all-intfs0 (set-union m3-port-data-intfs
                                m3-var-intfs
                                m3-port-chan-intfs))
         (all-intfs  (set-union all-intfs0 (ops-intfs all-intfs0)))

         ;; remove var1s and clean  up blocks
         (the-varfree-blocks (map filter-out-var1s the-blocks))
         (the-exec-blocks (remove-empty-blocks the-varfree-blocks))
         )

    (set! text10 the-exec-blocks)

    (dis "do-m3! : m3-port-data-intfs : " m3-port-data-intfs dnl)
    (dis "do-m3! : m3-port-chan-intfs : " m3-port-chan-intfs dnl)
    (dis "do-m3! : m3-var-intfs       : " m3-var-intfs dnl)
    (dis "do-m3! : all-intfs          : " all-intfs dnl)
    
;;    (write-m3overrides!)
;;    (write-m3makefile-header!)

    ;; prepare needed custom interfaces

    (set! *ai* all-intfs)

    (let ((bld-intfs (uniq equal? (map node->uint-intf all-intfs))))
      (dis "tobuild : " bld-intfs dnl)
      (Wr.PutText ipwr (stringify bld-intfs))
;;      (map (curry m3-make-intf mkfile-write) bld-intfs)
      )


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (intf . x) (Wr.PutText i3wr (apply string-append x)))
    (define (modu . x) (Wr.PutText m3wr (apply string-append x)))
    (define (impo . x) (Wr.PutText ipwr (apply string-append x)))
    ;; interface file
    
    (intf "INTERFACE " root ";" dnl dnl)
    (intf "(*" dnl
          "FINGERPRINT " (fingerprint-string (stringify *cell*))  dnl
          "*)" dnl
          dnl
          )

    (m3-write-imports intf m3-port-chan-intfs)

    (m3-write-build-decl intf cell-info)

    (intf dnl "END " root "." dnl)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (modu "MODULE " root ";" dnl dnl)
    (modu "(*" dnl
          "FINGERPRINT " (fingerprint-string (stringify *cell*))  dnl
          "*)" dnl
          dnl
          )

    (m3-write-imports    modu all-intfs)
    (modu "IMPORT Mpz;" dnl
          dnl)
    (m3-write-proc-frame-decl modu
                              port-tbl the-exec-blocks cell-info the-decls
                              fork-counts)
    
    (m3-write-block-decl      modu)
    (m3-write-closure-decl    modu)
    (m3-write-build-defn      modu
                              cell-info the-exec-blocks the-decls fork-counts
                              *the-arr-tbl*)

    
    (let ((pc (make-proc-context
                         (m3-make-symtab the-decls *the-arr-tbl*)
                         cell-info
                         *the-struct-tbl*
                         the-scopes
                         (make-port-table cell-info)
                         (make-hash-table 100 Text.Hash)
                         *the-arr-tbl*
                         )))
      (set! *proc-context* pc)
      (m3-write-blocks          modu the-exec-blocks pc)

      (map modu
           (map m3-gen-constant-init 
                ((pc-constants pc) 'keys)
                ((pc-constants pc) 'values)
                )
           )
    );;tel
    
    (modu dnl "BEGIN END " root "." dnl)
    
    (Wr.Close i3wr)
    (Wr.Close m3wr)
    (Wr.Close ipwr)
    
    );;*tel
  
    
;;  (write-m3makefile-footer!)
  )

(define (m3-gen-constant-init m3-ident bigint)
  (sa "VAR "
      m3-ident " := Mpz.InitScan(\"" (BigInt.Format bigint 16) "\", 16);" dnl)
  )

(define (m3-make-symtab the-decls the-arrays)
  ;; at this point, the-decls contains declarations that are stripped
  ;; of their arrays, so we rebuild them from the array-decls
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


;; the below is unused
(define (m3-write-node-intf mkfile-write intf-width)
  (let* ((inm (sa "Node" (number->string intf-width)))
         (iwr (FileWr.Open (sa (build-dir) inm ".i3"))))
    
    (define (w . x) (Wr.PutText iwr (apply string-append x)))

    (mkfile-write "Smodule (\"" inm "\")" dnl)
    
    (w "INTERFACE " inm ";" dnl)
    (w dnl
       "TYPE T = RECORD END;" dnl ;; what do we want here?
       dnl)
    (w "END " inm "." dnl)
    (Wr.Close iwr)
    )
  )

(define (find-filepaths dir pat)
  ;; find files in the directory matching the regex pattern pat
  ;; return them with the full relative path
  (let ((files (FileFinder.Find dir pat)))
    (map (curry string-append dir) files)
    )
  )

(define (m3-clear-build-area!)
  (define (clear)
    (map FS.DeleteFile (find-filepaths (build-dir) "^[^.]")))
  (define attempt
    (unwind-protect
     (clear)
     (lambda()'ok)
     (begin
       (dis "m3-clear-build-area! : warning : couldn't delete all files" dnl)
       (lambda()'fail)
       )
     )
    )
  (attempt)
  )

(define *imports-extension* "imports")

(define (derive-mod-name imp-fn)
  (let* ((len       (Text.Length imp-fn))
         (pfx-len   (Text.Length (build-dir)))
         (sfx-len   (+ 1 (Text.Length *imports-extension*))))
    (Text.Sub imp-fn pfx-len (- len pfx-len sfx-len))
    )
  )

(define (m3-write-makefile!)

  (define (mkfile-write . text)
    (apply (curry append-text (sa (build-dir) "/m3makefile")) text)
    )
  
  ;; scan build directory for .imports files
  ;; write the m3makefile
  (let* ((im-files  (find-filepaths
                     (build-dir)
                     (sa "\\." *imports-extension* "$")))
         ;; the imports files
         
         (mod-nams  (map derive-mod-name im-files))
         ;; derive module names from the imports files
         
         (im-data   (map read-importlist im-files))
         ;; load the actual imports we need to build
         
         (all-ims   (uniq equal? (apply append im-data)))
         ;; and this is the cleaned up list of import interfaces to build
         
         )
    
    (dis "m3-write-makefile : im-files : " (stringify im-files) dnl)
    (dis "m3-write-makefile : all-ims  : " all-ims dnl)

    ;; write m3overrides
    (write-m3overrides!)

    ;; and now the m3makefile
    (write-m3makefile-header!)

    ;; make code for the requested types, as well as the m3makefile entries
    (map (curry m3-make-intf mkfile-write) all-ims) 

    ;; record the modules we need to compile
    (map (lambda(root)
           (mkfile-write "Smodule (\"" root "\")" dnl))
         mod-nams)

    ;; close out the m3makefile
    (write-m3makefile-footer!)
    )

  'ok
  )

(define (do-compile-m3! nm . x)
  (let* ((modname   (loaddata0! nm))
         (loaded-fp (fingerprint-string (stringify *cell*)))
         (old-fp    (FingerprintFinder.Find (sa (build-dir) "/" modname ".m3")))
         )

    (cond ((and (not (null? old-fp))
                (equal? loaded-fp old-fp)
                (or (null? x) (not eq? 'force (car x)))
                )
           (dis go-grn-bold-term
                "=========  ALREADY UP-TO-DATE : " modname " , SKIPPING"
                reset-term
                dnl
                dnl)
           'skip)

          (else
           (loaddata1!)
           (compile!)
           (do-m3!)
           'ok)
           
          );;dnoc
    );;*tel
  )

(define (compile-csp! . x)
  ;;(m3-clear-build-area!)
  (map do-compile-m3! x)
  (m3-write-makefile!)
  (done-banner)
  'ok
  )

(define (test!)
  (reload)

  (compile-csp! "tests/first_proc_false.scm" "tests/first_proc_true.scm")

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the driver
;;

(define (drive! fn . x)

  (if (and (not (null? x)) (eq? 'force (car x)))
      (m3-clear-build-area!))
  
  (define the-driver (obj-method-wrap
                      (new-modula-object 'CspCompilerDriver.T)
                      'CspCompilerDriver.T))

  (the-driver 'init fn)

  (let* ((proc-type-seq (obj-method-wrap (the-driver 'getProcTypes)
                                         'TextSeq.T))
         (the-modules   (count-execute
                         (proc-type-seq 'size)
                         (curry proc-type-seq 'get)))
         (the-scms      (map (lambda(m)(sa m ".scm")) the-modules))
         (the-port-tbl  (m3-make-module-intf-tbl the-modules))
         );;*tel
         

    (dis "the-modules  : " (stringify the-modules) dnl)
    (dis "the-port-tbl : " (stringify the-port-tbl) dnl)

    (apply compile-csp! the-scms)
    
    (the-driver 'setProcessPorts (the-port-tbl '*m3*))

    (m3-write-main! (the-driver 'genBuilder "BuildSimulation"))
    
    'ok
    )

  )

;;(drive! "demo_46_SYSTEM.procs")

(define (m3-write-main! builder-text)

  (let ((mwr (FileWr.Open (sa (build-dir) "SimMain.m3"))))

    (define (mw . x) (Wr.PutText mwr (apply string-append x)))

    (mw "MODULE SimMain EXPORTS Main;" dnl)
    (mw "IMPORT CspCompiledScheduler AS Scheduler;" dnl)

    (mw dnl)

    (mw builder-text)

    (mw dnl)


    (mw "BEGIN BuildSimulation() ; Scheduler.SchedulingLoop() END SimMain." dnl)

    (Wr.Close mwr)
    )

 'ok
  )

(define (get-module-cellinfo mod-name)
  (caddr (read-importlist (sa mod-name ".scm"))))

(define (get-module-ports mod-name)
  (caddddr (get-module-cellinfo mod-name)))

(define ppp #f)

(define (m3-make-csp-port pdef)
  (set! ppp pdef)
  (let ((ptype (cadddr pdef)))
    (CspPort.New (get-port-id pdef)                       ;; name
                 
                 (convert-dir (port-direction pdef))      ;; direction
                 
                 (cond ((node-port?    pdef) 'Node)       ;; class
                       ((channel-port? pdef) 'Channel)
                       (else (error "m3-make-csp-port : " pdef)))
                 
                 (port-type-width ptype)                  ;; width (in bits)

                 (if (channel-port? pdef)                 ;; type-name
                     (cadr ptype)
                     'node)

                 )
    )
  )

(define (m3-get-module-ports mod-name)
  (let ((seq (init-seq 'CspPortSeq.T)))
    (map (curry seq 'addhi)
         (map m3-make-csp-port (get-module-ports mod-name)))
    seq
    )
  )

(define (m3-make-module-intf-tbl mod-lst)
  (let ((the-port-tbl
         (obj-method-wrap (new-modula-object 'TextCspPortSeqTbl.Default)
                           'TextCspPortSeqTbl.Default))

        (the-port-seqs
         (map (lambda(x)(x '*m3*)) (map m3-get-module-ports mod-lst)))
        )
    (the-port-tbl 'init 100)
    
    (dis "m3-make-module-intf-tbl : mod-lst       : " mod-lst dnl)
    (dis "m3-make-module-intf-tbl : the-port-seqs : " the-port-seqs dnl)
    (map (curry the-port-tbl 'put) mod-lst the-port-seqs)

    the-port-tbl
  )
)
  
(define (fingerprint-string str)
  (let ((fp (Fingerprint.FromText str)))
    (number->string
     (apply +
            (map (lambda(x)(* (Math.pow 256 (car x)) (cdr x))) (cdar fp))))))
 

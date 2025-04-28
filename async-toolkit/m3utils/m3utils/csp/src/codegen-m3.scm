
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
                  
(define (m3-map-decltype type)
  ;; returns interface
  (cond ((string-type?  type) "CspString")
        ((boolean-type? type) "CspBoolean")
        ((array-type?   type) (error "not done") )
        ((struct-type?  type) (error "not done") )
        ((integer-type? type)
         (let ((width (cadddr type)))
           (cond ((not (bigint? width)) "DynamicInt")
                 ((caddr type)
                  (string-append "SInt" (BigInt.Format width 10)))
                 (else
                  (string-append "UInt" (BigInt.Format width 10)))
                 );;dnoc
           );;tel
         )
        (else (error "m3-map-decltype : unknown type to map : " type))
        );;dnoc
  )

(define (m3-map-declbuild type)
  ;; returns interface to build
  (cond ((string-type?  type) #f)
        ((boolean-type? type) #f)
        ((array-type?   type) (error "not done") )
        ((struct-type?  type) (error "not done") )
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
        (else (error "m3-map-decltype : unknown type to map : " type))
        );;dnoc
  )

(define (m3-convert-vardecl v1)
  (let ((id (get-var1-id v1))
        (ty (get-decl1-type (get-var1-decl1 v1)))
        )
    (string-append (pad 40 (m3-ident id)) " : " (m3-map-decltype ty) ".T;")
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


(define (m3-write-build-defn w cell-info the-blocks fork-counts)
  (let ((proc-ports (get-ports cell-info)))
    (m3-write-build-signature w cell-info)
    (w " = " dnl)
    (w "  BEGIN" dnl)
    (w "    WITH frame = NEW(Frame," dnl
       "                     name := name," dnl
       "                     id := Process.NextFrameId()" dnl)

    (let ((asslist (map m3-format-port-ass proc-ports)))
      (map (lambda(ass)
             (w "                     ," ass  dnl))
           asslist)
      );;tel
    
    (w "      ) DO" dnl)

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

;; note that the block closures need to be extended!
;; some block closures will have pointers to parallel-loop indices

(define (m3-write-text w the-blocks cell-info)

  )

(define (m3-write-imports w intfs)
  (dis "m3-write-imports : intfs : " intfs dnl)
  
  (w "<*NOWARN*>IMPORT CspCompiledProcess AS Process;" dnl)
  (w "<*NOWARN*>IMPORT CspCompiledScheduler AS Scheduler;" dnl)
  (w "<*NOWARN*>IMPORT CspString, Fmt;" dnl)
  (w "<*NOWARN*>IMPORT CspIntrinsics;" dnl)
  (map (lambda(intf)(w "IMPORT " intf ";" dnl))
       (map format-intf-name intfs))
  )

(define *map-chantypes* ;; these are types generated by generics..
  '((UIntChan UInt Chan)
    (SIntChan SInt Chan)))

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
         (rhs (get-assign-rhs x))
         (comp-lhs 
          (sa (m3-format-designator pc lhs) " := ")))
    
    (cond ((ident? rhs)
           (sa comp-lhs (force-type pc 'native rhs)))
        
          ((bigint? rhs)
           (sa comp-lhs (BigInt.Format rhs 10)))
              
          ((binary-expr? rhs)
           (sa comp-lhs
               (m3-compile-typed-binop pc
                                       'native
                                       (car rhs)
                                       (cadr rhs)
                                       (caddr rhs))))
              
          ((unary-expr? rhs)
           (sa comp-lhs
               (m3-compile-native-unop pc rhs)))
              
          ((bits? rhs)
           (sa comp-lhs
               (m3-compile-native-bits pc rhs)))

          ((recv-expression? rhs)
           (m3-compile-recv pc `(recv ,(cadr rhs) ,lhs)))
              
          (else (error "m3-compile-native-int-assign"))
          );;dnoc
    );;*tel
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
        copy-type ".T := " (force-type pc port-class rhs)
        "; BEGIN IF NOT "
        port-typenam ".Send( " m3-pname "^ , toSend , cl ) THEN RETURN FALSE END END"
        )
    )
  )

(define (m3-compile-recv pc stmt)
  (dis "m3-compile-recv : " stmt dnl)
  (let* (
         (port-des     (get-recv-lhs stmt)) ;; doesnt work for arrays/structs
         (rhs          (get-recv-rhs stmt))

         
         (port-tbl     (caddddr pc))
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
          copy-type".T; BEGIN IF NOT "
          port-typenam ".Recv( " m3-pname "^ , toRecv , cl ) THEN RETURN FALSE END END")
      )

    (define (nonnull-rhs)
      (let ((rhs-class    (classify-type pc rhs)))
        (sa "VAR toRecv : "
            copy-type".T; BEGIN IF NOT "
            port-typenam ".Recv( " m3-pname "^ , toRecv , cl ) THEN RETURN FALSE END;"
            (m3-format-designator pc rhs) " := "
            (m3-compile-convert-type
             port-class
             rhs-class
             "toRecv"
             )
            " END"
            )
        )
      )
    

    (if (null? rhs) (null-rhs) (nonnull-rhs))
    )
  )

(define (m3-compile-eval pc stmt)
  (let ((expr (cadr stmt)))
    (string-append "EVAL " (m3-compile-intrinsic pc expr))
    )
  )

(define (m3-compile-intrinsic pc expr)
  (if (not (call-intrinsic? expr)) (error "not an intrinsic : " expr))

  (sa "CspIntrinsics." (symbol->string (cadr expr)) "(frame, "
      
      (m3-format-varid pc (cadaddr expr))
                       ")")

  )

(define *known-stmt-types* '(recv send assign eval goto local-if while))

(define (m3-space-comments txt)
  (CitTextUtils.Replace
   (CitTextUtils.Replace txt "(*" "( *")
   "*)" "* )"))

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
  (dis (get-block-label blk) dnl)
  (dis "m3-write-block : " blk dnl)
  (let* ((btag        m3-label)
         (bnam        (string-append "Block_" btag))
         (the-code    (cddr (filter-out-var1s blk)))

         (symtab      (car pc))
         (the-scopes  (cadddr pc))
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
     "    END(*WITH*)" dnl
     "  END " bnam ";" dnl
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
                  )
    
    (iw "INTERFACE " inm ";" dnl
       dnl)

    (if native
        (let* ((range (m3-get-intf-range intf))
               (lo-txt (BigInt.FormatLiteral (car range)  16))
               (hi-txt (BigInt.FormatLiteral (cadr range) 16))
               ) 
          (iw
           "TYPE T = [ " lo-txt " .. " hi-txt " ];" dnl
           dnl
           "CONST MustCopy = FALSE;" dnl
           dnl
           "PROCEDURE Copy(READONLY from : T; VAR to : T);" dnl
           dnl
           "CONST Brand = \"" inm "\";" dnl
           dnl
           )
        )

        (iw
         "IMPORT Word;" dnl
         dnl
         "TYPE T = ARRAY [ 0 .. " (ceiling (/ width 64) ) "-1 ] OF Word.T;" dnl
         dnl
         "CONST MustCopy = FALSE;" dnl
         dnl
         "PROCEDURE Copy(READONLY from : T; VAR to : T);" dnl
         dnl
         "CONST Brand = \"" inm "\";" dnl
         dnl
        )
        )

    (mw "MODULE " inm ";" dnl
        dnl
        "PROCEDURE Copy(READONLY from : T; VAR to : T) =" dnl
        "  BEGIN" dnl
        "    to := from" dnl
        "  END Copy;" dnl
        dnl
        )

    
    (iw dnl
       "END " inm "." dnl)
    (mw dnl
       "BEGIN END " inm "." dnl)
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

(define (make-decls) (gen-decls text9 *proposed-types*))

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
         (the-scopes (m3-make-scope-map the-blocks cell-info))

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

         (all-intfs (set-union m3-port-data-intfs
                               m3-var-intfs
                               m3-port-chan-intfs))

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
    (m3-write-proc-frame-decl modu port-tbl the-exec-blocks cell-info the-decls
                              fork-counts)
    (m3-write-block-decl      modu)
    (m3-write-closure-decl    modu)
    (m3-write-build-defn      modu cell-info the-exec-blocks fork-counts)

    
    (let ((pc (list
                         (m3-make-symtab the-decls)
                         cell-info
                         *the-struct-tbl*
                         the-scopes
                         (make-port-table cell-info))))
      (set! *proc-context* pc)
      (m3-write-blocks          modu the-exec-blocks pc)
      )
    
    (modu dnl "BEGIN END " root "." dnl)
    
    (Wr.Close i3wr)
    (Wr.Close m3wr)
    (Wr.Close ipwr)
    
    );;*tel
  
    
;;  (write-m3makefile-footer!)
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
    (number->string (apply + (map (lambda(x)(* (Math.pow 256 (car x)) (cdr x))) (cdar fp))))))
 
